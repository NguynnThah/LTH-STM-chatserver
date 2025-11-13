{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, unless, void)
import Control.Exception (finally)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO
import Network.Socket

-- ==========================================
-- PHẦN 1: CORE LOGIC STM (GIỮ NGUYÊN & NÂNG CẤP)
-- ==========================================

type ClientId = Int
type RoomName = Text

-- NÂNG CẤP: Thêm 'clientHandle' để server biết gửi tin nhắn về đâu
data Client = Client
    { clientId :: ClientId
    , clientName :: Text
    , clientHandle :: Handle 
    }

data Server = Server
    { nextClientId :: TVar ClientId
    , clients :: TVar (Map ClientId Client)
    , rooms :: TVar (Map RoomName (TVar [ClientId]))
    }

newServer :: IO Server
newServer = atomically $ do
    nc <- newTVar 0
    cs <- newTVar Map.empty
    rs <- newTVar Map.empty
    return $ Server nc cs rs

-- NÂNG CẤP: Hàm đăng ký nhận thêm Handle kết nối
registerClient :: Server -> Text -> Handle -> IO Client
registerClient srv name handle = atomically $ do
    cid <- readTVar (nextClientId srv)
    writeTVar (nextClientId srv) (cid + 1)
    -- Không cần TChan nữa vì ta sẽ gửi trực tiếp qua Handle
    let client = Client cid name handle
    modifyTVar' (clients srv) (Map.insert cid client)
    return client

unregisterClient :: Server -> ClientId -> IO ()
unregisterClient srv cid = atomically $ do
    modifyTVar' (clients srv) (Map.delete cid)
    rmap <- readTVar (rooms srv)
    let removeCid tv = do
          lst <- readTVar tv
          writeTVar tv (filter (/= cid) lst)
    mapM_ removeCid (Map.elems rmap)

joinRoom :: Server -> RoomName -> ClientId -> IO ()
joinRoom srv room cid = atomically $ do
    rmap <- readTVar (rooms srv)
    tv <- case Map.lookup room rmap of
        Just t -> return t
        Nothing -> do
            t <- newTVar []
            writeTVar (rooms srv) (Map.insert room t rmap)
            return t
    modifyTVar' tv (\lst -> if cid `elem` lst then lst else cid:lst)

leaveRoom :: Server -> RoomName -> ClientId -> IO ()
leaveRoom srv room cid = atomically $ do
    rmap <- readTVar (rooms srv)
    case Map.lookup room rmap of
        Just tv -> modifyTVar' tv (filter (/= cid))
        Nothing -> return ()

-- NÂNG CẤP: Logic gửi tin nhắn Broadcast ra mạng thật
broadcastToRoom :: Server -> RoomName -> Text -> IO ()
broadcastToRoom srv room msg = do
    -- 1. Dùng STM để lấy danh sách Handle của những người trong phòng
    targetHandles <- atomically $ do
        rmap <- readTVar (rooms srv)
        cmap <- readTVar (clients srv)
        case Map.lookup room rmap of
            Nothing -> return []
            Just tv -> do
                cids <- readTVar tv
                -- Lấy ra danh sách Handle của các client ID đó
                return [ clientHandle c | cid <- cids, Just c <- [Map.lookup cid cmap] ]
    
    -- 2. Thực hiện I/O (Gửi tin) bên ngoài khối atomically
    mapM_ (\h -> hPutStrLn h (T.unpack msg)) targetHandles

-- ==========================================
-- PHẦN 2: NETWORK LAYER (SOCKET SERVER)
-- ==========================================

main :: IO ()
main = withSocketsDo $ do
    hSetEncoding stdout utf8
    srv <- newServer
    putStrLn "=== HASKELL STM CHAT SERVER STARTED ON PORT 8080 ==="
    
    -- 1. Tạo Socket
    addr <- resolve "8080"
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1 -- Cho phép dùng lại cổng ngay lập tức sau khi tắt
    bind sock (addrAddress addr)
    listen sock 10 -- Hàng đợi tối đa 10 kết nối
    
    -- 2. Vòng lặp chấp nhận kết nối (Accept Loop)
    forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Connection from: " ++ show peer
        
        -- Tạo Handle để đọc/ghi dễ dàng hơn
        handle <- socketToHandle conn ReadWriteMode
        hSetBuffering handle LineBuffering
        
        -- 3. Fork một thread mới để xử lý riêng client này (Parallelism)
        forkIO $ handleNewConnection srv handle

-- Hàm phụ trợ để phân giải địa chỉ
resolve :: String -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    head <$> getAddrInfo (Just hints) Nothing (Just port)

-- Xử lý kết nối mới (Chưa đăng nhập)
handleNewConnection :: Server -> Handle -> IO ()
handleNewConnection srv handle = do
    hPutStrLn handle "Welcome to STM Chat! Please type: LOGIN <username>"
    loop
  where
    loop = do
        eof <- hIsEOF handle
        if eof then hClose handle else do
            line <- TIO.hGetLine handle
            let cmd = T.words line
            case cmd of
                ("LOGIN":name:_) -> do
                    -- Đăng ký client vào STM
                    client <- registerClient srv name handle
                    putStrLn $ "User logged in: " ++ T.unpack name
                    hPutStrLn handle $ "Welcome " ++ T.unpack name ++ "!"
                    
                    -- Chuyển sang vòng lặp xử lý lệnh chính
                    -- finally đảm bảo luôn dọn dẹp khi client ngắt kết nối
                    finally (runClientCommands srv client) 
                            (cleanupClient srv client)
                _ -> do
                    hPutStrLn handle "Unknown command. Please use: LOGIN <username>"
                    loop

-- Dọn dẹp khi client rời đi
cleanupClient :: Server -> Client -> IO ()
cleanupClient srv client = do
    putStrLn $ "Client disconnected: " ++ T.unpack (clientName client)
    unregisterClient srv (clientId client)
    hClose (clientHandle client)

-- Vòng lặp xử lý lệnh Chat (JOIN, PART, MSG) - Khớp với StressTest.hs
runClientCommands :: Server -> Client -> IO ()
runClientCommands srv client = forever $ do
    eof <- hIsEOF (clientHandle client)
    if eof then fail "Disconnected" else do
        line <- TIO.hGetLine (clientHandle client)
        let cmd = T.words line
        let cid = clientId client
        let name = clientName client
        
        case cmd of
            ("JOIN":room:_) -> do
                joinRoom srv room cid
                putStrLn $ T.unpack name ++ " joined " ++ T.unpack room
                -- Phản hồi cho client biết (StressTest không in cái này nhưng tốt cho debug)
                -- hPutStrLn (clientHandle client) $ "JOINED " ++ T.unpack room

            ("PART":room:_) -> do
                leaveRoom srv room cid
                putStrLn $ T.unpack name ++ " left " ++ T.unpack room

            ("MSG":room:contentParts) -> do
                let content = T.unwords contentParts
                let fullMsg = T.concat ["[", room, "] ", name, ": ", content]
                
                -- Log ra server để em thấy "ma trận" chữ chạy vù vù
                putStrLn $ "Broadcast " ++ T.unpack room ++ ": " ++ T.unpack content
                
                broadcastToRoom srv room fullMsg

            _ -> return () -- Bỏ qua lệnh lạ