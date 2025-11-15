-- src/Client.hs
-- Client chat cho người thật (Human Client)
-- Biên dịch: ghc --make src/Client.hs -threaded -o ChatClient.exe -package network -package stm -package base

import System.IO
import Network.Socket
import Control.Concurrent (forkIO)
import Control.Monad (forever, unless)
import Control.Exception (bracket)

-- ĐỔI TÊN BIẾN Ở ĐÂY CHO KHỎI TRÙNG
serverPort :: String
serverPort = "8080"

main :: IO ()
main = withSocketsDo $ do
    hSetEncoding stdout utf8
    hSetEncoding stdin utf8

    putStrLn "=== HASKELL CHAT CLIENT ==="
    
    -- 1. Nhập IP Server
    putStr "Nhap IP Server: "
    hFlush stdout
    inputIP <- getLine
    let serverIP = if null inputIP then "192.168.191.38" else inputIP

    -- 2. Kết nối 
    runClient serverIP serverPort

runClient :: String -> String -> IO ()
runClient host port = do
    addrInfo <- getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just port)
    let addr = head addrInfo

    -- Mở kết nối Socket
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetEncoding handle utf8

    putStrLn $ "Da ket noi den " ++ host ++ ":" ++ port
    putStrLn "Nhap lenh: LOGIN <ten_ban>"

    -- 3. Tạo luồng lắng nghe (Nghe tin nhắn từ Server trả về)
    forkIO $ listenToServer handle

    -- 4. Luồng chính: Đọc bàn phím và gửi đi
    sendToServer handle
    
    hClose handle

-- Hàm lắng nghe tin nhắn từ Server
listenToServer :: Handle -> IO ()
listenToServer h = forever $ do
    eof <- hIsEOF h
    if eof then return () else do
        msg <- hGetLine h
        putStrLn msg -- In tin nhắn ra màn hình

-- Hàm gửi tin nhắn từ bàn phím
sendToServer :: Handle -> IO ()
sendToServer h = do
    line <- getLine
    unless (line == "QUIT") $ do
        hPutStrLn h line -- Gửi qua mạng
        sendToServer h   -- Lặp lại