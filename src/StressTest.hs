-- Một client mô phỏng để "tấn công" (stress test) STM Chat Server
-- Biên dịch: ghc --make src/StressTest.hs -threaded -o StressTest.exe -package network -package random -package base

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_, forever)
import System.Random (randomRIO)
import Text.Printf (printf)

----------------------------------------------------
-- CẤU HÌNH TEST
----------------------------------------------------
numClients :: Int
numClients = 50 

testDurationSec :: Int
testDurationSec = 20 

serverHost :: String
serverHost = "127.0.0.1"

serverPort :: String
serverPort = "8080" 

chaosRoom :: String
chaosRoom = "#LTH-ChatServer-STM" 

----------------------------------------------------
-- HÀM MAIN
----------------------------------------------------
main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8 
  printf "--- Bat dau kich ban 'Hon loan' ---\n"
  printf "--- Tao %d client ao... \n" numClients
  
  replicateM_ numClients $ do
    clientId <- randomRIO (1, 10000) :: IO Int
    forkIO (runClient clientId)
    threadDelay 50000 

  printf "--- %d client dang tan cong server trong %d giay... ---\n" numClients testDurationSec
  threadDelay (testDurationSec * 1000000)
  
  printf "--- Kich ban 'Hon loan' ket thuc. ---\n"

----------------------------------------------------
-- HÀM CLIENT CHÍNH
----------------------------------------------------
runClient :: Int -> IO ()
runClient clientId = do
  (handle, username) <- connectToServer ("user" ++ show clientId)
  
  hPutStrLn handle $ "LOGIN " ++ username
  hPutStrLn handle $ "JOIN " ++ chaosRoom
  hFlush handle
  
  -- SỬA TÊN HÀM Ở ĐÂY
  forkIO $ listenToServer handle
  
  chaosLoop handle username

-- SỬA TÊN HÀM Ở ĐÂY (Đổi từ listen thành listenToServer)
listenToServer :: Handle -> IO ()
listenToServer handle = forever $ do
  _ <- hGetLine handle 
  return ()

chaosLoop :: Handle -> String -> IO ()
chaosLoop handle username = forever $ do
  actionCode <- randomRIO (1, 10) :: IO Int
  
  let actionMessage = case actionCode of
        1 -> Just $ "PART " ++ chaosRoom
        2 -> Just $ "PART " ++ chaosRoom
        3 -> Just $ "PART " ++ chaosRoom
        4 -> Just $ "PART " ++ chaosRoom
        
        5 -> Just $ "JOIN " ++ chaosRoom
        6 -> Just $ "JOIN " ++ chaosRoom
        7 -> Just $ "JOIN " ++ chaosRoom
        8 -> Just $ "JOIN " ++ chaosRoom
        
        _ -> Just $ "MSG " ++ chaosRoom ++ " " ++ username ++ " dang spam"
  
  case actionMessage of
    Just msg -> do
      hPutStrLn handle msg
      hFlush handle
    Nothing -> return ()
  
  delay <- randomRIO (500000, 2000000) 
  threadDelay delay

----------------------------------------------------
-- HÀM HELPER
----------------------------------------------------
connectToServer :: String -> IO (Handle, String)
connectToServer username = do
  addr <- head <$> getAddrInfo (Just defaultHints) (Just serverHost) (Just serverPort)
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering 
  return (handle, username)