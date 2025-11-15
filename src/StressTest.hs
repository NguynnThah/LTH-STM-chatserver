-- StressTest.hs 
-- Dùng để demo giao diện Python chạy vù vù
-- Compile: ghc --make src/StressTest.hs -threaded -o StressTest.exe -package network -package random -package base

import Network.Socket
import System.IO
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_, forever)
import System.Random (randomRIO)
import Text.Printf (printf)

----------------------------------------------------
-- CẤU HÌNH
----------------------------------------------------
numClients :: Int
numClients = 50 

testDurationSec :: Int
testDurationSec = 30 

serverHost :: String
serverHost = "192.168.191.38"     

serverPort :: String
serverPort = "8080" 

chaosRoom :: String
chaosRoom = "#chaos" 

----------------------------------------------------
-- MAIN
----------------------------------------------------
main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8 -- Sửa lỗi font tiếng Việt trên Windows
  
  printf "--- Bat dau che do SPAM demo ---\n"
  printf "--- Tao %d client ao... \n" numClients
  
  replicateM_ numClients $ do
    clientId <- randomRIO (1, 10000) :: IO Int
    forkIO (runClient clientId)
    threadDelay 50000 

  printf "--- %d client dang tan cong server trong %d giay... ---\n" numClients testDurationSec
  threadDelay (testDurationSec * 1000000)
  
  printf "--- Ket thuc kich ban. ---\n"

----------------------------------------------------
-- CLIENT LOGIC
----------------------------------------------------
runClient :: Int -> IO ()
runClient clientId = do
  (handle, username) <- connectToServer ("bot" ++ show clientId)
  
  hPutStrLn handle $ "LOGIN " ++ username
  hPutStrLn handle $ "JOIN " ++ chaosRoom
  hFlush handle
  
  -- Lắng nghe 
  forkIO $ listenToServer handle
  
  -- Bắt đầu Spam
  spamLoop handle username

-- Hàm lắng nghe đơn giản
listenToServer :: Handle -> IO ()
listenToServer handle = forever $ do
  _ <- hGetLine handle 
  return ()

-- VÒNG LẶP SPAM 
spamLoop :: Handle -> String -> IO ()
spamLoop handle username = forever $ do
  let msg = "MSG " ++ chaosRoom ++ " " ++ username ++ " : spam tin nhan demo GUI!!!"
  
  hPutStrLn handle msg
  hFlush handle
  
  -- Tốc độ spam: 0.2 giây / 1 tin 
  threadDelay 200000

----------------------------------------------------
-- KẾT NỐI
----------------------------------------------------
connectToServer :: String -> IO (Handle, String)
connectToServer username = do
  addr <- head <$> getAddrInfo (Just defaultHints) (Just serverHost) (Just serverPort)
  sock <- socket (addrFamily addr) Stream defaultProtocol
  connect sock (addrAddress addr)
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle LineBuffering 
  return (handle, username)