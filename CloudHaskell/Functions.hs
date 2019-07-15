{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)


data Type = Int | String

data ST = Send (SendPort Int) Int ST | Recv (ReceivePort Int) ST | End

triple :: ST -> Process ()
triple (Send sp v st) = do
  sendChan sp v
  say $ "Sent value: " ++ show v
  triple st

triple (Recv rp st) = do
  v <- receiveChan rp
  say $ "Received value: " ++ show v
  triple st

triple (End) = do
  say $ "End"


main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8082"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable
  
  runProcess node $ do
    (s1,r1) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (s2,r2) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (s3,r3) <- newChan :: Process (SendPort Int, ReceivePort Int)

    pid1 <- spawnLocal $ do
      triple (Send s1 3 $ Send s2 5 $ Recv r3 $ End)
      say $ "end1"
      terminate

    pid2 <- spawnLocal $ do
      triple (Recv r1 $ Recv r2 $ Send s3 6 $ End)
      say $ "end2"
      terminate
    
    
    _ <- receiveChan r1
    say $ show pid2

  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
