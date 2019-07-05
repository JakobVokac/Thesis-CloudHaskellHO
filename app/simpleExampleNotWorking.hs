{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Data.ByteString.Lazy
import Data.Binary
import Data.Rank1Dynamic
import Control.Distributed.Static
import Control.Distributed.Process
import Control.Distributed.Process.Node hiding (initRemoteTable)
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)


sendnt :: SendPort Int -> Process ()
sendnt s = do
  sendChan s 5
  say $ "sent int"
  terminate

remotable['sendnt]

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8921"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters

  node <- newLocalNode transport rtable

  runProcess node $ do
    nid <- getSelfNode
    (s,r) <- newChan :: Process (SendPort Int, ReceivePort Int)
    _ <- spawn nid $ $(mkClosure 'sendnt) s
    say $ "spawned remote process"
    x <- receiveChan r 
    say $ "received: " ++ show x
    terminate
  where
    rtable :: RemoteTable
    rtable =  __remoteTable initRemoteTable
