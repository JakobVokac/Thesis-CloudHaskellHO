{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)

p1 :: SendPort String -> Process ()
p1 p = do
          say $ "hello"
          sendChan p "hello"
p2 :: SendPort String -> Process ()
p2 p = do
          say $ "hello2"
          sendChan p "hello2"

p3 :: ReceivePort String -> Process ()
p3 rp = do
          say $ "hello3"
          str <- receiveChan rp
          say $ str

remotable ['p1, 'p2, 'p3]

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "10532"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable
  runProcess node $ do
    (s1,r1) <- newChan
    (s2,r2) <- newChan
    
    m <- returnCP $(mkStatic 'p3) p3
    sp <- spawnChannel $(mkStatic 'p3) (localNodeId node) p3

    sendChan sp "hello back"
    
    
    where
      rtable :: RemoteTable
      rtable = __remoteTable initRemoteTable
