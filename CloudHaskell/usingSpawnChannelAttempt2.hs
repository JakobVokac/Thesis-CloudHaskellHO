{-# LANGUAGE TemplateHaskell #-}
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)


-- Doesn't work. I've tried changing up multiple things and nothing seems
-- to help. I don't understand the difference between this and
-- the simpleExampleNoTriplets.hs, which works perfectly and also
-- spawns remote processes using closures, using the same functions
-- to do so.


receiveAnInt :: ReceivePort Int -> Process ()
receiveAnInt r = do
  say $ "trying to receive an int"
  x <- receiveChan r
  say $ "received an int: " ++ show x
  terminate

sendAnInt :: (SendPort Int, Int) -> Process ()
sendAnInt (s, x) = do
  say $ "trying to send an int"
  sendChan s x
  say $ "sent an int: "
  terminate

remotable['sendAnInt]


main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8524" (\port -> ("127.0.0.1", port)) defaultTCPParameters
  node <- newLocalNode transport rtable

  runProcess node $ do
    (s,r) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (sProc,rProc) <- newChan
    (sfake,rfake) <- newChan :: Process (SendPort Int, ReceivePort Int)
    
    pid <- spawnLocal $ do
      sendChan sProc ($(mkClosure 'sendAnInt) (s, 5 :: Int))
      say $ "hello1"
      receiveAnInt r
      say $ "hello2"

    process <- receiveChan rProc
    say $ "received process"
    nid <- getSelfNode
    pid2 <- spawn nid process
    
    x <- receiveChan rfake
    say $ "it Works!!"
  where
    rtable :: RemoteTable
    rtable =  __remoteTable initRemoteTable

