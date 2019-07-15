{-# LANGUAGE TemplateHaskell #-}

import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)



clientServerSplit r1 s2 s6 = do
  _ <- receiveChan r1
  sendChan s2 ()
  sendChan s6 ()


-- breakdown of eqServer:
eqServerB1 r2 y1 s3 = do
  _ <- receiveChan r2
  sp <- receiveChan y1
  sendChan sp 4
  sendChan s3 ()

eqServerB2 r3 y2 s4 = do
  _ <- receiveChan r3
  sp <- receiveChan y2
  sendChan sp 4
  sendChan s4 ()

eqServerB3 r4 y3 s5 = do
  _ <- receiveChan r4
  b <- receiveChan y3
  say $ "received answer: " ++ show b
  sendChan s5 ()


establishChannelInt :: SendPort (SendPort Int) -> Process (ReceivePort Int)
establishChannelInt spInit = do
  (spNew, rp) <- newChan :: Process (SendPort Int, ReceivePort Int)
  sendChan spInit spNew
  return rp

establishChannelBool :: SendPort (SendPort Bool) -> Process (ReceivePort Bool)
establishChannelBool spInit = do
  (spNew, rp) <- newChan :: Process (SendPort Bool, ReceivePort Bool)
  sendChan spInit spNew
  return rp

-- breakdown of eqClient:

eqClientB1 :: SendPort (SendPort Int) -> SendPort (Int) -> Process ()
eqClientB1 ss1 s = do
  rr <- establishChannelInt ss1
  a <- receiveChan rr
  say $ "received: " ++ show a
  sendChan s (a)

eqClientB2 :: SendPort (SendPort Int) -> SendPort (SendPort Int) -> SendPort (Int, Int) -> Process ()
eqClientB2 ss1 ss2 sFinal = do
  (s,r) <- newChan :: Process (SendPort (Int), ReceivePort (Int))
  _ <- spawnLocal $ eqClientB1 ss1 s
  a <- receiveChan r
  rr <- establishChannelInt ss2
  b <- receiveChan rr
  say $ "received: " ++ show b
  sendChan sFinal (a,b)

eqClientB3 :: (SendPort (SendPort Int), SendPort (SendPort Int), SendPort Bool, SendPort ()) -> Process ()
eqClientB3 (ss1, ss2, s3, sFinal) = do
  (s,r) <- newChan :: Process (SendPort (Int, Int), ReceivePort (Int, Int))
  _ <- spawnLocal $ eqClientB2 ss1 ss2 s
  (a,b) <- receiveChan r
  sendChan s3 $ a == b 
  sendChan sFinal ()


remotable['eqClientB1, 'eqClientB2, 'eqClientB3]


masterB :: LocalNode -> Process ()
masterB node = do

  -- propagator channels for the breakdown:
  (s1,r1) <- newChan :: Process (SendPort (), ReceivePort ())
  (s2,r2) <- newChan :: Process (SendPort (), ReceivePort ())
  (s3,r3) <- newChan :: Process (SendPort (), ReceivePort ())
  (s4,r4) <- newChan :: Process (SendPort (), ReceivePort ())
  (s5,r5) <- newChan :: Process (SendPort (), ReceivePort ())
  (s6,r6) <- newChan :: Process (SendPort (), ReceivePort ())
  (s7,r7) <- newChan :: Process (SendPort (), ReceivePort ())
  (s8,r8) <- newChan :: Process (SendPort (), ReceivePort ())
  (s9,r9) <- newChan :: Process (SendPort (), ReceivePort ())

  -- decomposition of channels:
  (x1,y1) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
  (x2,y2) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
  (x3,y3) <- newChan :: Process (SendPort Bool, ReceivePort Bool)

  -- trio for channel split
  spawnLocal $ clientServerSplit r1 s2 s6

  -- spawning the three leading trios for the server:
  spawnLocal $ eqServerB1 r2 y1 s3
  spawnLocal $ eqServerB2 r3 y2 s4
  spawnLocal $ eqServerB3 r4 y3 s5
  
  spawn (localNodeId node) ( $(mkClosure 'eqClientB3) (x1, x2, x3, s9) )

  sendChan s1 ()
  _ <- receiveChan r5
  _ <- receiveChan r9

  say $ "end of program"



main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8422"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  Right transport2 <- createTransport "127.0.0.1" "8424"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters

  node <- newLocalNode transport rtable     
  node2 <- newLocalNode transport rtable     

  runProcess node $ masterB node2
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
