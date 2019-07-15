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
eqServerB1 r2 x1 s3 = do
  _ <- receiveChan r2
  a <- receiveChan x1
  say $ "received: " ++ show a
  sendChan s3 a

eqServerB2 r3 x2 s4 = do
  a <- receiveChan r3
  b <- receiveChan x2
  say $ "received: " ++ show b
  sendChan s4 (a,b)

eqServerB3 r4 y3 s5 = do
  (a,b) <- receiveChan r4
  sp <- receiveChan y3
  sendChan sp $ a == b 
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

eqClientB1 :: SendPort Int -> SendPort () -> Process ()
eqClientB1 s1 s = do
  sendChan s1 4
  sendChan s ()

eqClientB2 :: SendPort Int -> SendPort Int -> SendPort () -> Process ()
eqClientB2 s1 s2 sFinal = do
  (s,r) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- spawnLocal $ eqClientB1 s1 s
  _ <- receiveChan r
  sendChan s2 2
  sendChan sFinal ()

eqClientB3 :: (SendPort Int, SendPort Int, SendPort (SendPort Bool), SendPort ()) -> Process ()
eqClientB3 (s1, s2, ss3, sFinal) = do
  (s,r) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- spawnLocal $ eqClientB2 s1 s2 s
  _ <- receiveChan r
  rr <- establishChannelBool ss3
  check <- receiveChan rr
  say $ "received: " ++ show check
  sendChan sFinal ()


remotable['eqClientB1, 'eqClientB2, 'eqClientB3]


masterB :: LocalNode -> Process ()
masterB node = do

  -- propagator channels for the breakdown:
  (s1,r1) <- newChan :: Process (SendPort (), ReceivePort ())
  (s2,r2) <- newChan :: Process (SendPort (), ReceivePort ())
  (s3,r3) <- newChan :: Process (SendPort Int, ReceivePort Int)
  (s4,r4) <- newChan :: Process (SendPort (Int,Int), ReceivePort (Int,Int))
  (s5,r5) <- newChan :: Process (SendPort (), ReceivePort ())
  (s6,r6) <- newChan :: Process (SendPort (), ReceivePort ())
  (s7,r7) <- newChan :: Process (SendPort (), ReceivePort ())
  (s8,r8) <- newChan :: Process (SendPort (), ReceivePort ())
  (s9,r9) <- newChan :: Process (SendPort (), ReceivePort ())

  -- decomposition of channels:
  (x1,y1) <- newChan :: Process (SendPort Int, ReceivePort Int)
  (x2,y2) <- newChan :: Process (SendPort Int, ReceivePort Int)
  (x3,y3) <- newChan :: Process (SendPort (SendPort Bool), ReceivePort (SendPort Bool))

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
  Right transport <- createTransport "127.0.0.1" "8322"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  Right transport2 <- createTransport "127.0.0.1" "8324"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters

  node <- newLocalNode transport rtable     
  node2 <- newLocalNode transport rtable     

  runProcess node $ masterB node2
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
