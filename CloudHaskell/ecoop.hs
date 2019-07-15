import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)


masterB :: Process ()
masterB = do

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
  (x3,y3) <- newChan :: Process (SendPort Bool, ReceivePort Bool)

  -- trio for channel split
  spawnLocal $ clientServerSplit r1 s2 s6

  -- spawning the three leading trios for the server:
  spawnLocal $ eqServerB1 r2 y1 s3
  spawnLocal $ eqServerB2 r3 y2 s4
  spawnLocal $ eqServerB3 r4 x3 s5

  -- spawning the three leading trios for the client:
  spawnLocal $ eqClientB1 r6 x1 s7
  spawnLocal $ eqClientB2 r7 x2 s8
  spawnLocal $ eqClientB3 r8 y3 s9

  -- degenerate trios, to see if the whole communication worked
  sendChan s1 ()
  _ <- receiveChan r5
  _ <- receiveChan r9

  say $ "end of program"

  
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
   sendChan y3 $ a == b 
   sendChan s5 ()

-- breakdown of eqClient:
eqClientB1 r6 y1 s7 = do
   _ <- receiveChan r6
   sendChan y1 4
   sendChan s7 ()

eqClientB2 r7 y2 s8 = do
    _ <- receiveChan r7
    sendChan y2 2
    sendChan s8 ()

eqClientB3 r8 x3 s9 = do
   _ <- receiveChan r8
   check <- receiveChan x3
   say $ "received: " ++ show check
   sendChan s9 ()



main :: IO ()
main = do
    [host, port] <- getArgs
    Right transport <- createTransport host port (\port -> ("localhost", port)) defaultTCPParameters
    node <- newLocalNode transport initRemoteTable     

    runProcess node masterB
