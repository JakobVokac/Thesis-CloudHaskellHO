{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Data.ByteString.Lazy
import Data.Binary
import Data.Rank1Dynamic
import Control.Distributed.Static hiding (initRemoteTable)
import Control.Distributed.Process
import Control.Distributed.Process.Node -- hiding (initRemoteTable)
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)



-- Name passing example with local processes, to make sending receivePorts easier.
-- Currently works, but all meta processes (ones that spawn trios) need to
-- be kept running indefinitely, since I haven't figured out how to exit
-- them based on the spawned processes and if the meta process exits, then
-- the spawned processes exit abruptly as well from what I can tell from
-- testing.

-- Usual note: - do not use terminate, exits program
--             - do not use initRemoteTable from Static, only Node

type ProcClosure = ByteString

------------------------------------------------------------------------

-- Processes and trios

dP :: Process ()
dP = do

  -- propagator channels for the breakdown:
  (s1,r1) <- newChan :: Process (SendPort (), ReceivePort ())
  (s2,r2) <- newChan :: Process (SendPort (), ReceivePort ())
  (s11,r11) <- newChan :: Process (SendPort (), ReceivePort ())

  (sU1,rU1) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)

  pid1 <- spawnLocal $ trioStart s1
  pid2 <- spawnLocal $ trioQRSplit r1 s2 s11
  pid3 <- spawnLocal $ bQ r2 sU1 
  pid4 <- spawnLocal $ bR r11 rU1 
  
  say $ show mRef4
  say $ "end of program"
  
  
  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()


bQ :: ReceivePort () -> SendPort ProcClosure -> Process ()
bQ r2 sU1 = do
  
  (s5,r5) <- newChan :: Process (SendPort (), ReceivePort ())
  (s6,r6) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  (s7,r7) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  (s8,r8) <- newChan :: Process (SendPort (), ReceivePort ())
  (s10,r10) <- newChan :: Process (SendPort (), ReceivePort ())
  
  (sS1,rS1) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  (sM1,rM1) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
    
  spawnLocal $ trioQ1 r2 sU1 sM1 s5
  spawnLocal $ trioQ2 r5 rM1 s6
  spawnLocal $ trioQ3 r6 s7 s8
  spawnLocal $ trioQ4 r7 rS1
  spawnLocal $ trioQ5 r8 sS1 s10
  spawnLocal $ trioQ6 r10
  
  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()


bR :: ReceivePort () -> ReceivePort ProcClosure -> Process ()
bR r11 rU1 = do

  (s12,r12) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  (s13,r13) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  (s14,r14) <- newChan :: Process (SendPort (), ReceivePort ())
  (s19,r19) <- newChan :: Process (SendPort (), ReceivePort ())
  
  (sS1,rS1) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)

  spawnLocal $ trioR1 r11 rU1 s12
  spawnLocal $ trioR2 r12 s13 s14
  spawnLocal $ trioR3 r13 rS1
  spawnLocal $ trioR4 r14 sS1 s19
  spawnLocal $ trioR5 r19

  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()


vV :: SendPort ProcClosure -> ReceivePort ProcClosure -> Process ()
vV sM1 rZ1 = do

  (s3,r3) <- newChan :: Process (SendPort (), ReceivePort ())
  (s4,r4) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)
  
  spawnLocal $ triovV1 s3
  spawnLocal $ triovV2 r3 rZ1 s4
  spawnLocal $ triovV3 r4 sM1

  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()

  
vDeltab0 :: Bool -> Process ()
vDeltab0 b = do

  (s9,r9) <- newChan :: Process (SendPort (), ReceivePort ())
  
  spawnLocal $ triovDeltab01 s9
  spawnLocal $ triovDeltab02 r9
  
  say $ "vDeltab0 applied with bool: " ++ show b
  
  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()


  
vW :: SendPort ProcClosure -> Process ()
vW sX1 = do
  
  (s15,r15) <- newChan :: Process (SendPort (), ReceivePort ())
  (s18,r18) <- newChan :: Process (SendPort (), ReceivePort ())
  
  spawnLocal $ triovW1 s15
  spawnLocal $ triovW2 r15 sX1 s18
  spawnLocal $ triovW3 r18
  
  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  
  return ()


vW2 :: ReceivePort ProcClosure -> Process ()
vW2 rZ1 = do

  (s16,r16) <- newChan :: Process (SendPort (), ReceivePort ())
  (s17,r17) <- newChan :: Process (SendPort ProcClosure, ReceivePort ProcClosure)

  spawnLocal $ triovW21 s16
  spawnLocal $ triovW22 r16 rZ1 s17
  spawnLocal $ triovW23 r17 True
  
  -- Keeping main process running
  (sFake, rFake) <- newChan :: Process (SendPort (), ReceivePort ())
  _ <- receiveChan rFake
  --
  return ()


trioStart :: SendPort () -> Process ()
trioStart sp = do
  sendChan sp ()
  say $ "trioStart end"

trioQRSplit :: ReceivePort () -> SendPort () -> SendPort () -> Process ()
trioQRSplit rpS spQ spR = do
  _ <- receiveChan rpS
  sendChan spQ ()
  sendChan spR ()
  say $ "trioQRSplit end"

trioQ1 :: ReceivePort () -> SendPort ProcClosure -> SendPort ProcClosure -> SendPort () -> Process ()
trioQ1 rpS spU1 sM1 spE = do
  _ <- receiveChan rpS
  sendChan spU1 (encode $ vVClosure sM1)
  sendChan spE ()
  say $ "trioQ1 end"

trioQ2 :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
trioQ2 rpS rpM1 spE = do
  _ <- receiveChan rpS
  y <- receiveChan rpM1
  sendChan spE y
  say $ "trioQ2 end"

trioQ3 :: ReceivePort ProcClosure -> SendPort ProcClosure -> SendPort () -> Process ()
trioQ3 rpS sp1 sp2 = do
  y <- receiveChan rpS
  sendChan sp1 y
  sendChan sp2 ()
  say $ "trioQ3 end"
  
trioQ4 :: ReceivePort ProcClosure -> ReceivePort ProcClosure -> Process ()
trioQ4 rpS rpS1 = do
  y <- receiveChan rpS
  Right proc <- return $ unclosure rtable $ (decode y :: Closure (ReceivePort ProcClosure -> Process ()))
  proc rpS1
  say $ "trioQ4 end"
  
trioQ5 :: ReceivePort () -> SendPort ProcClosure -> SendPort () -> Process ()
trioQ5 rpS spS1 spE = do
  _ <- receiveChan rpS
  sendChan spS1 (encode vDeltab0Closure)
  sendChan spE () 
  say $ "trioQ5 end"

trioQ6 :: ReceivePort () -> Process ()
trioQ6 rpS = do
  _ <- receiveChan rpS
  say $ "trioQ6 end, Q process done"
  
trioR1 :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
trioR1 rpS rpU1 spE = do
  _ <- receiveChan rpS
  y <- receiveChan rpU1
  sendChan spE y
  say $ "trioR1 end" 

trioR2 :: ReceivePort ProcClosure -> SendPort ProcClosure -> SendPort () -> Process ()
trioR2 rpS sp1 sp2 = do
  y <- receiveChan rpS
  sendChan sp1 y
  sendChan sp2 ()
  say $ "trioR2 end"

trioR3 :: ReceivePort ProcClosure -> ReceivePort ProcClosure -> Process ()
trioR3 rpS rpS1 = do
  x <- receiveChan rpS
  Right proc <- return $ unclosure rtable $ (decode x :: Closure (ReceivePort ProcClosure -> Process ()))
  proc rpS1
  say $ "trioR3 end"

trioR4 :: ReceivePort () -> SendPort ProcClosure -> SendPort () -> Process ()
trioR4 rpS spS1 spE = do
  _ <- receiveChan rpS
  sendChan spS1 (encode vWClosure)
  sendChan spE ()
  say $ "trioR4 end"

trioR5 :: ReceivePort () -> Process ()
trioR5 rpS = do
  _ <- receiveChan rpS
  say $ "trioR5 end, R process done"

triovV1 :: SendPort () -> Process ()
triovV1 sp = do
  sendChan sp ()
  say $ "triovV1 end"
  
triovV2 :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
triovV2 rpS rpZ1 spE = do
  _ <- receiveChan rpS
  x <- receiveChan rpZ1
  sendChan spE x
  say $ "triovV2 end"

triovV3 :: ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
triovV3 rpS spM1 = do
  x <- receiveChan rpS
  Right proc <- return $ unclosure rtable $ (decode x :: Closure (SendPort ProcClosure -> Process ()))
  proc spM1
  say $ "triovV3 end, vV process done"

triovDeltab01 :: SendPort () -> Process ()
triovDeltab01 sp = do
  sendChan sp ()
  say $ "triovDeltab01 end"

triovDeltab02 :: ReceivePort () -> Process ()
triovDeltab02 rp = do
  _ <- receiveChan rp
  say $ "triovDeltab02 end, vDeltab0 process done"

triovW1 :: SendPort () -> Process ()
triovW1 sp = do
  sendChan sp ()
  say $ "triovW1 end"
  
triovW2 :: ReceivePort () -> SendPort ProcClosure -> SendPort () -> Process ()
triovW2 rpS spX1 spE = do
  _ <- receiveChan rpS
  sendChan spX1 (encode vW2Closure)
  sendChan spE ()
  say $ "triovW2 end"
  
triovW3 :: ReceivePort () -> Process ()
triovW3 rpS = do
  _ <- receiveChan rpS
  say $ "triovW3 end, vW process done"

triovW21 :: SendPort () -> Process ()
triovW21 sp = do
  sendChan sp ()
  say $ "triovW21 end"
  
triovW22 :: ReceivePort () -> ReceivePort ProcClosure -> SendPort ProcClosure -> Process ()
triovW22 rpS rpZ1 spE = do
  _ <- receiveChan rpS
  x <- receiveChan rpZ1
  sendChan spE x
  say $ "triovW22 end"

triovW23 :: ReceivePort ProcClosure -> Bool -> Process ()
triovW23 rpS b = do
  x <- receiveChan rpS
  Right proc <- return $ unclosure rtable $ (decode x :: Closure (Bool -> Process ()))
  proc b
  say $ "triovW23 end, vW2 process done"


------------------------------------------------------------------------

-- Closures


--vV :: SendPort ProcClosure -> ReceivePort ProcClosure -> Process ()
vVStatic :: Static (SendPort ProcClosure -> ReceivePort ProcClosure -> Process ())
vVStatic = staticLabel "$vV"

decodeSPPCl :: Static (ByteString -> SendPort ProcClosure)
decodeSPPCl = staticLabel "$decodeSPPCl"

vVClosure :: SendPort ProcClosure -> Closure (ReceivePort ProcClosure -> Process ())
vVClosure sp = closure decoder (encode sp)
  where 
    decoder :: Static (ByteString -> ReceivePort ProcClosure -> Process ())
    decoder = vVStatic `staticCompose` decodeSPPCl


--vDeltab0 :: Bool -> Process ()
vDeltab0Static :: Static (Bool -> Process ())
vDeltab0Static = staticLabel "$vDeltab0"

vDeltab0Closure :: Closure (Bool -> Process ())
vDeltab0Closure = staticClosure vDeltab0Static


--vW :: SendPort ProcClosure -> Process ()
vWStatic :: Static (SendPort ProcClosure -> Process ())
vWStatic = staticLabel "$vW"

vWClosure :: Closure (SendPort ProcClosure -> Process ())
vWClosure = staticClosure vWStatic 


--vW2 :: ReceivePort ProcClosure -> Process ()
vW2Static :: Static (ReceivePort ProcClosure -> Process ())
vW2Static = staticLabel "$vW2"

vW2Closure :: Closure (ReceivePort ProcClosure -> Process ())
vW2Closure = staticClosure vW2Static 


-- Global declaration of rtable. Necessary for remote use of encoded functions.
rtable :: RemoteTable
rtable = registerStatic "$vV" (toDynamic vV)
       . registerStatic "$decodeSPPCl" (toDynamic (decode :: ByteString -> SendPort ProcClosure))
       . registerStatic "$vDeltab0" (toDynamic vDeltab0)
       . registerStatic "$vW" (toDynamic vW)
       . registerStatic "$vW2" (toDynamic vW2)
       $ initRemoteTable


main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8108"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  runProcess node dP





