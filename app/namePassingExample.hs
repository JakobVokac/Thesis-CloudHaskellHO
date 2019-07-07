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


type ProcClosure = ByteString


dP :: Process ()
dP = do

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
  (s10,r10) <- newChan :: Process (SendPort (), ReceivePort ())
  (s11,r11) <- newChan :: Process (SendPort (), ReceivePort ())
  (s12,r12) <- newChan :: Process (SendPort (), ReceivePort ())
  (s13,r13) <- newChan :: Process (SendPort (), ReceivePort ())
  (s14,r14) <- newChan :: Process (SendPort (), ReceivePort ())
  (s15,r15) <- newChan :: Process (SendPort (), ReceivePort ())
  (s16,r16) <- newChan :: Process (SendPort (), ReceivePort ())
  (s17,r17) <- newChan :: Process (SendPort (), ReceivePort ())
  (s18,r18) <- newChan :: Process (SendPort (), ReceivePort ())
  (s19,r19) <- newChan :: Process (SendPort (), ReceivePort ())

  spawn trioStart
  spawn trioQRSplit
  spawn bQ
  spawn bR

  say $ "end of program"

bQ :: Process ()
bQ = do
  
  spawn
  

bR :: Process ()
bR = do

vV :: ReceivePort ProcClosure -> Process ()
vV rpProc = do

vDeltab0 :: Bool -> Process ()
vDeltab0 b = do
  
vW :: SendPort ProcClosure -> Process ()
vW spProc = do

vW2 :: ReceivePort ProcClosure -> Process ()
vW2 rpProc = do



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

trioQ1 :: ReceivePort () -> SendPort ProcClosure -> SendPort () -> Process ()
trioQ1 rpS spU1 spE = do
  _ <- receiveChan rpS
  sendChan spU1 {-insert vV closure here-}
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
trioQ4 rpProc rpS1 = do
  y <- receiveChan rpProc
  Right proc <- return $ unclosure rtable $ decode y
  proc rpS1
  say $ "trioQ4 end"
  
trioQ5 :: ReceivePort () -> SendPort ProcClosure -> SendPort () -> Process ()
trioQ5 rpS spS1 spE = do
  _ <- receiveChan rpS
  sendChan spS1 {-insert vDeltab0 closure here-}
  sendChan spE () 
  say $ "trioQ5 end"

trioQ6 :: ReceivePort () -> Process ()
trioQ6 rpS = do
  _ <- receiveChan rpS
  say $ "trioQ6 end, Q process done"
  

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8097"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  runProcess node masterP

-- Global declaration of rtable. Necessary for remote use of encoded functions.
rtable :: RemoteTable
rtable = 
       . 
       $ __remoteTable initRemoteTable







