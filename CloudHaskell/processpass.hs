import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)


masterB :: Process ()
masterB = do


procDP = do

	(s1,r1) <- newChan :: Process (SendPort (), ReceivePort ())
	(s2,r2) <- newChan :: Process (SendPort (), ReceivePort ())
	(s11,r11) <- newChan :: Process (SendPort (), ReceivePort ())

	(su1,ru1) <- newChan :: Process (SendPort (), ReceivePort ())

	spawnLocal $ initProcess s1
	spawnLocal $ startBQBR r1 s2 s11
	
	procBQ r2 su1
	procBR r11 ru1

initProcess s1 = do


startBQBR r1 s2 s11 = do


procBQ r2 su1 = do

	(s3,r3) <- newChan :: Process (SendPort (), ReceivePort ())
	(s4,r4) <- newChan :: Process (SendPort (), ReceivePort ())
	(s5,r5) <- newChan :: Process (SendPort (), ReceivePort ())
	(s6,r6) <- newChan :: Process (SendPort (), ReceivePort ())
	(s7,r7) <- newChan :: Process (SendPort (), ReceivePort ())
	(s8,r8) <- newChan :: Process (SendPort (), ReceivePort ())
	(s9,r9) <- newChan :: Process (SendPort (), ReceivePort ())
	(s10,r10) <- newChan :: Process (SendPort (), ReceivePort ())

	(ss1,rs1) <- newChan :: Process (SendPort (), ReceivePort ())

	spawnLocal $ 


procBQ1 r2 su1 procVV s5 = do
	
	_ <- receiveChan r2
	sendChan su1 procVV
	sendChan s5 ()


procBR r11 ru1 = do

	(s12,r12) <- newChan :: Process (SendPort (), ReceivePort ())
	(s13,r13) <- newChan :: Process (SendPort (), ReceivePort ())
	(s14,r14) <- newChan :: Process (SendPort (), ReceivePort ())
	(s15,r15) <- newChan :: Process (SendPort (), ReceivePort ())
	(s16,r16) <- newChan :: Process (SendPort (), ReceivePort ())
	(s17,r17) <- newChan :: Process (SendPort (), ReceivePort ())
	(s18,r18) <- newChan :: Process (SendPort (), ReceivePort ())
	(s19,r19) <- newChan :: Process (SendPort (), ReceivePort ())

	(ss1,rs1) <- newChan :: Process (SendPort (), ReceivePort ())

	spawnLocal $


procBR1 r11 ru1 s12 = do
	
	_ <- receiveChan r11
	y <- receiveChan ru1
	sendChan s12 y


procVV = do


procVb = do


procVW = do


procVU = do




main :: IO ()
main = do
	[host, port] <- getArgs
	Right transport <- createTransport host port (\port -> ("localhost", port)) defaultTCPParameters
	node <- newLocalNode transport initRemoteTable     

	runProcess node masterB
