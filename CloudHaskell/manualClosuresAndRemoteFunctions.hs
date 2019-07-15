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

-- Example with different uses of manual closures
-- !IMPORTANT!: always use the initRemoteTable from the Node package,
-- not the Static package, otherwise the remotable[] declarations won't
-- work and no error will be given.
-- Also, never use the function terminate!! Used for stopping the program
-- completely, not for exiting a process.





------------------------------------------------------------------------

-- Manual closure that is encoded and decoded in the same process
sendInt :: ProcessId -> Int -> Process ()
sendInt pid x = send pid x

sendIntStatic :: Static (ProcessId -> Int -> Process ())
sendIntStatic = staticLabel "$send"

decodeProcessIdStatic :: Static (ByteString -> ProcessId)
decodeProcessIdStatic = staticLabel "$decodeProcessId"

sendIntClosure :: ProcessId -> Closure (Int -> Process ())
sendIntClosure pid = closure decoder (encode pid)
  where
    decoder :: Static (ByteString -> Int -> Process ())
    decoder = sendIntStatic `staticCompose` decodeProcessIdStatic

------------------------------------------------------------------------







------------------------------------------------------------------------

-- Manual closure for the spawnChannel function
-- Here we use the staticClosure function instead of closure, since we
-- don't have an environment that we have to encode,
-- like in the previous example, where the
-- environment is pid :: ProcessId
rpProc :: ReceivePort Int -> Process ()
rpProc rp = do
  x <- receiveChan rp
  say $ "received int remotely with manual closure: " ++ show x
  
rpProcStatic :: Static (ReceivePort Int -> Process ())
rpProcStatic = staticLabel "$rpProc"

rpProcClosure :: Closure (ReceivePort Int -> Process ())
rpProcClosure = staticClosure rpProcStatic

sdictT :: SerializableDict Int
sdictT = SerializableDict

sdictBS :: SerializableDict ByteString
sdictBS = SerializableDict

------------------------------------------------------------------------





------------------------------------------------------------------------

-- Automatic closure to make sure these work with the same RemoteTable,
-- since the remote table is made from both manual entries and
-- the Template Haskell entries from remotable[]
sendnt :: SendPort Int -> Process ()
sendnt s = do
  sendChan s 5
  say $ "sent int"
  terminate

------------------------------------------------------------------------





------------------------------------------------------------------------

-- Function that is closed and sent over a channel then decoded and used
-- remotely. Works fine, but you have to make sure to make the remoteTable
-- (rtable) global so that it can be used by any process running this code
remoteEQ :: Int -> Int -> Process (Bool)
remoteEQ x y = return (x == y)

remoteEQStatic :: Static (Int -> Int -> Process (Bool))
remoteEQStatic = staticLabel "$EQ"

remoteEQClosure :: Closure (Int -> Int -> Process (Bool))
remoteEQClosure = staticClosure remoteEQStatic

------------------------------------------------------------------------





------------------------------------------------------------------------

-- Process to be run remotely. Receives the encoded function through
-- a receivePort and then uses it automatically. Works fine, as long
-- as the remoteTable (rtable) is global.
remoteEQProc :: ReceivePort (ByteString) -> Process ()
remoteEQProc rp = do
  say $ "spawned remoteEQProc1"
  fClosure <- receiveChan rp
  say $ "spawned remoteEQProc2"
  Right f <- return $ unclosure rtable (decode fClosure :: (Closure (Int -> Int -> Process (Bool))))
  --say $ "function: " ++ show f
  say $ "spawned remoteEQProc3"
  b <- f (5 :: Int) (6 :: Int)
  say $ "remote result: " ++ show (b :: Bool)

remoteEQProcClosure :: Closure (ReceivePort (ByteString) -> Process ())
remoteEQProcClosure = staticClosure $ staticLabel "$EQProc"

------------------------------------------------------------------------



remotable['sendnt, 'sdictT, 'sdictBS]



main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8097"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  runProcess node $ do
  
    -- Spawning a normal remote process with mkClosure and remotable[]
    say $ "spawning normally closed process"
    nid <- getSelfNode
    (s,r) <- newChan :: Process (SendPort Int, ReceivePort Int)
    _ <- spawn nid $ $(mkClosure 'sendnt) s
    say $ "spawned remote process"
    x <- receiveChan r 
    say $ "received: " ++ show x
    
    
    -- Encoding and decoding a manually closed process, then using it.
    say $ "building and decoding manual closure"
    pid <- getSelfPid
    clProc <- return $ encode $ sendIntClosure pid
    Right f <- return $ ((unclosure rtable $ decode clProc) :: Either String (Int -> Process ()))
    f (5 :: Int)
    x <- expect :: Process Int 
    say $ "int " ++ show x
    
    
    -- Using spawnChannel to spawn a manually closed remote process
    -- with a receivePort.
    say $ "spawning manually closed process (no environment) and using spawnChannel"
    sp <- spawnChannel ($(mkStatic 'sdictT)) nid rpProcClosure
    sendChan sp (5 :: Int)
    say $ "sent Int"
    
    -- Spawning a manually closed process with a receivePort for a function
    -- that is decoded and used remotely.
    say $ "sending over and using function on remote process" 
    sp <- spawnChannel ($(mkStatic 'sdictBS)) nid remoteEQProcClosure
    sendChan sp (encode remoteEQClosure)
    say $ "sent function"
    
    
-- Global declaration of rtable. Necessary for remote use of encoded functions.
rtable :: RemoteTable
rtable = registerStatic "$send" (toDynamic sendInt)
       . registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> ProcessId))
       . registerStatic "$rpProc" (toDynamic rpProc)
       . registerStatic "$EQ" (toDynamic remoteEQ)
       . registerStatic "$EQProc" (toDynamic remoteEQProc)
       $ __remoteTable initRemoteTable
