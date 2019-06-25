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



receiveMessage :: ReceivePort String -> Process ()
receiveMessage rp = do
  say "working process"
  str <- receiveChan rp
  say str

receiveMessageStatic :: Static (ReceivePort String -> Process ())
receiveMessageStatic = staticLabel "$receiveMessage"

receiveMessageClosure :: Closure (ReceivePort String -> Process ())
receiveMessageClosure = staticClosure receiveMessageStatic

sdictString :: SerializableDict String
sdictString = SerializableDict

sdictStringStatic :: Static (SerializableDict String)
sdictStringStatic = staticLabel "$sdictString"


main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "10111"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable
  runProcess node $ do
    
    sp <- spawnChannel sdictStringStatic (localNodeId node) receiveMessageClosure
    sendChan sp "FUCKING WORK"
    where
      rtable :: RemoteTable
      rtable =  registerStatic "$send" (toDynamic sendInt)
             .  registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> Int))
             .  registerStatic "$receiveMessage" (toDynamic receiveMessage)
             .  registerStatic "$sdictString" (toDynamic sdictString)
             $  initRemoteTable
