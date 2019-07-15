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


-- actual working closure programm --
-- IMPORTANT: initRemoteTable functions differently, depending on
-- where it is imported from. Node version seems to work for all uses,
-- whereas the static version fails when using prebuilt closure functions,
-- like mkClosure and remotable

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


main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8531" (\port -> ("127.0.0.1", port)) defaultTCPParameters
  node <- newLocalNode transport rtable

  runProcess node $ do
    pid <- getSelfPid
    clProc <- return $ encode $ sendIntClosure pid
    Right f <- return $ ((unclosure rtable $ decode clProc) :: Either String (Int -> Process ()))
    f (5 :: Int)
    x <- expect :: Process Int 
    say $ "int " ++ show x
    
  where
    rtable :: RemoteTable
    rtable = registerStatic "$send" (toDynamic sendInt)
           . registerStatic "$decodeProcessId" (toDynamic (decode :: ByteString -> ProcessId))
           $ initRemoteTable
