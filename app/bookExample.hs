{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
import System.Environment (getArgs)
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Distributed.Process hiding (Message)
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Network.Transport.TCP (createTransport, defaultTCPParameters)


data Message = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)

instance Binary Message


pingServer :: Process ()
pingServer = do
  Ping from <- expect
  say $ print "ping received from %s" (show from)
  mypid <- getSelfPid
  send from (Pong mypid)


remotable ['pingServer]

master :: Process ()
master = do
  node <- getSelfNode

  say $ print "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)

  mypid <- getSelfPid
  say $ print "sending ping to %s" (show pid)
  send pid (Ping mypid)

  Pong _ <- expect
  say "pong."

  terminate     

main :: IO ()
main = distribMain (\_ -> master) Main.__remoteTable
