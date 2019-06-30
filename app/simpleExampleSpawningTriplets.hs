{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
import System.Environment (getArgs)
import Data.Binary
import Data.Typeable
import GHC.Generics
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Serializable
import Network.Transport.TCP (createTransport, defaultTCPParameters)




-- Attempt at a remote process, where each triplet gets spawned by the previous
-- and each triplet has its own channel.
--
-- Local triplets have receive ports and remote have send ports for serializabiliy.
-- Remote triplets receive data by establishing their own receive ports and sending
-- back the corresponding send ports, which the local triplets then use.
--
-- This method fails for unknown reasons. You get an unknown exception (with a bugged out message)
-- and after the first established channel from the remote triplet, the process fails.




-- HO Process
-- n : S = (?Int:?Int;!Int;End)
-- u : T
{-
   P = S | C 
   P = n?(x).n?(y).m(n).n!(x+y).0|n!<3>.n!<5>.m<n>n?(z).0
-}

-- [[P]] = [[S]] | [[C]]
-- m1!<SendPort n1>.n1>(x)  ... | m1(n1).n1<3>....
-- master 

-- spawnLocal S  
-- spawn C 

-- master (S)
-- mr, ms <- newChan
-- <- spawnLocal (C ns1,ns2, m3) 
-- 

-- C 
-- ... m3<n3>.n3(z).


--Session type structure. There are two structures, for the remote and
--for the local part. The local part only has receive ports and the remote
--part only has send ports such that the data structure is serializable.
--Whenever the remote part needs to receive something, it should set up
--a new channel and send back the sendport.
data LocalProc = LSendInt Int (ReceivePort (SendPort Int)) LocalProc |
            LRecvInt (ReceivePort Int) LocalProc |
            LEnd

data RemoteProc = RSendInt Int (SendPort Int) RemoteProc |
            RRecvInt (SendPort (SendPort Int)) RemoteProc |
            REnd
  deriving (Typeable, Generic)
instance Binary RemoteProc

--Simplified triplets, only sending and receiving right now.

--Triplets for the local process
tripleLocal :: LocalProc -> Process ()
tripleLocal (LSendInt x rsp proc) = do
  sp <- receiveChan rsp
  sendChan sp x
  say $ "Sent value: " ++ show x
  _ <- spawnLocal $ do tripleLocal proc 
  terminate

tripleLocal (LRecvInt rp proc) = do
  x <- receiveChan rp
  say $ "Received value: " ++ show x
  _ <- spawnLocal $ do tripleLocal proc
  terminate

tripleLocal (LEnd) = do
  say $ "End"

--Triplets for the remote process
tripleRemote :: RemoteProc -> Process ()
tripleRemote (RSendInt x sp proc) = do
  sendChan sp x
  say $ "Sent value: " ++ show x
  _ <- spawnLocal $ do tripleRemote proc 
  terminate

tripleRemote (RRecvInt ssp proc) = do
  rp <- establishChannel ssp
  say $ "Established channel"
  x <- receiveChan rp
  say $ "Received x: " ++ show x
  _ <- spawnLocal $ do tripleRemote proc 
  terminate

tripleRemote (REnd) = do
  say $ "End"

--Here we establish a receive port for the remote process.
establishChannel :: SendPort (SendPort Int) -> Process (ReceivePort Int)
establishChannel spInit = do
  (spNew, rp) <- newChan :: Process (SendPort Int, ReceivePort Int)
  sendChan spInit spNew
  return rp

remotable['tripleRemote]

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8322"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  Right transport2 <- createTransport "127.0.0.1" "8324"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters

  node <- newLocalNode transport rtable
  node2 <- newLocalNode transport2 rtable

  --The idea is that we have two nodes, one with a remote process and one
  --with a local process. The remote process is turned into a closure and
  --the local process doesn't have to be.
  runProcess node $ do
    (s1,r1) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
    (s2,r2) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
    (s3,r3) <- newChan :: Process (SendPort Int, ReceivePort Int)

    (sfake,rfake) <- newChan :: Process (SendPort Int, ReceivePort Int)
    
    _ <- spawn (localNodeId node2) ( $(mkClosure 'tripleRemote) (RRecvInt s1 $ RRecvInt s2 $ RSendInt 7 s3 REnd) ) 
    
    tripleLocal (LSendInt 4 r1 $ LSendInt 5 r2 $ LRecvInt r3 LEnd)

    --Is here just so the main process doesn't terminate before the other two.
    a <- receiveChan rfake
    say $ show a

  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
