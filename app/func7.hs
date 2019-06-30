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

--Environment for variables

--spawnChannelLocal :: Serializable a => (ReceivePort a -> Process ()) -> Process (SendPort a)

--Session type structure (quite messy at the moment, but I wanted to
--focus primarily on getting the example to work)
data LocalProc = LSendInt Int (ReceivePort (SendPort Int)) LocalProc |
            LRecvInt (ReceivePort Int) LocalProc |
            LEnd

data RemoteProc = RSendInt Int (SendPort Int) RemoteProc |
            RRecvInt (SendPort (SendPort Int)) RemoteProc |
            REnd
  deriving (Typeable, Generic)
instance Binary RemoteProc

--Simplified triplets, only sending and receiving right now.
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

--Initial function for the remote process. Here we establish a receive port
--for the remote process.
--
--The receive port doesn't work yet.
establishChannel :: SendPort (SendPort Int) -> Process (ReceivePort Int)
establishChannel spInit = do
  (spNew, rp) <- newChan :: Process (SendPort Int, ReceivePort Int)
  sendChan spInit spNew
  return rp

remotable[ 'tripleRemote ]








tripletLocal1 = do
  x <- receiveChan rp
  say $ "received at 1: " ++ show x
  tripletLocal2 x

tripletLocal2 x = do
  y <- receiveChan rp
  say $ "received at 2: " ++ show y
  tripletLocal3 x y

tripletLocal3 x y = do
  sendChan sp (x+y)
  say $ "sent at 3: " ++ show (x+y)
  say $ "End"
  terminate


tripletRemote1 x y = do
  sendChan sp x
  say $ "sent at 1: " ++ show x
  tripletRemote2 y

tripletRemote2 y = do
  sendChan sp y
  say $ "sent at 2: " ++ show y
  tripletRemote3 

tripletRemote3 = do
  z <- receiveChan rp
  say $ "received sum at 3: " ++ show z
  say $ "End"
  terminate

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8192"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  --The idea is that the channels are created and the variables are saved
  --in the environment, so they can be used passed from triplet to triplet,
  --like they would in HO. This current example doesn't work yet because
  --of the closure part.
  runProcess node $ do
    (s1,r1) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
    (s2,r2) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
    (s3,r3) <- newChan :: Process (SendPort Int, ReceivePort Int)

    (sfake,rfake) <- newChan :: Process (SendPort Int, ReceivePort Int)
    
    nodeId <- getSelfNode
    _ <- spawn nodeId ( $(mkClosure 'tripleRemote) (RRecvInt s1 $ RRecvInt s2 $ RSendInt 7 s3 REnd) )
    
    tripleLocal (LSendInt 4 r1 $ LSendInt 5 r2 $ LRecvInt r3 LEnd)

    --Is here just so the main process doesn't terminate before the other two.
    a <- receiveChan rfake
    say $ show a

  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
