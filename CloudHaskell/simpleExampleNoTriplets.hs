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



-- Attempt at remote process without spawning subsequent triplets and
-- using the same channels throughout a single process on each side.
-- This attempt works completely, however the problem comes when the
-- remote process should split, since we can't run more than 2 processes (1 local, 1 remote)
-- concurrently with this method.
--
-- The problem aside from this is that is isn't a proper simulation of the decomposition
-- since there aren't any propagators, so the context passing would have
-- to be done differently.



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

type Channel = (SendPort Int, ReceivePort Int)
--Environment for variables

--Session type structure (quite messy at the moment, but I wanted to
--focus primarily on getting the example to work)
data Proc = SendInt Int Proc |
          RecvInt Proc |
          End
  deriving (Typeable, Generic)
instance Binary Proc

--Simplified triplets with no spawning, only sending and receiving right now.
triple :: Proc -> Channel -> Process ()
triple (SendInt x proc) (sp, rp) = do
  sendChan sp x
  say $ "Sent value: " ++ show x
  triple proc (sp, rp)

triple (RecvInt proc) (sp, rp) = do
  x <- receiveChan rp
  say $ "Received value: " ++ show x
  triple proc (sp, rp)

triple (End) (sp, rp) = do
  say $ "End"
  terminate

--Initial function for the remote process. Here we establish a receive port
--for the remote process. 
--
--The receive port doesn't work yet.
establishChannel :: (SendPort (SendPort Int), SendPort Int, Proc) -> Process ()
establishChannel (spInit, sp, proc) = do
  (spNew, rp) <- newChan :: Process (SendPort Int, ReceivePort Int)
  sendChan spInit spNew
  say $ "Port sent!"
  triple proc (sp, rp)

remotable['triple, 'establishChannel]

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8185"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  --The idea is that the channels are created and the variables are saved
  --in the environment, so they can be used passed from triplet to triplet,
  --like they would in HO. This current example doesn't work yet because
  --of the closure part.
  runProcess node $ do
    (s0,r0) <- newChan :: Process (SendPort (SendPort Int), ReceivePort (SendPort Int))
    (sn,rn) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (sC,rC) <- newChan
    (sfake,rfake) <- newChan :: Process (SendPort Int, ReceivePort Int)

    pid <- spawnLocal $ do
      sendChan sC ( $(mkClosure 'establishChannel) (s0, sn, (RecvInt $ SendInt 4 $ End)) )
      sp <- receiveChan r0
      triple (SendInt 5 $ RecvInt $ End) (sp, rn)
      terminate
    process <- receiveChan rC
    nodeId <- getSelfNode
    pid2 <- spawn nodeId process
    --Is here just so the main process doesn't terminate before the other two.
    a <- receiveChan rfake
    say $ show a

  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable
