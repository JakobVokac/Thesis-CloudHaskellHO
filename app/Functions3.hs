{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Closure
import Network.Transport.TCP (createTransport, defaultTCPParameters)

-- HO Process
-- P = (?Int:?Int;!Int;End)
{-
    n1?(x).n2?(y).n3!(x+y).0|n1!<3>.n2!<5>.n3?(z).0
-}

-- Name of a variable
type Name = String

--Environment for variables
type Env = [(Name, Int)]

--Session type structure (quite messy at the moment, but I wanted to
--focus primarily on getting the example to work)
data ST = SendInt (SendPort Int) Name ST |
          RecvInt (ReceivePort Int) Name ST |
          SendBool (SendPort Bool) Name ST |
          RecvBool (ReceivePort Bool) Name ST |
          UseProcess String [Name] ST |
          End

--Process for accessing environment variables
useInEnvProc :: Env -> Name -> Process (Int, Env)
useInEnvProc env m = do return $ (findInEnv env m, removeInEnv env m)

findInEnv :: Env -> Name -> Int
findInEnv ((n,v):env) m 
  | m == n = v
  | otherwise = findInEnv env m
findInEnv [] m = 0

removeInEnv :: Env -> Name -> Env
removeInEnv ((n,v):env) m 
  | m == n = env
  | otherwise = (n,v):(removeInEnv env m)
removeInEnv [] m = []


--Process for adding up two numbers from the environment
----I wanted to create a closure for this, since then it could be more 
----concisely described in the ST definition, but I haven't gotten it
----to work yet.
addTwoNums :: Env -> [Name] -> Process (Env)
addTwoNums env (n:m:[]) = do 
  (a, env2) <- useInEnvProc env n
  (b, env3) <- useInEnvProc env2 m
  return (("c",a+b):env3)

--Current implementation of triplets. They take in the session type and
--environment, execute the current triplet and spawn the next. Right now
--the ones for sending and receiving work. But the UseProcess one doesn't yet.
triple :: ST -> Env -> Process ()
triple (SendInt sp n st) env = do
  (v, newEnv) <- useInEnvProc env n
  sendChan sp v
  say $ "Sent value: " ++ show v
  _ <- spawnLocal $ do triple st newEnv
  terminate

triple (RecvInt rp n st) env = do
  v <- receiveChan rp
  say $ "Received value: " ++ show v
  _ <- spawnLocal $ do triple st ((n,v):env)
  terminate

triple (End) [] = do
  say $ "End"

--(Fixed version without the closure part!)
--In the works. I get the following error, when I try to make a bytestring
--in main and decode it here. 
{-
 Not in scope: ‘addTwoNums__tdict’
    Perhaps you meant one of these:
      ‘addTwoNums__sdict’ (line 67), ‘addTwoNums__static’ (line 67)
    In the splice: $(functionTDict 'addTwoNums)
-}
triple (UseProcess n args st) env = do
  env2 <- addTwoNums env args
  _ <- spawnLocal $ do triple st env2
  terminate
  
main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "8092"  (\port' -> ("127.0.0.1" , port')) defaultTCPParameters
  node <- newLocalNode transport rtable

  --The idea is that the channels are created and the variables are saved
  --in the environment, so they can be used passed from triplet to triplet,
  --like they would in HO. This current example doesn't work yet because
  --of the closure part.
  runProcess node $ do
    (s1,r1) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (s2,r2) <- newChan :: Process (SendPort Int, ReceivePort Int)
    (s3,r3) <- newChan :: Process (SendPort Int, ReceivePort Int)

    pid1 <- spawnLocal $ do
      triple (SendInt s1 "a" $ SendInt s2 "b" $ RecvInt r3 "c" $ End) ([("a",3),("b",5)])

    pid2 <- spawnLocal $ do
      triple (RecvInt r1 "a" $ RecvInt r2 "b" $ UseProcess "addTwoNums" (["a","b"]) $ SendInt s3 "c" $ End) ([])

    --Is here just so the main process doesn't terminate before the other two.
    a <- receiveChan r1
    say $ show a

  where
    rtable :: RemoteTable
    rtable = initRemoteTable
