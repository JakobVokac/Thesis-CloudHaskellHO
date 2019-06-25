{-# LANGUAGE TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)

isPrime :: Integer -> Process Bool
isPrime n = return . (n `elem`) . takeWhile (<= n) . sieve $ [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]

remotable ['isPrime]

master :: [NodeId] -> Process ()
master [] = liftIO $ putStrLn "no slaves"
master (slave:_) = do
  isPrime79 <- call $(functionTDict 'isPrime) slave ($(mkClosure 'isPrime) (79 :: Integer))
  liftIO $ print isPrime79

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend master
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
  where
    rtable :: RemoteTable
    rtable = __remoteTable initRemoteTable