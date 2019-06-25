import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

main :: IO ()
main = do
  [host, port] <- getArgs
  Right transport <- createTransport host port (\port -> ("localhost", port)) defaultTCPParameters
  node <- newLocalNode transport initRemoteTable 
  runProcess node $ do
    (sp,rp) <- newChan :: Process (SendPort (Int -> Int), ReceivePort (Int -> Int))
    say $ show "hello1"
    forkPid <- spawnLocal $ do
      id <- getSelfPid
      say $ show "helloA"
      say $ show "helloA2"   
      return ()
    say $ show "hello2"
    fork2Pid <- spawnLocal $ do
      id <- getSelfPid
      say $ show "helloB"
      say $ show "helloB2"
      return () 
    say $ show "hello3"

    return ()
  return ()