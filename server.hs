import IO
import Network
import Control.Concurrent
import Control.Monad

http :: (Handle, HostName, PortNumber) -> IO ()
http (handle, hostName, portNumber) = do
  putStrLn $ "Connection accepted from " ++ hostName ++ " on port " ++ (show portNumber)
  hPutStrLn handle "connection accepted, kthxbye!"

server :: Socket -> IO ()
server socket = forever $ do
                  accepted_socket <- Network.accept socket
                  forkIO $ http accepted_socket

main = withSocketsDo $ do
         socket <- listenOn $ PortNumber 4000
         server socket
