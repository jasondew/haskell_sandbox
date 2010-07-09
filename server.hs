import IO
import Network
import Control.Concurrent
import Control.Monad

http :: (Handle, HostName, PortNumber) -> IO ()
http (handle, hostName, portNumber) = do
  putStrLn $ "Connection accepted from " ++ hostName ++ " on port " ++ (show portNumber)
  hPutStrLn handle "HTTP/1.1 200 OK\rContent-Length: 4\rDate: Sun, 14 Mar 2010 04:46:00 EST\rServer: Simple/0.0\r\rHi2U"

server :: Socket -> IO ()
server socket = forever $ do
                  accepted_socket <- Network.accept socket
                  forkIO $ http accepted_socket

main = withSocketsDo $ do
         socket <- listenOn $ PortNumber 4000
         server socket
