module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment

main :: IO ()
main = do
  hn <- head <$> getArgs
  sock <- clientSocket hn 9042
  print "Connected."

clientSocket :: String -> Int -> IO Socket
clientSocket hn port = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just hn) (Just $ show port)
  sock <-  socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  pure sock


  
