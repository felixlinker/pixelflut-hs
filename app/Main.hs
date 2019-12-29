module Main where

import           Lib
import           Control.Monad
import           System.Environment
import           Network.Socket
import           Control.Exception
import qualified Data.ByteString               as B
import           Network.Socket.ByteString     as BSocket

onSocket :: String -> String -> (Socket -> IO a) -> IO ()
onSocket ip p callback = withSocketsDo $ do
    addr   <- head <$> getAddrInfo (Just defaultHints) (Just ip) (Just p)
    socket <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption socket NoDelay 1
    connect socket $ addrAddress addr
    callback socket
    close socket

flut :: String -> String -> Int -> Int -> B.ByteString -> IO ()
flut ip p dx dy imgBs =
    let ps = payloads dx dy imgBs
    in  onSocket ip p $ \s -> do
            putStrLn "sending"
            mapM (BSocket.sendAll s) ps

main :: IO ()
main = do
    file : ip : port : dx : dy : _ <- getArgs
    imgBs                          <- B.readFile file
    flut ip port (read dx) (read dy) imgBs
