module Server (
server,
HandlerFunc
) where
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Network.BSD
import Data.List
import Data.Bits

type HandlerFunc = Socket -> String -> IO ()

server :: String -> HandlerFunc -> IO (Socket)
server port handlerfunc = withSocketsDo $
    do
        addrinfos <- getAddrInfo
                     (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing (Just port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        bindSocket sock (addrAddress serveraddr)
        listen sock 5
        lock <- newMVar ()
        procRequests lock sock
    where
        procRequests :: MVar () -> Socket -> IO (Socket)
        procRequests lock mastersock = do
            (connsock, clientaddr) <- accept mastersock
            handle lock connsock "Client connected"
            forkIO $ procMessages lock connsock clientaddr
            return (connsock)

        procMessages :: MVar () -> Socket -> SockAddr -> IO ()
        procMessages lock connsock clientaddr = do
            connhdl <- socketToHandle connsock ReadMode
            hSetBuffering connhdl LineBuffering
            messages <- hGetContents connhdl
            mapM_ (handle lock connsock) (lines messages)
            hClose connhdl
            handle lock connsock "Client disconnected"

        handle :: MVar () -> HandlerFunc
        handle lock sock msg = withMVar lock (\a -> handlerfunc sock msg >> return a)
