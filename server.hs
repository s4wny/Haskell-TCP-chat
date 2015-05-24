import SharedCode
import Network
import System.IO
import System.IO.Error
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import System.Exit
import Control.Applicative
import Control.Monad (forever)

main = do
    putStrLn "Server v0.0.3"
        
    -- GHC crashes if you use `newChan` or `dupChan` inside `withSocketsDo`.
    -- Even though the documentation says you need `withSocketsDo` on Windows
    -- the code seems to work, at least on my two Windows machines (I'm using
    -- msysgit). That's why I have omitted the `withSocketsDo` part.

    chan <- newChan 
    sock <- listenOn $ PortNumber 4000

    handleNewClients sock chan


handleNewClients :: Socket -> Chan ChatProtocol -> IO ()
handleNewClients sock chan = forever $ do
    -- This is a blocking call
    (handle, hostname, port) <- accept sock
    logger $ "'" ++ hostname ++ ":"++ show port ++"' connected to the server"

    forkIO $ clientConnected handle chan `catchIOError` clientDisconnected


clientDisconnected :: IOException -> IO ()
clientDisconnected e = do
    putStrLn $ "Client disconnected, ("++ show e ++")"


clientConnected :: Handle -> Chan ChatProtocol -> IO ()
clientConnected handle chan = do
    hPutStrLn handle welcomeMsg

    chan' <- dupChan chan

    forkIO $ broadcastToClient handle chan'
    forkIO $ whenClientWrite handle chan

    return ()


whenClientWrite :: Handle -> Chan ChatProtocol -> IO ()
whenClientWrite handle chan = forever $ do
    -- Blocking call, waits for client to write something
    userInput <- (\x -> read x :: ChatProtocol) <$> hGetLine handle

    writeChan chan userInput

    logger $ username userInput ++": "++ message userInput


broadcastToClient :: Handle -> Chan ChatProtocol -> IO ()
broadcastToClient handle chan = forever $ do
    -- Blocking call, waits for new messages in chan
    clientData <- readChan chan

    hPutStrLn handle $ username clientData ++": "++ message clientData


welcomeMsg :: String
welcomeMsg = "Hi!"


simpleBot :: String -> String
simpleBot str =
    case str' of
        "hi"      -> "Hello!"
        "wazup"   -> "The sky of course, what else would be up?"
        "-.-"     -> ":D"
        ":)"      -> str
        ":("      -> str
        "help"    -> "Ask google, google knows everything"
        "bai"     -> "Goodbye"
        _         -> "I agree!"

    where
        str' = strToLower str
