import SharedCode
import Network
import System.IO
import System.IO.Error
import Control.Concurrent
import Control.Exception
import System.Exit
import Control.Applicative
import Control.Monad (forever)

main = do
    putStrLn "Server v0.0.2"

    withSocketsDo $ do
        sock <- listenOn $ PortNumber 4000

        mainLoop sock


mainLoop :: Socket -> IO ()
mainLoop sock = forever $ do
    -- This is a blocking call
    (handle, n, p) <- accept sock
    print n
    print p

    forkIO $ clientConnected handle `catchIOError` clientDisconnect

    putStrLn "MainLoop()"


clientDisconnect :: IOException -> IO ()
clientDisconnect e = do
    putStrLn $ "Client disconnected, ("++ show e ++")"


clientConnected :: Handle -> IO ()
clientConnected handle = do
    putStrLn $ "Client connected"

    hPutStrLn handle welcomeMsg

    clientHandler handle


clientHandler :: Handle -> IO ()
clientHandler handle = do
    userInput <- (\x -> read x :: ChatProtocol) <$> hGetLine handle

    putStrLn $ username userInput ++": "++ message userInput
    hPutStrLn handle $ simpleBot $ message userInput

    clientHandler handle


welcomeMsg :: String
welcomeMsg = "Hi!"


simpleBot :: String -> String
simpleBot str =
    case str of
        "hi"      -> "Hello!"
        "wazup"   -> "The sky of course, what else would be up?"
        "-.-"     -> ":D"
        ":)"      -> str
        ":("      -> str
        "help"    -> "Ask google, google knows everything"
        "bai"     -> "Goodbye"
        _         -> "I agree!"