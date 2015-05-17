import SharedCode
import Control.Monad
import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.Async (race_)
import Control.Exception.Base


main = do 
    putStrLn "Client v0.0.3"
    putStrLn ""

    putStrLn "Please enter a username: "
    username <- getLine

    withSocketsDo $ do
        handle <- connectTo "localhost" (PortNumber 4000)
        mainLoop handle username `finally` hClose handle


mainLoop :: Handle -> String -> IO ()
mainLoop handle username = do
    _ <- race_ fromServer toServer

    putStrLn "Race is done"
    return ()

    where
        fromServer = forever $ do
            line <- hGetLine handle
            putStrLn $ "Server: " ++ line

        toServer = do
            let chatProtocol = ChatProtocol username

            line <- getLine
            case line of
                ":help" -> do 
                    putStrLn "Write :quit to exit"
                    toServer
                ":quit" -> hPutStrLn handle $ show $ chatProtocol line
                _       -> do
                    hPutStrLn handle $ show $ chatProtocol line
                    toServer