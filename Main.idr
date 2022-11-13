module Main


import Network
import Data.List
import Data.String
import System
import System.File
import Network.Socket
import Network.Socket.Data

infixl 5 |>
(|>) : a -> (a -> b) -> b
a |> f = f a

mapRight : (b -> c) -> (Either SocketError b) -> (Either SocketError c)
mapRight fn res = case res of
    Left err => Left err
    Right item => Right (fn item)

data ServerState = Begin | Accept | Recv | End


resolveSocket : ServerState -> Socket -> IO (ServerState,Socket)
resolveSocket Begin serverSock = do
    osock <- accept serverSock
    case osock of 
        Left err => pure (End, serverSock)
        Right (clientSock,addr) => do
            putStrLn ("accepted")
            resolveSocket Accept (clientSock)
resolveSocket Accept clientSock = do
    res <- recv clientSock 500
    case res of 
        Left err => pure (End, clientSock)
        Right (msgstr,resultcode) => do
            putStrLn ("request: " ++ msgstr)
            resolveSocket Recv clientSock
resolveSocket Recv sock = do
    res <- send sock "HTTP/1.1 200 OK\r\nContent: text/plain\r\n\r\nbody"
    case res of 
        Left err => pure (End, sock)
        Right (resultcode) => do
            putStrLn ("sent result:" ++ show resultcode)
            resolveSocket End sock
resolveSocket End sock = do
    -- close sock
    pure (Begin, sock)

serverLoop : ServerState -> (Either SocketError Socket) -> IO()
serverLoop state osock = do
    
    putStrLn "waiting for request"
    case osock of 
        Left error => putStrLn "error!!"
        Right sock => do
            (endstate, clientSock) <- resolveSocket state sock
            close clientSock
            close sock

                
            


startServer : Int -> IO()
startServer p = do
    osock <- socket AF_INET Stream 0
    case osock of 
        Left sockError => putStrLn "failed"
        Right sock => do
            res <- bind sock (Just (Hostname "0.0.0.0")) p
            putStrLn "bound to 0.0.0.0"
            if res /= 0
                then putStrLn ("Failed to bind socket with error: " ++ show res)
                else do
                    res <- listen sock
                    if res /= 0
                        then putStrLn ("Failed to listen on socket with error: " ++ show res)
                        else do
                            putStrLn ("server running on: " ++ (show p))
                            fflush stdout
                            serverLoop Begin (Right sock)

        
                                
    putStrLn "end"


main : IO ()
main = do
    startServer 5000


