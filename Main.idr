module Main

import Data.Maybe
import Data.Either
import Network.Socket

mapRightIO : (Either a b) -> (b -> IO ()) -> IO ()
mapRightIO x f = do
    case x of
        Left a => pure()
        Right b => f b
        
data SocketState = Bind | Listen | Accept | Recv | Send

record ServerState where
  constructor MkServerState
  port : Port
  sock : Socket
  client : Maybe Socket

update : SocketState -> ServerState -> IO ()

update Bind srv = do
    res <- bind srv.sock (Just (Hostname "0.0.0.0")) srv.port
    if res /= 0
        then putStrLn ("Failed to bind socket with error: " ++ show res)
        else do update Listen srv

update (Listen) srv = do
    res <- listen srv.sock
    if res /= 0
        then putStrLn ("Failed to listen on socket with error: " ++ show res)
        else do 
            putStrLn ("server running on: " ++ (show srv.port))
            update Accept srv
            putStrLn ("killing process ")

update Accept srv = do
    res <- accept srv.sock
    mapRightIO res (\(soc,addr) => do
        update Recv (MkServerState srv.port srv.sock (Just soc)))

update Recv srv = do
    case srv.client of
        Nothing => pure()
        Just c => do
    res <- recv c 500
    mapRightIO res (\(msgstr,resultcode) => do 
        putStrLn ("request: " ++ msgstr)
        update Send srv)
        
update Send srv = do
    case srv.client of
        Nothing => pure()
        Just c => do
    res <- send c "HTTP/1.1 200 OK\r\nContent: text/plain\r\n\r\nbody"
    mapRightIO res (\(resultcode) => do 
        close c 
        update Accept srv
        )


startServer : Int -> IO()
startServer p = do
    osock <- socket AF_INET Stream 0
    case osock of 
        Left sockError => pure()
        Right sock => do
            
    update Bind (MkServerState p sock Nothing)


main : IO ()
main = do
    startServer 5000
