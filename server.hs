{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.Environment
import Network

import Server

main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	let port = PortNumber $ fromIntegral pn
	socket <- listenOn port
	forever $ do
		(client, _, _) <- accept socket
		_ <- forkIO $ do
			ret <- httpServer client "Good afternoon, world!\n"
			print ret
		return ()
