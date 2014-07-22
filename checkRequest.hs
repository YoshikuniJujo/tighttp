{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Concurrent
import System.IO
import System.Environment
import Network

main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	let port = PortNumber $ fromIntegral pn
	socket <- listenOn port
	forever $ do
		(cl, _, _) <- accept socket
		_ <- forkIO $ do
			ret <- toEmpty cl
			mapM_ putStrLn ret
			print =<< getChars cl 24
		return ()

toEmpty :: Handle -> IO [String]
toEmpty h = do
	l <- hGetLine h
	if l == "\r" then return [] else (l :) <$> toEmpty h

getChars :: Handle -> Int -> IO String
getChars _ 0 = return ""
getChars h n = (:) <$> hGetChar h <*> getChars h (n - 1)
