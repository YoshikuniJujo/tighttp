{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent
import Data.Maybe
import Data.Pipe
import Data.Pipe.List
import System.Environment
import Network

import Data.HandleLike

import Network.PeyoTLS.ReadFile
import Network.PeyoTLS.Server
import Network.TigHTTP.Server
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS

main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	let port = PortNumber $ fromIntegral pn
	k <- readKey "localhost.sample_key"
	c <- readCertificateChain ["localhost.sample_crt"]
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	socket <- listenOn port
	void . (`runStateT` g0) . forever $ do
		(client, _, _) <- liftIO $ accept socket
		g <- StateT $ return . cprgFork
		_ <- liftIO . forkIO . (`run` g) $ do
			cl <- open client ["TLS_RSA_WITH_AES_128_CBC_SHA"] [(k, c)]
				Nothing
			ret <- getRequest cl
			putResponse cl "Good afternoon, world!\n"
			bs <- (BS.concat . fromJust) `liftM` runPipe
				(requestBody ret =$= toList)
			hlDebug cl "critical" bs
			hlClose cl
		return ()
