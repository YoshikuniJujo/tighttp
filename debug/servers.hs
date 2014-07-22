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
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	let port = PortNumber $ fromIntegral pn
	k <- readKey "localhost.sample_key"
	c <- readCertificateChain ["localhost.sample_crt"]
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	socket <- listenOn port
	void . (`runStateT` g0) . forever $ do
		(client_, _, _) <- liftIO $ accept socket
		let client = DebugHandle client_ (Just "low")
		g <- StateT $ return . cprgFork
		_ <- liftIO . forkIO . (`run` g) $ do
			cl <- open client ["TLS_RSA_WITH_AES_128_CBC_SHA"] [(k, c)]
				Nothing
			ret <- getRequest cl
			putResponse cl $ response "Good afternoon, world!\n"
			hlDebug cl "critical" . BSC.pack . (++ "\n") . show $
				requestPath ret
			bs <- (BS.concat . fromJust) `liftM` runPipe
				(requestBody ret =$= toList)
			hlDebug cl "critical" $ bs `BS.append` "\n"
			hlClose cl
		return ()
