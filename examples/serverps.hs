{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import Control.Concurrent
import Data.HandleLike
import Data.Pipe
import System.IO
import System.Environment
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import Network.TigHTTP.Server
import Network.TigHTTP.Types
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	as <- getArgs
	k <- readKey "localhost.sample_key"
	c <- readCertificateChain ["localhost.sample_crt"]
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	soc <- listenOn $ PortNumber 443
	void . (`runStateT` g0) . forever $ do
		(h, _, _) <- liftIO $ accept soc
		g <- StateT $ return . cprgFork
		void . liftIO . forkIO . (`run` g) $ do
			t <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"] [(k, c)]
				Nothing
			r <- getRequest t
			liftIO . print $ requestPath r
			void . runPipe $
				requestBody r =$= (printP `finally` liftIO (putStrLn ""))
			putResponse t
				. (response :: LBS.ByteString ->
					Response Pipe (TlsHandle Handle SystemRNG))
				. LBS.fromChunks $ map BSC.pack as
			hlClose t

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
