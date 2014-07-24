{-# LANGUAGE OverloadedStrings, TupleSections, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import System.Environment
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import Network.TigHTTP.Client
import Network.TigHTTP.Types
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	addr : pth : msgs <- getArgs
	ca <- readCertificateStore ["cacert.sample_pem"]
	h <- connectTo addr $ PortNumber 443
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	void . (`run` g) $ do
		t <- open h ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		r <- request t . post addr 443 pth . (Nothing ,) .
			LBS.fromChunks $ map BSC.pack msgs
		runPipe $ responseBody r =$= printP

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
