{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Data.Pipe
import System.Environment
import Network
import qualified Network.PeyoTLS.Client as P
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Network.TigHTTP.Client

import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem" ]
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	_ <- (`P.run` g) $ do
		t <- P.open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		p <- run t $ do
			setHost (BSC.pack addr) pn
			httpPost "I am client.\n"
		runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
