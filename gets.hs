{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import System.Environment
import Network
import qualified Network.PeyoTLS.Client as P
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Client


main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem" ]
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`P.run` g) $ do
		t <- P.open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		httpGet t >>= liftIO . print
