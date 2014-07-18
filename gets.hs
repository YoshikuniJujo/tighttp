{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import System.Environment
import Network
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Char8 as BSC
import qualified Network.PeyoTLS.Client as P

import Network.TigHTTP.Client


main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem",
		"/etc/ssl/certs/DigiCert_High_Assurance_EV_Root_CA.pem" ]
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`P.run` g) $ do
		t <- P.open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		run t $ do
			setHost (BSC.pack addr) 443
			httpGet >>= liftIO . putStrLn . (++ "...") . take 100 . show
