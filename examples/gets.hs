{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Trans
import System.Environment
import Data.Pipe
import Network
import Network.TigHTTP.Client
import Network.TigHTTP.Types
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Network.PeyoTLS.Client as P

main :: IO ()
main = do
	addr : pth : _ <- getArgs
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem",
		"/etc/ssl/certs/DigiCert_High_Assurance_EV_Root_CA.pem",
		"/etc/ssl/certs/GlobalSign_Root_CA.pem" ]
	h <- connectTo addr $ PortNumber 443
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`P.run` g) $ do
		t <- P.open' h addr ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		P.getNames t >>= liftIO . print
		p <- request t $ get addr 443 pth
		void . runPipe $
			responseBody p =$= (printP `finally` liftIO (putStrLn ""))

printP :: MonadIO m => Pipe BS.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BS.putStr s) >> printP)
