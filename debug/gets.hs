{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Data.HandleLike
import Data.Pipe
import System.Environment
import Network
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import qualified Data.ByteString.Char8 as BSC
import qualified Network.PeyoTLS.Client as P

import Network.TigHTTP.Client
import Network.TigHTTP.Types

main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem",
		"/etc/ssl/certs/DigiCert_High_Assurance_EV_Root_CA.pem",
		"/etc/ssl/certs/GlobalSign_Root_CA.pem" ]
	sv <- flip DebugHandle (Just "low") <$>
		connectTo addr (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`P.run` g) $ do
		t <- P.open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		p <- request t $ get addr 443 "/"
		_ <- runPipe $ responseBody p =$= takeP 1 =$= printP
		return ()

takeP :: Monad m => Int -> Pipe a a m ()
takeP 0 = return ()
takeP n = do
	mx <- await
	case mx of
		Just x -> yield x >> takeP (n - 1)
		_ -> return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = do
	ms <- await
	case ms of
		Just s -> liftIO (BSC.putStrLn (BSC.take 100 s)) >> printP
		_ -> return ()
