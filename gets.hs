{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import System.Environment
import Network
import Network.PeyoTLS.Client
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Client


main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	ca <- readCertificateStore ["cacert.pem"]
	sv <- connectTo "localhost" (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	(`run` g) $ do
		t <- open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		httpGet t >>= liftIO . print
