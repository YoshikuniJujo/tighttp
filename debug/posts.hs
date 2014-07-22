{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections,
	PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.HandleLike
import Data.Pipe
import System.Environment
import Network
import qualified Network.PeyoTLS.Client as P
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import Network.TigHTTP.Client
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	addr : spn : pth : msgs <- getArgs
	(pn :: Int) <- readIO spn
	ca <- readCertificateStore [
		"cacert.sample_pem",
		"/etc/ssl/certs/GeoTrust_Global_CA.pem" ]
	sv <- flip DebugHandle (Just "low") <$>
		connectTo addr (PortNumber $ fromIntegral pn)
	g <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	_ <- (`P.run` g) $ do
		t <- P.open sv ["TLS_RSA_WITH_AES_128_CBC_SHA"] [] ca
		p <- request t . post addr pn pth . (Nothing ,) .
			LBS.fromChunks $ map BSC.pack msgs
		runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
