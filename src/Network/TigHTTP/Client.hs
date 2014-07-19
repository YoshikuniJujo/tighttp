{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (
	ClientM, run, setHost, httpGet, httpPost,
	ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
	) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
import Data.Pipe.List
import Numeric

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

type ClientM h = StateT (h, Maybe (BS.ByteString, Int)) (HandleMonad h)

run :: HandleLike h => h -> ClientM h a -> HandleMonad h a
run h = (`evalStateT` (h, Nothing))

setHost :: HandleLike h => BS.ByteString -> Int -> ClientM h ()
setHost hn pn = modify $ second $ const $ Just (hn, pn)

httpGet :: HandleLike h => ClientM h BS.ByteString
-- httpGet = (BS.concat . fromJust) `liftM` runPipe (httpGet_ =$= toList)
httpGet = do
	p <- httpGet__
	(BS.concat . fromJust) `liftM` runPipe (p =$= toList)

httpGet_ :: HandleLike h => Pipe () BS.ByteString (ClientM h) ()
httpGet_ = lift (gets fst) >>= \sv -> do
	lift . lift . hlPutStrLn sv . request =<< lift (gets snd)
	src <- lift . lift $ hGetHeader sv
	let res = parseResponse src
	lift . lift . mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
		. catMaybes $ showResponse res
	httpContent_ $ contentLength <$> responseContentLength res

httpGet__ :: HandleLike h => ClientM h (Pipe () BS.ByteString (ClientM h) ())
httpGet__ = gets fst >>= \sv -> do
	lift . hlPutStrLn sv . request =<< gets snd
	src <- lift $ hGetHeader sv
	let res = parseResponse src
	lift . mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
		. catMaybes $ showResponse res
	return (httpContent_ $ contentLength <$> responseContentLength res)

httpContent :: HandleLike h => Maybe Int -> ClientM h BS.ByteString
httpContent n = (BS.concat . fromJust) `liftM` runPipe (httpContent_ n =$= toList)

httpContent_ :: HandleLike h => Maybe Int -> Pipe () BS.ByteString (ClientM h) ()
httpContent_ (Just n) = lift (gets fst) >>= \h -> yield =<< lift (lift $ hlGet h n)
httpContent_ _ = getChunked

getChunked :: HandleLike h => Pipe () BS.ByteString (ClientM h) ()
getChunked = lift (gets fst) >>= \h -> do
	(n :: Int) <- lift . lift $
		(fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	lift . lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	r <- lift . lift $ hlGet h n
			"" <- lift . lift $ hlGetLine h
			yield r
			getChunked

httpPost :: HandleLike h => LBS.ByteString -> ClientM h (ContentType, BS.ByteString)
httpPost cnt = gets fst >>= \sv -> do
	let pst = mkChunked $ LBS.toChunks cnt
	lift . hlPutStrLn sv . requestToString . flip post pst =<< gets snd
	res <- lift $ parseResponse `liftM` hGetHeader sv
	cnt' <- httpContent $ contentLength <$> responseContentLength res
	let res' = res { responseBody = cnt' }
	lift . hlDebug sv "critical" . (`BS.append` "\n") $ responseToString res'
	return (responseContentType res', responseBody res')

mkChunked :: [BS.ByteString] -> BS.ByteString
mkChunked [] = "0" `BS.append` "\r\n\r\n"
mkChunked (b : bs) = BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
	`BS.append` b `BS.append` "\r\n" `BS.append` mkChunked bs

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	if (BS.null l) then return [] else (l :) `liftM` hGetHeader h

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (+++ "\r\n")

request :: Maybe (BS.ByteString, Int) -> BS.ByteString
request hnpn = crlf . catMaybes . showRequest . RequestGet (Uri "/") (Version 1 1) $
	Get {
		getHost = uncurry Host . second Just <$> hnpn,
		getUserAgent = Just [Product "Mozilla" (Just "5.0")],
		getAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		getAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		getAcceptEncoding = Just [],
		getConnection = Just [Connection "keep-alive"],
		getCacheControl = Just [MaxAge 0],
		getOthers = []
	 }

requestToString :: Request -> BS.ByteString
requestToString = crlf . catMaybes . showRequest

responseToString :: Response -> BS.ByteString
responseToString = crlf . catMaybes . showResponse

post :: Maybe (BS.ByteString, Int) -> BS.ByteString -> Request
post hnpn cnt = RequestPost (Uri "/") (Version 1 1) $
	Post {
		postHost = uncurry Host . second Just <$> hnpn,
		postUserAgent = Just [Product "Mozilla" (Just "5.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType Text Plain [],
		postContentLength = Nothing, --Just . ContentLength $ BS.length cnt,
		postTransferEncoding = Just Chunked,
		postOthers = [],
		postBody = cnt
	 }
