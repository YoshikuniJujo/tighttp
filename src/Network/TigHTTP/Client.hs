{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (
	ClientM, run, setHost, httpGet, httpPost,
	ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
	Response(..),
	) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Maybe
import Data.Pipe
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
setHost hn pn = modify . second . const $ Just (hn, pn)

httpGet :: HandleLike h => ClientM h (Response h)
httpGet = gets fst >>= \sv -> do
	lift . hlPutStrLn sv . request =<< gets snd
	src <- lift $ hGetHeader sv
	let res = parseResponse src
	lift $ mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
		. catMaybes =<< showResponse sv res
	let res' = putResponseBody sv res
		(httpContent (contentLength <$> responseContentLength res) sv)
	return res'

putResponseBody :: HandleLike h =>
	h -> Response h -> Pipe () BS.ByteString (HandleMonad h) () -> Response h
putResponseBody _ res rb = res { responseBody = rb }

httpContent :: HandleLike h =>
	Maybe Int -> h -> Pipe () BS.ByteString (HandleMonad h) ()
httpContent (Just n) h = yield =<< lift (hlGet h n)
httpContent _ h = getChunked h `onBreak` readRest h

getChunked :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
getChunked h = do
	(n :: Int) <- lift $ (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	r <- lift $ hlGet h n
			"" <- lift $ hlGetLine h
			yield r
			getChunked h

readRest :: HandleLike h => h -> HandleMonad h ()
readRest h = do
	hlDebug h "critical" "begin readRest\n"
	(n :: Int) <- (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	_ <- hlGet h n
			"" <- hlGetLine h
			readRest h

httpPost :: HandleLike h => LBS.ByteString -> ClientM h (Response h)
httpPost cnt = gets fst >>= \sv -> do
	let pst = mkChunked $ LBS.toChunks cnt
	lift . hlPutStrLn sv . requestToString . flip post pst =<< gets snd
	res <- lift $ parseResponse `liftM` hGetHeader sv
	lift $ hlDebug sv "critical" =<< responseToString sv res
	let res' = putResponseBody sv res $
		httpContent (contentLength <$> responseContentLength res) sv
	return res'

mkChunked :: [BS.ByteString] -> BS.ByteString
mkChunked = flip foldr ("0" `BS.append` "\r\n\r\n") $ \b ->
	BS.append (BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
		`BS.append` b `BS.append` "\r\n")

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	if BS.null l then return [] else (l :) `liftM` hGetHeader h

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

responseToString :: HandleLike h => h -> Response h -> HandleMonad h BS.ByteString
responseToString h c = (crlf . catMaybes) `liftM` showResponse h c

post :: Maybe (BS.ByteString, Int) -> BS.ByteString -> Request
post hnpn cnt = RequestPost (Uri "/") (Version 1 1)
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
