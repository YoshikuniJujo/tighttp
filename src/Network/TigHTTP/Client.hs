{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (request, get, post) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State (lift, MonadTrans)
import Data.Pipe
import Data.Pipe.List
import Numeric

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

request, httpGet :: (
	PipeClass p, MonadTrans (p () BS.ByteString),
	Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h ) =>
	h -> Request h -> HandleMonad h (Response p h)
request = httpGet
httpGet sv req = do
	putRequest sv req
	src <- hGetHeader sv
	let res = parseResponse src
--	mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
--		. catMaybes =<< showResponse sv res
	let res' = putResponseBody sv res
		(httpContent (contentLength <$> responseContentLength res) sv)
	return res'

get :: HostName -> Int -> FilePath -> Request h
get hn pn fp = RequestGet (Path $ BSC.pack fp) (Version 1 1)
	Get {
		getHost = uncurry Host . second Just <$> Just (BSC.pack hn, pn),
		getUserAgent = Just [Product "tighttp" (Just "0.0.0.0")],
		getAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		getAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		getAcceptEncoding = Just [],
		getConnection = Just [Connection "keep-alive"],
		getCacheControl = Just [MaxAge 0],
		getOthers = []
	 }

putResponseBody :: (PipeClass p, HandleLike h) =>
	h -> Response p h -> p () BS.ByteString (HandleMonad h) () -> Response p h
putResponseBody _ res rb = res { responseBody = rb }

httpContent :: (
	PipeClass p, MonadTrans (p () BS.ByteString),
	Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h ) =>
	Maybe Int -> h -> p () BS.ByteString (HandleMonad h) ()
httpContent (Just n) h = yield =<< lift (hlGet h n)
httpContent _ h = getChunked h `onBreak` readRest h

getChunked :: (
	PipeClass p, MonadTrans (p () BS.ByteString),
	Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h ) =>
	h -> p () BS.ByteString (HandleMonad h) ()
getChunked h = do
	(n :: Int) <- lift $ (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	lift . hlDebug h "medium" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> do	l <- lift $ hlGetLine h
			lift . hlDebug h "medium" . BSC.pack . (++ "\n") $ show l
			return ()
		_ -> do	r <- lift $ hlGet h n
			"" <- lift $ hlGetLine h
			yield r
			getChunked h

readRest :: HandleLike h => h -> HandleMonad h ()
readRest h = do
	hlDebug h "low" "begin readRest\n"
	(n :: Int) <- (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	hlDebug h "medium" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	_ <- hlGet h n
			"" <- hlGetLine h
			readRest h

post :: HandleLike h =>
	HostName -> Int -> FilePath -> (Maybe Int, LBS.ByteString) -> Request h
post hn pn fp (len, pst) = RequestPost (Path $ BSC.pack fp) (Version 1 1)
	Post {
		postHost = uncurry Host . second Just <$> hnpn,
		postUserAgent = Just [Product "tighttp" (Just "0.0.0.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType Text Plain [],
		postContentLength = cl,
		postTransferEncoding = ch,
		postOthers = [],
		postBody = fromList cnt
	 }
	where
	hnpn = Just (BSC.pack hn, pn)
	(cl, ch, cnt) = case len of
		Just l -> (Just $ ContentLength l, Nothing, LBS.toChunks pst)
		_ -> (Nothing, Just Chunked,
			LBS.toChunks . mkChunked $ LBS.toChunks pst)

mkChunked :: [BS.ByteString] -> LBS.ByteString
mkChunked = flip foldr ("0\r\n\r\n") $ \b ->
	LBS.append (LBSC.pack (showHex (BS.length b) "") `LBS.append` "\r\n"
		`LBS.append` LBS.fromStrict b `LBS.append` "\r\n")

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	hlDebug h "medium" $ l `BS.append` "\n"
	if BS.null l then return [] else (l :) `liftM` hGetHeader h
