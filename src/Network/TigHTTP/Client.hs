{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (
	run, setHost, httpGet, httpGet', httpPost) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State
import Data.Maybe

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS

type ClientM h = StateT (h, Maybe (BS.ByteString, Int)) (HandleMonad h)

httpGet' :: HandleLike h => h -> HandleMonad h BS.ByteString
httpGet' sv = httpGet `evalStateT` (sv, Nothing)

run :: HandleLike h => h -> ClientM h a -> HandleMonad h a
run h = (`evalStateT` (h, Nothing))

setHost :: HandleLike h => BS.ByteString -> Int -> ClientM h ()
setHost hn pn = modify $ second $ const $ Just (hn, pn)

httpGet :: HandleLike h => ClientM h BS.ByteString
httpGet = gets fst >>= \sv -> do
--	modify . second $ const ("www.google.co.jp", 443)
	lift . hlPutStrLn sv . request =<< gets snd
	src <- lift $ hGetHeader sv
	let res = parseResponse src
	cnt <- lift $ hlGet sv $ maybe 10 contentLength $ responseContentLength res
	let res' = res { responseBody = cnt }
	lift . mapM_ (hlDebug sv "critical" . (`BS.append` "\n"))
		. catMaybes $ showResponse res'
	return cnt

httpPost :: HandleLike h => h -> BS.ByteString -> HandleMonad h BS.ByteString
httpPost sv cnt = do
	hlPutStrLn sv . requestToString $ post cnt
	res <- parseResponse `liftM` hGetHeader sv
	cnt' <- hlGet sv $ maybe 10 contentLength $ responseContentLength res
	let res' = res { responseBody = cnt' }
	hlDebug sv "critical" . (`BS.append` "\n") $ responseToString res'
	return cnt'

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
--		getHost = Just . Host "www.google.co.jp" $ Just 443,
--		getHost = Just . Host "www.facebook.com" $ Just 443,
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

post :: BS.ByteString -> Request
post cnt = RequestPost (Uri "/") (Version 1 1) $
	Post {
		postHost = Just . Host "www.google.co.jp" $ Just 443,
		postUserAgent = Just [Product "Mozilla" (Just "5.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType ("text", "plain"),
		postContentLength = Just . ContentLength $ BS.length cnt,
		postOthers = [],
		postBody = cnt
	 }
