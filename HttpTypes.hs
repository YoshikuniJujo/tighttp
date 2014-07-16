{-# LANGUAGE OverloadedStrings #-}

module HttpTypes (
	Version(..),
	Request(..), RequestType(..), Uri(..), Get(..), CacheControl(..),
	Accept(..), AcceptLanguage(..), Qvalue(..),
	Host(..), Product(..), Connection(..),
	Response(..), StatusCode(..), ContentLength(..), contentLength,
	ContentType(..),

	parse, parseResponse, showRequest, showResponse, (+++),
	myLast, requestBodyLength, postAddBody, getPostBody,

	Post(..),
) where

import Control.Applicative
import Data.Maybe
import Data.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Time
import System.Locale

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

(-:-) :: Char -> BS.ByteString -> BS.ByteString
(-:-) = BSC.cons

data Request
	= RequestGet Uri Version Get
	| RequestPost Uri Version Post
	| RequestRaw RequestType Uri Version [(BS.ByteString, BS.ByteString)]
	deriving Show

requestBodyLength :: Request -> Int
requestBodyLength (RequestPost _ _ p) = contentLength . fromJust $ postContentLength p
requestBodyLength _ = 0

postAddBody :: Request -> BS.ByteString -> Request
postAddBody (RequestPost u v p) b = RequestPost u v $ p { postBody = b }
postAddBody r _ = r

getPostBody :: Request -> BS.ByteString
getPostBody (RequestPost _ _ p) = postBody p
getPostBody _ = ""

showRequest :: Request -> [Maybe BS.ByteString]
showRequest (RequestGet uri vsn g) = [
	Just $ "GET " +++ showUri uri +++ " " +++ showVersion vsn,
	("Host: " +++) . showHost <$> getHost g,
	("User-Agent: " +++) . BSC.unwords . map showProduct <$> getUserAgent g,
	("Accept: " +++) . BSC.intercalate "," . map showAccept <$> getAccept g,
	("Accept-Language: " +++) . BSC.intercalate "," .
		map showAcceptLanguage <$> getAcceptLanguage g,
	("Accept-Encoding: " +++) . BSC.intercalate "," .
		map showAcceptEncoding <$> getAcceptEncoding g,
	("Connection: " +++) . BSC.intercalate "," .
		map showConnection <$> getConnection g,
	("Cache-Control: " +++) . BSC.intercalate "," .
		map showCacheControl <$> getCacheControl g,
	Just ""
 ]
showRequest (RequestPost uri vsn p) = [
	Just $ "POST " +++ showUri uri +++ " " +++ showVersion vsn,
	("Host: " +++) . showHost <$> postHost p,
	("User-Agent: " +++) . BSC.unwords . map showProduct <$> postUserAgent p,
	("Accept: " +++) . BSC.intercalate "," . map showAccept <$> postAccept p,
	("Accept-Language: " +++) . BSC.intercalate "," .
		map showAcceptLanguage <$> postAcceptLanguage p,
	("Accept-Encoding: " +++) . BSC.intercalate "," .
		map showAcceptEncoding <$> postAcceptEncoding p,
	("Connection: " +++) . BSC.intercalate "," .
		map showConnection <$> postConnection p,
	("Cache-Control: " +++) . BSC.intercalate "," .
		map showCacheControl <$> postCacheControl p,
	("Content-Type: " +++) .  showContentType <$> postContentType p,
	("Content-Length: " +++) .  showContentLength <$> postContentLength p
 ] ++ map (\(k, v) -> Just $ k +++ ": " +++ v) (postOthers p) ++ [
 	Just "",
	Just $ postBody p
 ]
showRequest (RequestRaw rt uri vsn kvs) = [
	Just $ showRequestType rt +++ " " +++ showUri uri +++ " " +++ showVersion vsn
 ] ++ map (\(k, v) -> Just $ k +++ ": " +++ v) kvs ++ [ Just "" ]

data Get = Get {
	getHost :: Maybe Host,
	getUserAgent :: Maybe [Product],
	getAccept :: Maybe [Accept],
	getAcceptLanguage :: Maybe [AcceptLanguage],
	getAcceptEncoding :: Maybe [AcceptEncoding],
	getConnection :: Maybe [Connection],
	getCacheControl :: Maybe [CacheControl],
	getOthers :: [(BS.ByteString, BS.ByteString)]
 } deriving Show

data Post = Post {
	postHost :: Maybe Host,
	postUserAgent :: Maybe [Product],
	postAccept :: Maybe [Accept],
	postAcceptLanguage :: Maybe [AcceptLanguage],
	postAcceptEncoding :: Maybe [AcceptEncoding],
	postConnection :: Maybe [Connection],
	postCacheControl :: Maybe [CacheControl],
	postContentType :: Maybe ContentType,
	postContentLength :: Maybe ContentLength,
	postOthers :: [(BS.ByteString, BS.ByteString)],
	postBody :: BS.ByteString
 } deriving Show

data RequestType
	= RequestTypeGet
	| RequestTypePost
	| RequestTypeRaw BS.ByteString
	deriving Show

showRequestType :: RequestType -> BS.ByteString
showRequestType RequestTypeGet = "GET"
showRequestType RequestTypePost = "POST"
showRequestType (RequestTypeRaw rt) = rt

data Uri = Uri BS.ByteString deriving Show

showUri :: Uri -> BS.ByteString
showUri (Uri uri) = uri

data Version = Version Int Int deriving Show

showVersion :: Version -> BS.ByteString
showVersion (Version vmjr vmnr) =
	"HTTP/" +++ BSC.pack (show vmjr) +++ "." +++ BSC.pack (show vmnr)

parse :: [BS.ByteString] -> Request
parse (h : t) = let
	(rt, uri, v) = parseRequestLine h in
	parseSep rt uri v $ map separate t
	where
	separate i = let (k, csv) = BSC.span (/= ':') i in
		case BS.splitAt 2 csv of
			(": ", v) -> (k, v)
			_ -> error "parse: bad"
parse [] = error "parse: bad request"

parseSep :: RequestType -> Uri -> Version -> [(BS.ByteString, BS.ByteString)] -> Request
parseSep RequestTypeGet uri v kvs = RequestGet uri v $ parseGet kvs
parseSep RequestTypePost uri v kvs = RequestPost uri v $ parsePost kvs
parseSep rt uri v kvs = RequestRaw rt uri v kvs

parseRequestLine :: BS.ByteString -> (RequestType, Uri, Version)
parseRequestLine rl = let
	[rts, uris, vs] = BSC.words rl
	rt = case rts of
		"GET" -> RequestTypeGet
		"POST" -> RequestTypePost
		_ -> RequestTypeRaw rts in
	(rt, Uri uris, parseVersion vs)

parseVersion :: BS.ByteString -> Version
parseVersion httpVns
	| ("HTTP/", vns) <- BS.splitAt 5 httpVns = let
		(vmjrs, dvmnrs) = BSC.span (/= '.') vns in case BSC.uncons dvmnrs of
			Just ('.', vmnrs) -> Version
				(read $ BSC.unpack vmjrs) (read $ BSC.unpack vmnrs)
			_ -> error "parseVersion: bad http version"
parseVersion _ = error "parseVersion: bad http version"

parseGet :: [(BS.ByteString, BS.ByteString)] -> Get
parseGet kvs = Get {
	getHost = parseHost <$> lookup "Host" kvs,
	getUserAgent = map parseProduct . sepTkn <$> lookup "User-Agent" kvs,
	getAccept = map parseAccept . unlist <$> lookup "Accept" kvs,
	getAcceptLanguage =
		map parseAcceptLanguage . unlist <$> lookup "Accept-Language" kvs,
	getAcceptEncoding =
		map parseAcceptEncoding . unlist <$> lookup "Accept-Encoding" kvs,
	getConnection = map parseConnection . unlist <$> lookup "Connection" kvs,
	getCacheControl =
		map parseCacheControl . unlist <$> lookup "Cache-Control" kvs,
	getOthers = filter ((`notElem` getKeys) . fst) kvs
 }

parsePost :: [(BS.ByteString, BS.ByteString)] -> Post
parsePost kvs = Post {
	postHost = parseHost <$> lookup "Host" kvs,
	postUserAgent = map parseProduct . sepTkn <$> lookup "User-Agent" kvs,
	postAccept = map parseAccept . unlist <$> lookup "Accept" kvs,
	postAcceptLanguage =
		map parseAcceptLanguage . unlist <$> lookup "Accept-Language" kvs,
	postAcceptEncoding =
		map parseAcceptEncoding . unlist <$> lookup "Accept-Encoding" kvs,
	postConnection = map parseConnection . unlist <$> lookup "Connection" kvs,
	postCacheControl =
		map parseCacheControl . unlist <$> lookup "Cache-Control" kvs,
	postContentType = parseContentType <$> lookup "Content-Type" kvs,
	postContentLength = ContentLength . read . BSC.unpack <$>
		lookup "Content-Length" kvs,
	postOthers = filter ((`notElem` postKeys) . fst) kvs,
	postBody = ""
 }

postKeys :: [BS.ByteString]
postKeys = [
	"Host", "User-Agent", "Accept", "Accept-Language", "Accept-Encoding",
	"Connection", "Cache-Control", "Content-Type", "Content-Length"
 ]

sepTkn :: BS.ByteString -> [BS.ByteString]
sepTkn "" = []
sepTkn psrc
	| Just ('(', src) <- BSC.uncons psrc = let
		(cm, src') = let (c_, s_) = BSC.span (/= ')') src in
			case BSC.uncons s_ of
				Just (')', s__) -> (c_, s__)
				_ -> error "setTkn: bad comment" in
		('(' -:- cm +++ ")") : sepTkn (BSC.dropWhile isSpace src')
-- sepTkn ('(' : src) = ('(' : cm ++ ")") : sepTkn (dropWhile isSpace src')
--	where
--	(cm, ')' : src') = span (/= ')') src
sepTkn src = tk : sepTkn (BSC.dropWhile isSpace src')
	where
	(tk, src') = BSC.span (not . isSpace) src

getKeys :: [BS.ByteString]
getKeys = [
	"Host", "User-Agent", "Accept", "Accept-Language", "Accept-Encoding",
	"Connection", "Cache-Control"
 ]

data Host = Host BS.ByteString (Maybe Int) deriving Show

parseHost :: BS.ByteString -> Host
parseHost src = case BSC.span (/= ':') src of
	(h, cp) -> case BSC.uncons cp of
		Just (':', p) -> Host h (Just . read $ BSC.unpack p)
		Nothing -> Host h Nothing
		_ -> error "parseHost: never occur"

showHost :: Host -> BS.ByteString
showHost (Host h p) = h +++ (maybe "" ((':' -:-) . BSC.pack . show) p)

data Product
	= Product BS.ByteString (Maybe BS.ByteString)
	| ProductComment BS.ByteString
	deriving Show

showProduct :: Product -> BS.ByteString
showProduct (Product pn mpv) = pn +++ case mpv of
	Just v -> '/' -:- v
	_ -> ""
showProduct (ProductComment cm) = "(" +++ cm +++ ")"

parseProduct :: BS.ByteString -> Product
parseProduct pcm
	| Just ('(', cm) <- BSC.uncons pcm = case myLast "here" cm of
		')' -> ProductComment $ BS.init cm
		_ -> error "parseProduct: bad comment"
parseProduct pnv = case BSC.span (/= '/') pnv of
	(pn, sv) -> case BSC.uncons sv of
		Just ('/', v) -> Product pn $ Just v
		_ -> Product pnv Nothing

myLast :: String -> BS.ByteString -> Char
myLast err bs
	| BS.null bs = error err
	| otherwise = BSC.last bs

data Accept
	= Accept (BS.ByteString, BS.ByteString) Qvalue
	deriving Show

showAccept :: Accept -> BS.ByteString
showAccept (Accept (t, st) qv) = ((t +++ "/" +++ st) +++) $ showQvalue qv

parseAccept :: BS.ByteString -> Accept
parseAccept src = case BSC.span (/= ';') src of
	(mr, sqv) -> case BSC.uncons sqv of
		Just (';', qv) -> Accept (parseMediaRange mr) $ parseQvalue qv
		Nothing -> Accept (parseMediaRange mr) $ Qvalue 1
		_ -> error "parseAccept: never occur"

parseMediaRange :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parseMediaRange src = case BSC.span (/= '/') src of
	(t, sst) -> case BSC.uncons sst of
		Just ('/', st) -> (t, st)
		_ -> error "parseMediaRange: bad media range"

unlist :: BS.ByteString -> [BS.ByteString]
unlist "" = []
unlist src = case BSC.span (/= ',') src of
	(h, "") -> [h]
	(h, ct) -> case BSC.uncons ct of
		Just (',', t) -> h : unlist (BSC.dropWhile isSpace t)
		_ -> error "unlist: never occur"

data Qvalue
	= Qvalue Double
	deriving Show

showQvalue :: Qvalue -> BS.ByteString
showQvalue (Qvalue 1.0) = ""
showQvalue (Qvalue qv) = ";q=" +++ BSC.pack (show qv)

parseQvalue :: BS.ByteString -> Qvalue
parseQvalue qeqv
	| ("q=", qv) <- BS.splitAt 2 qeqv = Qvalue . read $ BSC.unpack qv
parseQvalue _ = error "parseQvalue: bad qvalue"

data AcceptLanguage
	= AcceptLanguage BS.ByteString Qvalue
	deriving Show

showAcceptLanguage :: AcceptLanguage -> BS.ByteString
showAcceptLanguage (AcceptLanguage al qv) = al +++ showQvalue qv

parseAcceptLanguage :: BS.ByteString -> AcceptLanguage
parseAcceptLanguage src = case BSC.span (/= ';') src of
	(al, sqv) -> case BSC.uncons sqv of
		Just (';', qv) -> AcceptLanguage al $ parseQvalue qv
		Nothing -> AcceptLanguage al $ Qvalue 1
		_ -> error "parseAcceptLanguage: never occur"

data AcceptEncoding
	= AcceptEncoding BS.ByteString Qvalue
	deriving Show

showAcceptEncoding :: AcceptEncoding -> BS.ByteString
showAcceptEncoding (AcceptEncoding ae qv) = ae +++ showQvalue qv

parseAcceptEncoding :: BS.ByteString -> AcceptEncoding
parseAcceptEncoding src = case BSC.span (/= ';') src of
	(ae, sqv) -> case BSC.uncons sqv of
		Just (';', qv) -> AcceptEncoding ae $ parseQvalue qv
		Nothing -> AcceptEncoding ae $ Qvalue 1
		_ -> error "parseAcceptEncoding: never occur"

data Connection
	= Connection BS.ByteString
	deriving Show

showConnection :: Connection -> BS.ByteString
showConnection (Connection c) = c

parseConnection :: BS.ByteString -> Connection
parseConnection src = Connection src

data CacheControl
	= MaxAge Int
	| CacheControlRaw BS.ByteString
	deriving Show

showCacheControl :: CacheControl -> BS.ByteString
showCacheControl (MaxAge ma) = "max-age=" +++ BSC.pack (show ma)
showCacheControl (CacheControlRaw cc) = cc

parseCacheControl :: BS.ByteString -> CacheControl
parseCacheControl ccma
	| ("max-age", ema) <- BSC.span (/= '=') ccma = case BSC.uncons ema of
		Just ('=', ma) -> MaxAge . read $ BSC.unpack ma
		_ -> error "parseCacheControl: bad"
parseCacheControl cc = CacheControlRaw cc

data Response = Response {
	responseVersion :: Version,
	responseStatusCode :: StatusCode,
	responseDate :: UTCTime,
	responseContentLength :: ContentLength,
	responseContentType :: ContentType,
	responseServer :: Maybe [Product],
	responseLastModified :: Maybe UTCTime,
	responseETag :: Maybe BS.ByteString,
	responseAcceptRanges :: Maybe BS.ByteString,
	responseConnection :: Maybe BS.ByteString,
	responseOthers :: [(BS.ByteString, BS.ByteString)],
	responseBody :: BS.ByteString
 } deriving Show

parseResponse :: [BS.ByteString] -> Response
parseResponse (h : t) = let (v, sc) = parseResponseLine h in
	parseResponseSep v sc $ map separate t
	where
	separate i = let (k, csv) = BSC.span (/= ':') i in
		case BS.splitAt 2 csv of
			(": ", v) -> (k, v)
			_ -> error "parseResponse: bad response"
	-- let (k, ':' : ' ' : v) = span (/= ':') i in (k, v)
parseResponse _ = error "parseResponse: bad response"

parseResponseSep :: Version -> StatusCode -> [(BS.ByteString, BS.ByteString)] -> Response
parseResponseSep v sc kvs = Response {
	responseVersion = v,
	responseStatusCode = sc,
	responseDate = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" .
		BSC.unpack . initN 4 . fromJust $ lookup "Date" kvs,
	responseContentLength = ContentLength . read . BSC.unpack . fromJust $
		lookup "Content-Length" kvs,
	responseContentType = parseContentType . fromJust $
		lookup "Content-Type" kvs,
	responseServer = map parseProduct . sepTkn <$> lookup "Server" kvs,
	responseLastModified = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" .
		BSC.unpack . initN 4 <$> lookup "Last-Modified" kvs,
	responseETag = lookup "ETag" kvs,
	responseAcceptRanges = lookup "Accept-Ranges" kvs,
	responseConnection = lookup "Connection" kvs,
	responseOthers = filter ((`notElem` responseKeys) . fst) kvs,
	responseBody = ""
 }

responseKeys :: [BS.ByteString]
responseKeys = [
	"Date", "Content-Length", "Content-Type", "Server", "Last-Modified",
	"ETag", "Accept-Ranges", "Connection" ]

initN :: Int -> BS.ByteString -> BS.ByteString
initN n lst = BS.take (BS.length lst - n) lst

parseResponseLine :: BS.ByteString -> (Version, StatusCode)
parseResponseLine src = case BSC.span (/= ' ') src of
	(vs, sscs) -> case BSC.uncons sscs of
		Just (' ', scs) -> (parseVersion vs, parseStatusCode scs)
		_ -> error "parseResponseLine: bad response line"

parseStatusCode :: BS.ByteString -> StatusCode
parseStatusCode sc
	| ("200", _) <- BSC.span (not . isSpace) sc = OK
parseStatusCode _ = error "parseStatusCode: bad status code"

showResponse :: Response -> [Maybe BS.ByteString]
showResponse r =
	[	Just $ showVersion (responseVersion r) +++ " " +++
			showStatusCode (responseStatusCode r),
		Just $ "Date: " +++ showTime (responseDate r),
		Just $ "Content-Length: " +++
			showContentLength (responseContentLength r),
		Just $ "Content-Type: " +++
			showContentType (responseContentType r),
		("Server: " +++) . BSC.unwords . map showProduct <$> responseServer r,
		("Last-Modified: " +++) . showTime <$> responseLastModified r,
		("ETag: " +++) <$> responseETag r,
		("Accept-Ranges: " +++) <$> responseAcceptRanges r,
		("Connection: " +++) <$> responseConnection r
	 ] ++
	map (\(k, v) -> Just $ k +++ ": " +++ v) (responseOthers r) ++
	[	Just "",
		Just $ responseBody r
	 ]

data StatusCode = Continue | SwitchingProtocols | OK deriving Show

showStatusCode :: StatusCode -> BS.ByteString
showStatusCode Continue = "100 Continue"
showStatusCode SwitchingProtocols = "101 SwitchingProtocols"
showStatusCode OK = "200 OK"

data ContentLength = ContentLength Int deriving Show

showContentLength :: ContentLength -> BS.ByteString
showContentLength (ContentLength n) = BSC.pack $ show n

contentLength :: ContentLength -> Int
contentLength (ContentLength n) = n

data ContentType = ContentType (BS.ByteString, BS.ByteString) deriving Show

parseContentType :: BS.ByteString -> ContentType
parseContentType ct = case BSC.span (/= '/') ct of
	(t, sst) -> case BSC.uncons sst of
		Just ('/', st) -> ContentType (t, st)
		_ -> error "parseContentType: bad Content-Type"

showContentType :: ContentType -> BS.ByteString
showContentType (ContentType (t, st)) = t +++ "/" +++ st

showTime :: UTCTime -> BS.ByteString
showTime = BSC.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
