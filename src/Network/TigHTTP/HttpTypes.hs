{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Network.TigHTTP.HttpTypes (
	Version(..),
	Request(..), RequestType(..), Path(..), Get(..), CacheControl(..),
	Accept(..), AcceptLanguage(..), Qvalue(..),
	Host(..), Product(..), Connection(..),
	Response(..), StatusCode(..), ContentLength(..), contentLength,
	ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
	TransferEncoding(..),

	parseReq, parseResponse, putRequest, putResponse, (+++),
	myLast, requestBodyLength, postAddBody,

	Post(..),
	putPostBody,
	requestBody,
	requestPath,

	AcceptEncoding(..),
	HostName,

	SetCookie(..), setCookie,
) where

import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import Data.List
import Text.Read (readMaybe)
import Data.Char
-- import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Time
import Data.Pipe
import Data.Pipe.List
import System.Locale

import Network.TigHTTP.Papillon
import Network.TigHTTP.CookieTime

import Data.HandleLike

type HostName = String

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

(-:-) :: Char -> BS.ByteString -> BS.ByteString
(-:-) = BSC.cons

data Request h
	= RequestGet Path Version Get
	| RequestPost Path Version (Post h)
	| RequestRaw RequestType Path Version [(BS.ByteString, BS.ByteString)]

requestBodyLength :: Request h -> Maybe Int
requestBodyLength (RequestPost _ _ p) = contentLength <$> postContentLength p
requestBodyLength _ = Nothing

postAddBody :: HandleLike h => Request h -> BS.ByteString -> Request h
postAddBody (RequestPost u v p) b = RequestPost u v $ p { postBody = pp }
	where
	pp = fromList [b]
postAddBody r _ = r

putPostBody :: HandleLike h => h -> Request h ->
	Pipe () BS.ByteString (HandleMonad h) () -> Request h
putPostBody _ (RequestPost u v p) pp = RequestPost u v $ p { postBody = pp }
putPostBody _ r _ = r

requestBody :: HandleLike h => Request h -> Pipe () BS.ByteString (HandleMonad h) ()
requestBody (RequestPost _ _ p) = postBody p
requestBody _ = return ()

requestPath :: Request h -> Path
requestPath (RequestGet p _ _) = p
requestPath (RequestPost p _ _) = p
requestPath (RequestRaw _ p _ _) = p

putRequest :: HandleLike h => h -> Request h -> HandleMonad h ()
putRequest sv (RequestGet uri vsn g) = do
	let r = [
		Just $ "GET " +++ showPath uri +++ " " +++ showVersion vsn,
		("Host: " +++) . showHost <$> getHost g,
		("User-Agent: " +++) . BSC.unwords .
			map showProduct <$> getUserAgent g,
		("Accept: " +++) . BSC.intercalate "," .
			map showAccept <$> getAccept g,
		("Accept-Language: " +++) . BSC.intercalate "," .
			map showAcceptLanguage <$> getAcceptLanguage g,
		("Accept-Encoding: " +++) . BSC.intercalate "," .
			map showAcceptEncoding <$> getAcceptEncoding g,
		("Connection: " +++) . BSC.intercalate "," .
			map showConnection <$> getConnection g,
		("Cache-Control: " +++) . BSC.intercalate "," .
			map showCacheControl <$> getCacheControl g,
		("Cookie : " +++) . BSC.intercalate "; " .
			map (\(k, v) -> k +++ "=" +++ v) <$>
				if null $ getCookie g
					then Nothing
					else Just $ getCookie g,
		Just ""
		]
	hlDebug sv "medium" . crlf $ catMaybes r
	hlPut sv . crlf $ catMaybes r
putRequest sv (RequestPost uri vsn p) = do
	let hd = [
		Just $ "POST " +++ showPath uri +++ " " +++ showVersion vsn,
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
		("Content-Length: " +++) .  showContentLength <$> postContentLength p,
		("Cookie : " +++) . BSC.intercalate "; " .
			map (\(k, v) -> k +++ "=" +++ v) <$>
				if null $ postCookie p
					then Nothing
					else Just $ postCookie p
		] ++ map (\(k, v) -> Just $ k +++ ": " +++ v) (postOthers p) ++ [
 			Just "" ]
	hlDebug sv "medium" . crlf $ catMaybes hd
	hlPut sv . crlf $ catMaybes hd
	_ <- runPipe $ postBody p =$= putAll sv
	return ()
putRequest sv (RequestRaw rt uri vsn kvs) = do
	let r = [
		Just $ showRequestType rt +++
			" " +++ showPath uri +++ " " +++ showVersion vsn ] ++
		map (\(k, v) -> Just $ k +++ ": " +++ v) kvs ++ [ Just "" ]
	hlPut sv . crlf $ catMaybes r

data Get = Get {
	getCacheControl :: Maybe [CacheControl],
	getConnection :: Maybe [Connection],
	getAccept :: Maybe [Accept],
	getAcceptEncoding :: Maybe [AcceptEncoding],
	getAcceptLanguage :: Maybe [AcceptLanguage],
	getHost :: Maybe Host,
	getUserAgent :: Maybe [Product],
	getCookie :: [(BS.ByteString, BS.ByteString)],
	getOthers :: [(BS.ByteString, BS.ByteString)]
 } deriving Show

data Post h = Post {
	postCacheControl :: Maybe [CacheControl],
	postConnection :: Maybe [Connection],
	postTransferEncoding :: Maybe TransferEncoding,
	postAccept :: Maybe [Accept],
	postAcceptEncoding :: Maybe [AcceptEncoding],
	postAcceptLanguage :: Maybe [AcceptLanguage],
	postHost :: Maybe Host,
	postUserAgent :: Maybe [Product],
	postContentLength :: Maybe ContentLength,
	postContentType :: Maybe ContentType,
	postCookie :: [(BS.ByteString, BS.ByteString)],
	postOthers :: [(BS.ByteString, BS.ByteString)],
	postBody :: Pipe () BS.ByteString (HandleMonad h) () }

data RequestType
	= RequestTypeGet
	| RequestTypePost
	| RequestTypeRaw BS.ByteString
	deriving Show

showRequestType :: RequestType -> BS.ByteString
showRequestType RequestTypeGet = "GET"
showRequestType RequestTypePost = "POST"
showRequestType (RequestTypeRaw rt) = rt

data Path = Path BS.ByteString deriving Show

showPath :: Path -> BS.ByteString
showPath (Path uri) = uri

data Version = Version Int Int deriving Show

showVersion :: Version -> BS.ByteString
showVersion (Version vmjr vmnr) =
	"HTTP/" +++ BSC.pack (show vmjr) +++ "." +++ BSC.pack (show vmnr)

parseReq :: HandleLike h => [BS.ByteString] -> Request h
parseReq (h : t) = let
	(rt, uri, v) = parseRequestLine h in
	parseSep rt uri v $ map separate t
	where
	separate i = let (k, csv) = BSC.span (/= ':') i in
		case BS.splitAt 2 csv of
			(": ", v) -> (k, v)
			_ -> error "parse: bad"
parseReq [] = error "parse: null request"

parseSep :: HandleLike h => RequestType ->
	Path -> Version -> [(BS.ByteString, BS.ByteString)] -> Request h
parseSep RequestTypeGet uri v kvs = RequestGet uri v $ parseGet kvs
parseSep RequestTypePost uri v kvs = RequestPost uri v $ parsePost kvs
parseSep rt uri v kvs = RequestRaw rt uri v kvs

parseRequestLine :: BS.ByteString -> (RequestType, Path, Version)
parseRequestLine rl = let
	(rts, uris, vs) = case BSC.words rl of
		[r, u, v] -> (r, u, v)
		_ -> error $ "Network.TigHTTP.HttpTypes.parseRequestLine: " ++
			show rl
	rt = case rts of
		"GET" -> RequestTypeGet
		"POST" -> RequestTypePost
		_ -> RequestTypeRaw rts in
	(rt, Path uris, parseVersion vs)

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
	getCookie = maybe [] parseCookie $ lookup "Cookie" kvs,
	getOthers = filter ((`notElem` getKeys) . fst) kvs
 }

parsePost :: HandleLike h => [(BS.ByteString, BS.ByteString)] -> Post h
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
	postTransferEncoding = case lookup "Transfer-Encoding" kvs of
		Just "chunked" -> Just Chunked
		Nothing -> Nothing
		_ -> error "bad Transfer-Encoding",
	postCookie = maybe [] parseCookie $ lookup "Cookie" kvs,
	postOthers = filter ((`notElem` postKeys) . fst) kvs,
	postBody = return ()
 }

postKeys :: [BS.ByteString]
postKeys = [
	"Host", "User-Agent", "Accept", "Accept-Language", "Accept-Encoding",
	"Connection", "Cache-Control", "Content-Type", "Content-Length",
	"Transfer-Encoding"
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
	"Connection", "Cache-Control", "Cookie"
 ]

data Host = Host BS.ByteString (Maybe Int) deriving Show

parseHost :: BS.ByteString -> Host
parseHost src = case BSC.span (/= ':') src of
	(h, cp) -> case BSC.uncons cp of
		Just (':', p) -> Host h (Just . read $ BSC.unpack p)
		Nothing -> Host h Nothing
		_ -> error "parseHost: never occur"

showHost :: Host -> BS.ByteString
showHost (Host h p) = h +++ maybe "" ((':' -:-) . BSC.pack . show) p

data Product
	= Product BS.ByteString (Maybe BS.ByteString)
	| ProductComment BS.ByteString
	deriving (Show, Eq)

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
parseConnection = Connection

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

parseCookie :: BS.ByteString -> [(BS.ByteString, BS.ByteString)]
parseCookie bs =
	map ((\[k, v] -> (BSC.dropWhile isSpace k, BSC.dropWhile isSpace v))
			. BSC.split '=')
		$ BSC.split ';' bs

data Response p h = Response {
	responseVersion :: Version,
	responseStatusCode :: StatusCode,
	responseConnection :: Maybe BS.ByteString,
	responseDate :: Maybe UTCTime,
	responseTransferEncoding :: Maybe TransferEncoding,
	responseAcceptRanges :: Maybe BS.ByteString,
	responseETag :: Maybe BS.ByteString,
	responseServer :: Maybe [Product],
	responseContentLength :: Maybe ContentLength,
	responseContentType :: ContentType,
	responseLastModified :: Maybe UTCTime,
	responseSetCookie :: [SetCookie],
	responseOthers :: [(BS.ByteString, BS.ByteString)],
	responseBody :: p () BS.ByteString (HandleMonad h) ()
	}

data SetCookie = SetCookie {
	cookieName :: BS.ByteString,
	cookieValue :: BS.ByteString,
	cookieExpires :: Maybe UTCTime,
	cookieMaxAge :: Maybe DiffTime,
	cookieDomain :: Maybe BS.ByteString,
	cookiePath :: Maybe BS.ByteString,
	cookieSecure :: Bool,
	cookieHttpOnly :: Bool,
	cookieExtension :: [BS.ByteString]
	} deriving Show

setCookie :: BS.ByteString -> BS.ByteString -> SetCookie
setCookie n v = SetCookie {
	cookieName = n,
	cookieValue = v,
	cookieExpires = Nothing,
	cookieMaxAge = Nothing,
	cookieDomain = Nothing,
	cookiePath = Nothing,
	cookieSecure = True,
	cookieHttpOnly = True,
	cookieExtension = []
	}

parseResponse :: (
	Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h) =>
	[BS.ByteString] -> Response p h
parseResponse (h : t) = let (v, sc) = parseResponseLine h in
	parseResponseSep v sc $ map separate t
	where
	separate i = let (k, csv) = BSC.span (/= ':') i in
		case BS.splitAt 2 csv of
			(": ", v) -> (k, v)
			_ -> error $ "parseResponse: bad response: " ++ show csv
	-- let (k, ':' : ' ' : v) = span (/= ':') i in (k, v)
parseResponse _ = error "parseResponse: null response"

parseResponseSep :: (
	Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h) =>
	Version -> StatusCode ->
	[(BS.ByteString, BS.ByteString)] -> Response p h
parseResponseSep v sc kvs = Response {
	responseVersion = v,
	responseStatusCode = sc,
	responseDate = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" .
		BSC.unpack . initN 4 <$> lookup "Date" kvs,
	responseContentLength = ContentLength . read . BSC.unpack <$>
		lookup "Content-Length" kvs,
	responseTransferEncoding = case lookup "Transfer-Encoding" kvs of
		Just "chunked" -> Just Chunked
		Nothing -> Nothing
		_ -> error "bad Transfer-Encoding",
	responseContentType = parseContentType . fromJust $
		lookup "Content-Type" kvs,
	responseServer = map parseProduct . sepTkn <$> lookup "Server" kvs,
	responseLastModified = readTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S" .
		BSC.unpack . initN 4 <$> lookup "Last-Modified" kvs,
	responseETag = lookup "ETag" kvs,
	responseAcceptRanges = lookup "Accept-Ranges" kvs,
	responseConnection = lookup "Connection" kvs,
	responseSetCookie = map parseSetCookie . map snd $ filter ((== "Set-Cookie") . fst) kvs ,
	responseOthers = filter ((`notElem` responseKeys) . fst) kvs,
	responseBody = return ()
	}

{-
data SetCookie = SetCookie {
	cookieName :: BS.ByteString,
	cookieValue :: BS.ByteString,
	cookieExpire :: Maybe UTCTime,
	cookieMaxAge :: Maybe DiffTime,
	cookieDomain :: Maybe BS.ByteString,
	cookiePath :: Maybe BS.ByteString,
	cookieSecure :: Bool,
	cookieHttpOnly :: Bool,
	cookieExtension :: [BS.ByteString]
	}
	deriving Show
	-}

setCookieKeys :: [BS.ByteString]
setCookieKeys = [
	
	]

parseSetCookie :: BS.ByteString -> SetCookie
parseSetCookie bs = SetCookie {
	cookieName = nm,
	cookieValue = vl,
	cookieExpires = (\(CookieExpires e) -> e) <$> find isCookieExpires cas,
	cookieMaxAge = (\(CookieMaxAge ma) -> ma) <$> find isCookieMaxAge cas,
	cookieDomain = (\(CookieDomain d) -> d) <$> find isCookieDomain cas,
	cookiePath = (\(CookiePath p) -> p) <$> find isCookiePath cas,
	cookieSecure = CookieSecure `elem` cas,
	cookieHttpOnly = CookieHttpOnly `elem` cas,
	cookieExtension = (\(CookieExtension e) -> e) <$> filter isCookieExtension cas
	}
	where
	(nmvl) : kvs = map (BSC.dropWhile isSpace) $ BSC.split ';' bs
	[nm, vl] = BSC.split '=' nmvl
	cas = map cookieAttr kvs

cookieAttr :: BS.ByteString -> CookieAttr
cookieAttr ca
	| ["Expires", v] <- av, Just d <- parseCookieExpiresTime v =
		CookieExpires d
	| ["Max-Age", v] <- av, Just ma <- fromInteger <$> readMaybe (BSC.unpack v) =
		CookieMaxAge ma
	| ["Domain", v] <- av = CookieDomain v
	| ["Path", v] <- av = CookiePath v
	where
	av = BSC.split '=' ca
cookieAttr "Secure" = CookieSecure
cookieAttr "HttpOnly" = CookieHttpOnly
cookieAttr ca = CookieExtension ca

data CookieAttr
	= CookieExpires UTCTime
	| CookieMaxAge DiffTime
	| CookieDomain BS.ByteString
	| CookiePath BS.ByteString
	| CookieSecure
	| CookieHttpOnly
	| CookieExtension BS.ByteString
	deriving (Show, Eq)

isCookieExpires, isCookieMaxAge, isCookieDomain,
	isCookiePath, isCookieExtension :: CookieAttr -> Bool
isCookieExpires (CookieExpires _) = True; isCookieExpires _ = False
isCookieMaxAge (CookieMaxAge _) = True; isCookieMaxAge _ = False
isCookieDomain (CookieDomain _) = True; isCookieDomain _ = False
isCookiePath (CookiePath _) = True; isCookiePath _ = False
isCookieExtension (CookieExtension _) = True; isCookieExtension _ = False

responseKeys :: [BS.ByteString]
responseKeys = [
	"Date", "Content-Length", "Content-Type", "Server", "Last-Modified",
	"ETag", "Accept-Ranges", "Connection", "Transfer-Encoding", "Set-Cookie" ]

initN :: Int -> BS.ByteString -> BS.ByteString
initN n lst = BS.take (BS.length lst - n) lst

parseResponseLine :: BS.ByteString -> (Version, StatusCode)
parseResponseLine src = case BSC.span (/= ' ') src of
	(vs, sscs) -> case BSC.uncons sscs of
		Just (' ', scs) -> (parseVersion vs, parseStatusCode scs)
		_ -> error "parseResponseLine: bad response line"

parseStatusCode :: BS.ByteString -> StatusCode
parseStatusCode sc
	| ("100", _) <- BSC.span (not . isSpace) sc = Continue
	| ("101", _) <- BSC.span (not . isSpace) sc = SwitchingProtocols
	| ("200", _) <- BSC.span (not . isSpace) sc = OK
	| ("201", _) <- BSC.span (not . isSpace) sc = Created
	| ("202", _) <- BSC.span (not . isSpace) sc = Accepted
	| ("203", _) <- BSC.span (not . isSpace) sc = NonAuthoritativeInformation
	| ("204", _) <- BSC.span (not . isSpace) sc = NoContent
	| ("205", _) <- BSC.span (not . isSpace) sc = ResetContent
	| ("206", _) <- BSC.span (not . isSpace) sc = PartialContent
	| ("301", _) <- BSC.span (not . isSpace) sc = MovedPermanently
	| ("302", _) <- BSC.span (not . isSpace) sc = Found
	| ("303", _) <- BSC.span (not . isSpace) sc = SeeOther
	| ("304", _) <- BSC.span (not . isSpace) sc = NotModified
	| ("305", _) <- BSC.span (not . isSpace) sc = UseProxy
	| ("307", _) <- BSC.span (not . isSpace) sc = TemporaryRedirect
	| ("400", _) <- BSC.span (not . isSpace) sc = BadRequest
	| ("401", _) <- BSC.span (not . isSpace) sc = Unauthorized
	| ("402", _) <- BSC.span (not . isSpace) sc = PaymentRequired
	| ("403", _) <- BSC.span (not . isSpace) sc = Forbidden
	| ("404", _) <- BSC.span (not . isSpace) sc = NotFound
	| ("405", _) <- BSC.span (not . isSpace) sc = MethodNotAllowed
	| ("406", _) <- BSC.span (not . isSpace) sc = NotAcceptable
	| ("407", _) <- BSC.span (not . isSpace) sc = ProxyAuthenticationRequired
	| ("408", _) <- BSC.span (not . isSpace) sc = RequestTimeout
	| ("409", _) <- BSC.span (not . isSpace) sc = Conflict
	| ("500", _) <- BSC.span (not . isSpace) sc = InternalServerError
	| ("501", _) <- BSC.span (not . isSpace) sc = NotImplemented
	| ("502", _) <- BSC.span (not . isSpace) sc = BadGateway
	| ("503", _) <- BSC.span (not . isSpace) sc = ServiceUnavailable
	| ("504", _) <- BSC.span (not . isSpace) sc = GatewayTimeout
	| ("505", _) <- BSC.span (not . isSpace) sc = HttpVersionNotSupported
parseStatusCode sc = error $ "parseStatusCode: bad status code: " ++ BSC.unpack sc

putResponse :: (
	PipeClass p, MonadTrans (p BS.ByteString ()),
	Monad (p BS.ByteString () (HandleMonad h)),
	HandleLike h) =>
	h -> Response p h -> HandleMonad h ()
putResponse cl r = do
	let	hd = [
			Just $ showVersion (responseVersion r) +++ " " +++
				showStatusCode (responseStatusCode r),
			Just $ "Date: " +++ maybe "" showTime (responseDate r),
			("Content-Length: " +++) . showContentLength <$>
				responseContentLength r,
			("Transfer-Encoding: " +++) . showTransferEncoding <$>
				responseTransferEncoding r,
			Just $ "Content-Type: " +++
				showContentType (responseContentType r),
			("Server: " +++) . BSC.unwords . map showProduct <$> responseServer r,
			("Last-Modified: " +++) . showTime <$> responseLastModified r,
			("ETag: " +++) <$> responseETag r,
			("Accept-Ranges: " +++) <$> responseAcceptRanges r,
			("Connection: " +++) <$> responseConnection r
			] ++
			map (Just . ("Set-Cookie: " +++) . showSetCookie)
				(responseSetCookie r) ++
			map (\(k, v) -> Just $ k +++ ": " +++ v) (responseOthers r) ++
			[ Just "" ]
		p = responseBody r
	hlPut cl . crlf $ catMaybes hd
	_ <- runPipe $ p =$= putAll cl
	return ()

{-
data SetCookie = SetCookie {
	cookieName :: BS.ByteString,
	cookieValue :: BS.ByteString,
	cookieExpires :: Maybe UTCTime,
	cookieMaxAge :: Maybe DiffTime,
	cookieDomain :: Maybe BS.ByteString,
	cookiePath :: Maybe BS.ByteString,
	cookieSecure :: Bool,
	cookieHttpOnly :: Bool,
	cookieExtension :: [BS.ByteString]
	} deriving Show
-}

showSetCookie :: SetCookie -> BS.ByteString
showSetCookie sc = BS.intercalate "; " $ catMaybes [
	Just $ cookieName sc +++ "=" +++ cookieValue sc,
	("Expires=" +++) . cookieExpiresTime <$> cookieExpires sc,
	("Max-Age=" +++) . BSC.pack . init . show <$> cookieMaxAge sc,
	("Domain=" +++) <$> cookieDomain sc,
	("Path=" +++) <$> cookiePath sc,
	if cookieSecure sc then Just "Secure" else Nothing,
	if cookieHttpOnly sc then Just "HttpOnly" else Nothing
	]

putAll :: (
	PipeClass p, MonadTrans (p BS.ByteString ()),
	Monad (p BSC.ByteString () (HandleMonad h)),
	HandleLike h) =>
	h -> p BS.ByteString () (HandleMonad h) ()
putAll cl = do
	ms <- await
	case ms of
		Just s -> do
--			lift (hlPut cl s >> hlPut cl "\r\n")
			lift $ hlPut cl s
			putAll cl
		_ -> return ()

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (`BS.append` "\r\n")

data StatusCode
	= Continue
	| SwitchingProtocols
	| OK
	| Created
	| Accepted
	| NonAuthoritativeInformation
	| NoContent
	| ResetContent
	| PartialContent
	| MultipleChoices
	| MovedPermanently
	| Found
	| SeeOther
	| NotModified
	| UseProxy
	| TemporaryRedirect
	| BadRequest
	| Unauthorized
	| PaymentRequired
	| Forbidden
	| NotFound
	| MethodNotAllowed
	| NotAcceptable
	| ProxyAuthenticationRequired
	| RequestTimeout
	| Conflict
	| InternalServerError
	| NotImplemented
	| BadGateway
	| ServiceUnavailable
	| GatewayTimeout
	| HttpVersionNotSupported
	deriving Show

showStatusCode :: StatusCode -> BS.ByteString
showStatusCode Continue = "100 Continue"
showStatusCode SwitchingProtocols = "101 SwitchingProtocols"
showStatusCode OK = "200 OK"
showStatusCode Created = "201 Created"
showStatusCode Accepted = "202 Accepted"
showStatusCode NonAuthoritativeInformation = "203 Non-Authoritative Information"
showStatusCode NoContent = "204 No Content"
showStatusCode ResetContent = "205 Reset Content"
showStatusCode PartialContent = "206 Partial Content"
showStatusCode MultipleChoices = "300 Multiple Choices"
showStatusCode MovedPermanently = "301 Moved Permanently"
showStatusCode Found = "302 Found"
showStatusCode SeeOther = "303 See Other"
showStatusCode NotModified = "304 Not Modified"
showStatusCode UseProxy = "305 Use Proxy"
showStatusCode TemporaryRedirect = "307 Temporary Redirect"
showStatusCode BadRequest = "400 Bad Request"
showStatusCode Unauthorized = "401 Unauthorized"
showStatusCode PaymentRequired = "402 Payment Required"
showStatusCode Forbidden = "403 Forbidden"
showStatusCode NotFound = "404 Not Found"
showStatusCode MethodNotAllowed = "405 Method Not Allowd"
showStatusCode NotAcceptable = "406 Not Acceptable"
showStatusCode ProxyAuthenticationRequired = "407 Proxy Authentication Required"
showStatusCode RequestTimeout = "408 Request Timeout"
showStatusCode Conflict = "409 Conflict"
showStatusCode InternalServerError = "500 Internal Server Error"
showStatusCode NotImplemented = "501 Not Implemented"
showStatusCode BadGateway = "502 Bad Gateway"
showStatusCode ServiceUnavailable = "503 Service Unavailable"
showStatusCode GatewayTimeout = "504 Gateway Timeout"
showStatusCode HttpVersionNotSupported = "505 HTTP version Not Supported"

data ContentLength = ContentLength Int deriving Show

showContentLength :: ContentLength -> BS.ByteString
showContentLength (ContentLength n) = BSC.pack $ show n

contentLength :: ContentLength -> Int
contentLength (ContentLength n) = n

data TransferEncoding = Chunked deriving Show

showTransferEncoding :: TransferEncoding -> BS.ByteString
showTransferEncoding Chunked = "chunked"

showTime :: UTCTime -> BS.ByteString
showTime = BSC.pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
