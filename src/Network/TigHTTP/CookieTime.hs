module Network.TigHTTP.CookieTime (
	cookieExpiresTime, parseCookieExpiresTime) where

import Data.Time (UTCTime, formatTime, parseTime, defaultTimeLocale)

import qualified Data.ByteString.Char8 as BSC

format :: String
format = "%a, %d-%b-%Y %H:%M:%S GMT"

cookieExpiresTime :: UTCTime -> BSC.ByteString
cookieExpiresTime = BSC.pack . formatTime defaultTimeLocale format

parseCookieExpiresTime :: BSC.ByteString -> Maybe UTCTime
parseCookieExpiresTime = parseTime defaultTimeLocale format . BSC.unpack
