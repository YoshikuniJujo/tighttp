module Network.TigHTTP.Token (
	isTokenChar,
	isTextChar,
	isQdtextChar,
	) where

import Data.Char (isAscii)

isCtl, isSeparator, isTokenChar, isTextChar, isQdtextChar :: Char -> Bool
isCtl = (`elem` (['\0' .. '\31'] ++ "\DEL"))

isSeparator = (`elem` "()<>@,;:\\\"/[]?={} \t")

isTokenChar = (&&) <$> not . isCtl <*> not . isSeparator

isTextChar = (&&) <$> isAscii <*> not . isCtl

isQdtextChar = (&&) <$> isTextChar <*> (/= '"')
