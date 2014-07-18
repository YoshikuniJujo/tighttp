{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}

import Text.Papillon

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

instance Source ByteString where
	type Token ByteString = Char
	data Pos ByteString = NoPos
	getToken = BSC.uncons
	initialPos = NoPos
	updatePos _ _ = NoPos

[papillon|

source: ByteString

hoge :: ByteString
	= 'h' 'o' 'g' 'e'	{ "hoge" }

|]
