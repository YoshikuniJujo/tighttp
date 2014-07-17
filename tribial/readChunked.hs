{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

readChunkedTN :: IO BS.ByteString
readChunkedTN = do
	sn <- BSC.hGetLine stdin
	let n = read $ BSC.unpack sn
	case n of
		0 -> return ""
		_ -> do	r <- BS.hGet stdin n
			"" <- BSC.hGetLine stdin
			(r `BS.append`) <$> readChunkedTN
