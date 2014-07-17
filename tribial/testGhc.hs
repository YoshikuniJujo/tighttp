{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Monad
import Data.HandleLike
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Numeric
import System.IO

main = getChunked stdin

getChunked :: HandleLike h => h -> HandleMonad h BS.ByteString
getChunked h = do
	(n :: Int) <- (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ""
		_ -> do	r <- hlGet h n
			"" <- hlGetLine h
			(r `BS.append`) `liftM` getChunked
