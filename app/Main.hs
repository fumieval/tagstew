{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.HTML.TagSoup as Soup
import Text.HTML.TagStew as Stew
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.UTF8 as BU
import System.Environment (getArgs)
import Control.Monad
import Text.Show.Unicode
import System.IO (stdout)

fromTag :: Tag B.ByteString -> BB.Builder
fromTag = go where
  f = BB.byteString
  go (TagOpen t xs) = "\ESC[34m<" <> f t
    <> foldMap (\(k, v) -> " " <> f k <> "=" <> f v) xs
    <> ">\ESC[0m"
  go (TagClose t) = "\ESC[34m</" <> f t <> ">\ESC[0m"
  go (TagText t) = f t
  go (TagComment t) = "<!--" <> f t <> "-->"
{-# INLINE fromTag #-}

main :: IO ()
main = getArgs >>= \paths -> forM_ paths $ \path -> do
  bs <- B.readFile path
  BB.hPutBuilder stdout $ foldMap ((<>"\n") . fromTag) $ Soup.parseTags bs
