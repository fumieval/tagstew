{-# LANGUAGE LambdaCase, BangPatterns, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -ddump-simpl -dsuppress-all -ddump-to-file #-}
module Text.HTML.TagStew (parseTags) where

import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import qualified Text.HTML.TagSoup as S
import Text.HTML.TagStew.Entity (lookupEntity)
import Control.Concurrent
import Control.Monad
import Data.Bits
import System.IO.Unsafe
import Data.Char
import Data.Word (Word8)

data Stack = Text !Int
  | Entity !Int
  | TagOpen !Int
  | TagClose !Int
  | Attrs !Range !Attrs
  | AttrName !Range !Int !Attrs
  | AttrValue !Quote !Range !Range !Int !Attrs
  | Comment !Int
  | Script !Int
  deriving Show

newtype Range = MkRange Int deriving Show
data Quote = QNone | QSingle | QDouble
  deriving Show

type Attrs = [(Range, Range)]

pattern Range :: Int -> Int -> Range
pattern Range i j <- (unpack -> (i, j)) where
  Range i j = MkRange $ i `shiftL` 32 .|. j
{-# COMPLETE Range #-}

unpack :: Range -> (Int, Int)
unpack (MkRange x) = (x `shiftR` 32, x .&. 0xffffffff)
{-# INLINE unpack #-}

parseTags :: B.ByteString -> [S.Tag B.ByteString]
parseTags bs = unsafePerformIO $ do
    var <- newEmptyMVar
    _ <- forkIO $ parse bs var
    let go _ = unsafePerformIO $ do
          resp <- takeMVar var
          return $! case resp of
            S.TagText bs | B.null bs -> []
            s -> s : go ()
    return $ go ()

parse :: B.ByteString -> MVar (S.Tag B.ByteString) -> IO ()
parse bs out = go 0 (Text 0) where
  slice (Range i j) = B.unsafeTake (j - i) $ B.unsafeDrop i bs
  push x = putMVar out $! fmap slice x
  isScript (Range i j) = BC.map toLower (B.unsafeTake (j - i) (B.unsafeDrop i bs)) == "script"
  {-# INLINE isScript #-}
  go :: Int -> Stack -> IO ()
  go loc _ | loc >= B.length bs = push $ S.TagText $ Range 0 0
  go !loc !st = case st of
    Comment i
      | B.take 3 (B.drop loc bs) == "-->" -> do
        push $ S.TagComment $ Range i loc
        go (loc + 3) $ Text (loc + 3)
      | otherwise -> char
    Entity i -> case ch of
      C_SC -> do
        case lookupEntity (B.take (loc - i) $ B.drop i bs) of
          Just x -> putMVar out $ S.TagText x
          Nothing -> push $ S.TagText (Range i loc)
        next $ Text loc'
      _ | byteCount > 1 -> next $ Text i
      _ -> char
    Text i -> case ch of
      C_LT -> do
        pushText $ Range i loc
        next $ TagOpen loc'
      C_AMP -> do
        pushText $ Range i loc
        next $ Entity loc'
      _ -> char
    TagOpen i -> case ch of
      C_GT -> pushOpen 1 (Range i loc) []
      C_SL -> next $ TagClose loc'
      _ | isSpace' ch -> next $ AttrName (Range i loc) loc' []
      33 | B.unsafeIndex bs loc' == c2w '-',
          B.unsafeIndex bs (loc + 2) == c2w '-' -> go (loc + 3) (Comment (loc + 3))
      _ -> char
    Attrs tname attrs -> case ch of
      C_GT -> pushOpen 1 tname attrs
      C_SL -> pushOpen 2 tname attrs
      _ | isSpace' ch -> next $ AttrName tname loc' attrs
      _ -> char
    AttrName tname i attrs -> case ch of
      C_SQ -> next $ AttrValue QSingle tname (Range 0 0) loc' attrs
      C_DQ -> next $ AttrValue QDouble tname (Range 0 0) loc' attrs
      C_EQ -> next $ AttrValue QNone tname (Range i loc) loc' attrs
      C_SL -> pushOpen 2 tname attrs
      C_GT -> pushOpen 1 tname attrs
      _ | isSpace' ch -> next $ AttrName tname loc' ((Range i loc, Range 0 0) : attrs)
      _ -> char
    AttrValue QSingle tname aname i attrs -> case ch of
      C_SQ -> next $ Attrs tname $ (aname, Range i loc) : attrs
      C_BS -> go (loc + 2) st
      _ -> char
    AttrValue QDouble tname aname i attrs -> case ch of
      C_DQ -> next $ Attrs tname $ (aname, Range i loc) : attrs
      C_BS -> go (loc + 2) st
      _ -> char
    AttrValue QNone tname aname i attrs -> case ch of
      C_SQ -> next $ AttrValue QSingle tname aname (i + 1) attrs
      C_DQ -> next $ AttrValue QDouble tname aname (i + 1) attrs
      C_SL -> pushOpen 2 tname $ (aname, Range i loc) : attrs
      C_GT -> pushOpen 1 tname $ (aname, Range i loc) : attrs
      _ | isSpace' ch -> next $ AttrName tname loc' $ (aname, Range i loc) : attrs
      _ -> char
    TagClose i -> case ch of
      C_GT -> do
        push $ S.TagClose $! Range i loc
        next $ Text $ loc + 1
      _ -> char
    Script i
      | map toLower (BC.unpack (B.take 9 (B.drop loc bs))) == "</script>" -> do
        pushText $ Range i loc
        push $ S.TagClose $ Range (loc + 2) (loc + 8)
        next $ Text $ loc + 9
      | otherwise -> char
    where
      pushText str@(Range i j) = unless (i == j) $ push $ S.TagText str
      pushOpen ofs tname attrs = do
        push $ S.TagOpen tname attrs
        if isScript tname
          then go (loc + ofs) $ Script loc'
          else next $ Text $ loc + ofs
      {-# INLINE pushOpen #-}
      loc' = loc + 1
      next = go loc'
      ch = B.unsafeIndex bs loc
      byteCount = max 1 $ countLeadingZeros (complement ch)
      char = go (loc + byteCount) st

isSpace' :: Word8 -> Bool
isSpace' ch = ch == 13 || ch == 10 || ch == 32

pattern C_SP, C_LT, C_EQ, C_GT, C_SL, C_SQ, C_DQ, C_BS, C_AMP, C_SC :: Word8
pattern C_SP = 32
pattern C_LT = 60
pattern C_EQ = 61
pattern C_GT = 62
pattern C_SL = 47
pattern C_SQ = 39
pattern C_DQ = 34
pattern C_BS = 92
pattern C_AMP = 38
pattern C_SC = 59


c2w :: Char -> Word8
c2w = toEnum . fromEnum
