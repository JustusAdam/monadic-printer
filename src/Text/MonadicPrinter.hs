{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : $Header$
Description : Write long texts with do-notation
Copyright   : (c) Justus Adam, 2015
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX, Windows

= How to use it

"monadic-printer" enables you to write multi-line texts easily using do-notation.
It is recommended to use this library with the 'OverloadedStrings' extension enabled.

== Examples

=== Writing lines of text

@
  log_ $ do
    "A message on the first line"
    ""
    "Some more messages"
    "combining " <> "text"
@

Prints the following to stdout

@
  A message on the first line

  Some more messages
  combining text
@

=== Writing long texts

@
  getText $ do
    "My text"
    "line"
    "by"
    "line"
@

Produces @"My text\\nline\\nby\\nline" :: Text@

Or you can retrieve the content as list of lines

@
  getLines $ do
    "My text"
    "line"
    "by"
    "line"
@

Produces @["My text","line","by","line"] :: [Text]@


=== Using Showable data and other strings

This library also provides some convenient conversion functions for Strings other
than 'Text', and data with a 'Show' instance. Hence

@
  let string = "hello" :: String
  log_ $ do
    cs string
    "next line"
    co 1378
    "last line"
@

will print

@
  hello
  next line
  1378
  last line
@

to stdout.

-}
module Text.MonadicPrinter
  (
  -- * Writing
    Printer
  , module Data.Monoid
  -- ** Conversion functions
  , convertString, cs
  , convertObject, co
  -- * Printing
  , print_
  , log_
  , hPrint
  , hLog
  -- * Retrieving contents
  , getLines
  , getText
  ) where


import           Control.Applicative
import           Data.Foldable           (traverse_)
import           Data.Monoid
import           Data.String             (IsString, fromString)
import qualified Data.String.Conversions as CS (ConvertibleStrings,
                                                convertString)
import           Data.Text               (Text, pack, unlines)
import           Data.Text.IO
import           Prelude                 hiding (putStrLn, unlines)
import           System.IO               (Handle)
import           Unsafe.Coerce


-- | Stores what you write
newtype Printer a = Printer
  { getLines :: [Text] -- ^ Get all contents of the printer as list of lines
  }


instance Functor Printer where
  fmap _ = unsafeCoerce


instance Applicative Printer where
  pure = const $ Printer []
  f <*> v = f >> fmap unsafeCoerce v
  (Printer c1) *> (Printer c2) = Printer (c1 <> c2)
  (Printer c1) <* (Printer c2) = Printer (c2 <> c1)


instance Monad Printer where
  (>>) = (*>)

  h >>= f = h >> f (error "Cannot use monadic bind here")

  return = pure


instance IsString (Printer a) where
  fromString s = Printer [pack s]


instance Monoid (Printer a) where
  mempty = Printer []
  mappend (Printer s1) (Printer s2) =
    Printer $ case (reverse s1, s2) of
                ([], y) -> y
                (_, []) -> s1
                (x:xs, y:ys) -> reverse xs <> (x <> y : ys)


-- | Write some lines to stdout. For a version that writes to a different handle
-- see 'hPrint'.
print_ :: Printer a -> IO ()
print_ (Printer s) = traverse_ putStrLn s


-- | Synonym for 'print_'
log_ :: Printer a -> IO ()
log_ = print_


-- | Write some lines to a handle.
hPrint :: Handle -> Printer a -> IO ()
hPrint h = traverse_ (hPutStrLn h) . getLines


-- | Synonym for 'hPrint'
hLog :: Handle -> Printer a -> IO ()
hLog = hPrint


-- | Get all the printer contents as a single piece of 'Text' (inserts newlines)
getText :: Printer a -> Text
getText = unlines . getLines


-- | Convert a non-literal String to something printable
convertString :: CS.ConvertibleStrings a Text => a -> Printer c
convertString = Printer . return . CS.convertString


-- | shorter form of 'convertString'
cs :: CS.ConvertibleStrings a Text => a -> Printer c
cs = convertString


-- | convert a showable piece of data into something printable
convertObject :: Show a => a -> Printer b
convertObject = convertString . show


-- | shorter form of 'convertObject'
co :: Show a => a -> Printer b
co = convertObject
