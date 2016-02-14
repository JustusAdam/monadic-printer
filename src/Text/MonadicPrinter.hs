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

As you can see in the last line, you can combine pieces of Text using the '<>'
operator or 'mappend' method from 'Data.Monoid' which is reexported by this module
for your convenience.

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

=== All together

@
  let name = \"Jeremy\"
      millions = 4
  log_ $ do
    "He said he didn't want to do it."
    "\\"" <> cs name <> "\\" I said, \\"You have to. This job will make us rich.\\""
    "\\"How rich exactly?\\""
    "\\"" <> co millions <> " million dollars at least\\""
    "This seemed to convince him."
@

Prints

@
  He said he didn't want to do it.
  \"Jeremy\" I said, "You have to. This job will make us rich."
  "How rich exactly?"
  "4 million dollars at least"
  This seemed to convince him.
@

-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Text.MonadicPrinter
  (
  -- * Writing
    Printer

  , module Data.Monoid
  -- ** Conversion functions
  , convertString, cs
  , convertObject, co
  -- * Printing
  , print
  , log
  , hPrint
  , hLog
  -- * Retrieving contents
  , getLines
  , getText
  ) where


import           Control.Applicative
import           Data.Foldable           (traverse_)
import           Data.List               (intersperse)
import           Data.Monoid
import           Data.String             (IsString, fromString)
import qualified Data.String.Conversions as CS (ConvertibleStrings,
                                                convertString)
import           Data.Text               (Text)
import           Data.Text.IO
import           Prelude                 hiding (putStrLn, unlines, print, log)
import           System.IO               (Handle)
import           Unsafe.Coerce


-- | Stores what you write.
--
-- The type variale only exists for the 'Monad' instance and is ignored in all
-- operations.
newtype Printer t a = Printer
  { getLines :: [t] -- ^ Get all contents of the printer as list of lines
  }


instance Functor (Printer t) where
  fmap _ = unsafeCoerce


instance Applicative (Printer t) where
  pure = const $ Printer []
  f <*> v = f >> fmap unsafeCoerce v
  (Printer c1) *> (Printer c2) = Printer (c1 <> c2)
  (Printer c1) <* (Printer c2) = Printer (c2 <> c1)


instance Monad (Printer t) where
  (>>) = (*>)

  h >>= f = h >> f (error "Cannot use monadic bind here")

  return = pure


instance IsString t => IsString (Printer t a) where
  fromString s = Printer [fromString s]


instance Monoid t => Monoid (Printer t a) where
  mempty = Printer []
  mappend (Printer s1) (Printer s2) =
    Printer $ case (reverse s1, s2) of
                ([], y) -> y
                (_, []) -> s1
                (x:xs, y:ys) -> reverse xs <> (x <> y : ys)


-- | Write some lines to stdout. For a version that writes to a different handle
-- see 'hPrint'.
print :: Printer Text a -> IO ()
print (Printer s) = traverse_ putStrLn s


-- | Synonym for 'print_'
log :: Printer Text a -> IO ()
log = print


-- | Write some lines to a handle.
hPrint :: Handle -> Printer Text a -> IO ()
hPrint h = traverse_ (hPutStrLn h) . getLines


-- | Synonym for 'hPrint'
hLog :: Handle -> Printer Text a -> IO ()
hLog = hPrint


-- | Get all the printer contents as a single piece of 'Text' (inserts newlines)
getText :: (Monoid t, IsString t) => Printer t a -> t
getText = mconcat . intersperse "\n" . getLines


-- | Convert a non-literal String to something printable
convertString :: CS.ConvertibleStrings a t => a -> Printer t c
convertString = Printer . return . CS.convertString


-- | shorter form of 'convertString'
cs :: CS.ConvertibleStrings a t => a -> Printer t c
cs = convertString


-- | convert a showable piece of data into something printable
convertObject :: (IsString t, Show a) => a -> Printer t b
convertObject = fromString . show


-- | shorter form of 'convertObject'
co :: (IsString t, Show a) => a -> Printer t b
co = convertObject
