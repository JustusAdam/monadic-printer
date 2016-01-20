{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Text.MonadicPrinter
  ( Printer
  , print_
  , log_
  , getContent
  ) where


import Data.Monoid
import Data.String (IsString, fromString)
import Unsafe.Coerce
import Data.Text
import Data.Text.IO
import Prelude hiding (putStrLn, unlines, hPutStrLn)
import System.IO (Handle)
import Data.Foldable (traverse_)


data Printer a = Printer { getContent :: [Text] }


instance Functor Printer where
  fmap _ = unsafeCoerce


instance Applicative Printer where
  pure = const $ Printer []
  f <*> v = f >> fmap unsafeCoerce v
  (Printer c1) *> (Printer c2) = Printer (c1 <> c2)
  (Printer c1) <* (Printer c2) = Printer (c2 <> c1)


instance Monad Printer where
  (>>) = (*>)

  h >>= f = h >> f (error "Cannot use monadig bind here")

  return = pure


instance IsString (Printer a) where
  fromString s = Printer [pack s]


print_ :: Printer a -> IO ()
print_ (Printer s) = traverse_ putStrLn s


log_ :: Printer a -> IO ()
log_ = print_


hPrint :: Handle -> Printer a -> IO ()
hPrint h = traverse_ (hPutStrLn h) . getContent


hLog :: Handle -> Printer a -> IO ()
hLog = hPrint


getText :: Printer a -> Text
getText = unlines . getContent
