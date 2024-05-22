{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Kuifje.PrettyPrint where

import Data.List (transpose, intersperse)
import qualified Data.Map.Strict as HM
import Debug.Trace
import Text.Printf (printf)

import qualified Text.PrettyPrint.Boxes as PP
import Numeric

import Language.Kuifje.Distribution (Dist, unpackD)
import Language.Kuifje.ShallowConsts (decimalPrecision)


class Boxable a where
  toBox :: a -> PP.Box


instance Boxable Bool where
  toBox = PP.text . show

instance Boxable Integer where
  toBox = PP.text . show

instance Boxable Int where
  toBox = PP.text . show

rationalPrettyFormat =
  if decimalPrecision < 0
  then show
  else printf ("%." ++ show decimalPrecision ++ "f") . (fromRat :: _ -> Double)

instance Boxable Rational where
  toBox = PP.text . rationalPrettyFormat

instance Boxable Double where
  toBox = PP.text . printf ("%." ++ show decimalPrecision ++ "f")

instance Boxable a => Boxable [a] where
  toBox xs =
    let vs = map toBox xs
    in PP.text "[" PP.<+>
       boxmiddle vs PP.<+>
       PP.text "]"
    where
      boxmiddle ([b]) = b
      boxmiddle (b : bs) = b PP.<> PP.text "," PP.<+> boxmiddle bs
      boxmiddle [] = PP.text ""

instance (Show a, Show b) => Boxable (a,b) where
  toBox = PP.text . show

instance (Show a, Show b, Show c) => Boxable (a,b,c) where
  toBox = PP.text . show

instance (Show a, Show b, Show c, Show d) => Boxable (a,b,c,d) where
  toBox = PP.text . show

distToBox :: (Ord a, Boxable a) => Dist a -> PP.Box
distToBox d =
  let ps = HM.toList . unpackD $ d
   in if null ps
      then PP.text "✗"
      else tabulate . map (\(a,b) -> [toBox b, toBox a]) $ ps

instance (Boxable a, Ord a) => Boxable (Dist a) where
  toBox = distToBox

tabulate :: [[PP.Box]] -> PP.Box
tabulate rs = table
  where
   heights  = map (maximum . map PP.rows) rs
   rs''     = zipWith (\r h -> map (PP.alignVert PP.top h) r) rs heights
   columns  = transpose rs''
   widths   = map (maximum . map PP.cols) columns
   rs'      = transpose $ zipWith (\c w -> map (PP.alignHoriz PP.left w) c) columns widths
   columns' = map (PP.hsep 3 PP.top) rs'
   table    = PP.vsep 0 PP.left columns'
