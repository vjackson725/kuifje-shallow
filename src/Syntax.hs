{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}

module Syntax where

import DataTypes

type a ~> b = a -> Dist b

data PCL3 s
  = Skip3
  | Update3 (s ~> s) (PCL3 s)
  | If3 (s ~> Bool) (PCL3 s) (PCL3 s) (PCL3 s)
  | While3 (s ~> Bool) (PCL3 s) (PCL3 s)
  | forall o. (Ord o) => Observe3' (s ~> o) (PCL3 s)

data PCL3F s a
  =  Skip3F
  |  Update3F (s ~> s) a
  |  If3F (s ~> Bool) a a a
  |  While3F (s ~> Bool) a a
  |  forall o. (Ord o) => Observe3F' (s ~> o) a

skip3 :: PCL3 s
skip3 = Skip3

update3 :: (s ~> s) -> PCL3 s
update3 f  =  Update3 f skip3

while3 :: (s ~> Bool) -> PCL3 s -> PCL3 s
while3 c p  =  While3 c p skip3

cond3 :: (s ~> Bool) -> PCL3 s -> PCL3 s -> PCL3 s
cond3 c p q  =  If3 c p q skip3

observe3' :: (Ord o) => (s ~> o) -> PCL3 s
observe3' o = Observe3' o skip3