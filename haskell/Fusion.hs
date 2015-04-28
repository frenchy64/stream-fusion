import Prelude hiding (map, filter)
import Debug.Trace

{-# LANGUAGE ExistentialQuantification #-}

{-# RULES
"map -> fusible" [1]
   forall f xs. map f xs = (unstream (mapS f (stream xs)))
"filter -> fusible" [1]
   forall f xs. filter f xs = (unstream (filterS f (stream xs)))
"stream/unstream fusion" [0]
   forall s. stream (unstream s) = s
  #-}

data Stream a = forall s. Stream (s -> Step a s) s
data Step a s = Done
              | Yield a s
              | Skip s

stream :: [a] -> Stream a
stream xs0 = (Stream next xs0)
  where
    next []       = Done
    next (x : xs) = Yield x xs
{-# NOINLINE [1] stream #-}

unstream :: Stream a -> [a]
unstream (Stream next0 s0) = (unfold s0)
  where
    unfold s = case next0 s of
      Done       -> []
      Skip    s' ->     unfold s'
      Yield x s' -> x : unfold s'
{-# NOINLINE [1] unstream #-}


mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
      Done       -> Done
      Skip    s' -> Skip        s'
      Yield x s' -> Yield (f x) s'
{-# NOINLINE [1] mapS #-}

map :: (a -> b) -> [a] -> [b]
map f = unstream . mapS f . stream
{-# NOINLINE [1] map #-}

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
              Done    -> Done
              Skip s' -> Skip s'
              Yield x s' | p x       -> Yield x s'
                         | otherwise -> Skip s'

filter :: (a -> Bool) -> [a] -> [a]
filter p = unstream . filterS p . stream
{-# NOINLINE [1] filter #-}

main :: IO ()
main = 
  putStrLn . show 
  $ (map (+1) (filter even (map (+1) [1, 2, 3 ,4])))

-- unstream . mapS (+1) . stream $ (unstream . mapS (+1) . stream $ [1,2])
