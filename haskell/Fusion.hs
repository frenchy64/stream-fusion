import Prelude hiding (map)
import Debug.Trace

{-# LANGUAGE ExistentialQuantification #-}

{-# RULES
--"map -> fusible" 
--   forall f xs. map f xs = unstream (mapS f (stream xs))
"stream/unstream fusion" 
   forall s. stream (unstream s) = trace "FUSE" s
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

unstream :: Stream a -> [a]
unstream (Stream next0 s0) = (unfold s0)
  where
    unfold s = case next0 s of
      Done       -> []
      Skip    s' ->     unfold s'
      Yield x s' -> x : unfold s'


mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next0 s0) = Stream next s0
  where
    next s = case next0 s of
      Done       -> Done
      Skip    s' -> Skip        s'
      Yield x s' -> Yield (f x) s'

map :: (a -> b) -> [a] -> [b]
map f = unstream . mapS f . stream

main :: IO ()
main = putStrLn . show $ (map (+1) . map (+1)) [1, 2]

-- unstream . mapS (+1) . stream $ (unstream . mapS (+1) . stream $ [1,2])
