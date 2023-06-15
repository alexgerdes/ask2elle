-- ! Personally i prefer typechecker rejects the following snippet,
-- !     as incomplete typeclass instantiation
class X a where
  f :: a -> Int

instance X Int

main = f 3