module Data.DList

%default total

export
record DList a where
  constructor MkDList
  getDList : List a -> List a

export implicit
toDList : List a -> DList a
toDList = MkDList . (++)

export
Cast (List a) (DList a) where
  cast = toDList

export
fromDList : DList a -> List a
fromDList (MkDList f) = f []

export
Cast (DList a) (List a) where
  cast = fromDList

export
(++) : DList a -> DList a -> DList a
(MkDList f) ++ (MkDList g) = MkDList $ f . g

export
Semigroup (DList a) where
  (<+>) = (++)

export
Monoid (DList a) where
  neutral = []
