module ISO where

import Data.Void
import Data.Maybe
-- A type of `Void` have no value.
-- So it is impossible to construct `Void`,
-- unless using undefined, error, unsafeCoerce, infinite recursion, etc
-- And there is a function
-- absurd :: Void -> a
-- That get any value out of `Void`
-- We can do this becuase we can never have void in the zeroth place.

-- so, when are two type, `a` and `b`, considered equal?
-- a definition might be, it is possible to go from `a` to `b`,
-- and from `b` to `a`.
-- Going a roundway trip should leave you the same value.
-- Unfortunately it is virtually impossible to test this in Haskell.
-- This is called Isomorphism.

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (\a -> a, \a -> a)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (f, g) = (g, f)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (f, f') (g, g') = (g . f, f' . g')

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) =
  (\(a, c) -> (ab a, cd c), \(b, d) -> (ba b, dc d))

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (\(alist) -> map (ab) alist,
                    \(blist) -> map (ba) blist)

maybeBind :: (a -> b) -> Maybe a -> Maybe b
maybeBind _ Nothing = Nothing
maybeBind ab (Just a) = Just $ ab a

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (maybeBind ab, maybeBind ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (eitherBind ab cd, eitherBind ba dc)

eitherBind :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherBind ab _ (Left a) = Left $ ab a
eitherBind _ cd (Right c) = Right $ cd c

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) =
  (\ac -> cd . ac . ba, \bd -> dc . bd . ab)

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
-- Remember, for all valid ISO, converting and converting back
-- Is the same as the original value.
-- You need this to prove some case are impossible.
isoUnMaybe (mab, mba) = (maybeUnBind mab mba, maybeUnBind mba mab)

maybeUnBind :: (Maybe a -> Maybe b) -> (Maybe b -> Maybe a) -> a -> b
maybeUnBind mab mba a = case mab (Just a) of
                          (Just b) -> b
                          Nothing -> let (Just x) = mab . mba . mab $ (Just a)
                                     in x


-- We cannot have
-- isoUnEither :: ISO (Either a b) (Either c d) -> ISO a c -> ISO b d.
-- Note that we have
isoEU :: ISO (Either [()] ()) (Either [()] Void)
isoEU = (eitherUnBindLeft, eitherUnBindRight)

eitherUnBindLeft :: Either [()] () -> Either [()] Void
eitherUnBindLeft (Left xs) = Left xs
eitherUnBindLeft (Right ())= Right $ absurd (undefined :: Void)

eitherUnBindRight :: Either [()] Void -> Either [()] ()
eitherUnBindRight (Left xs) = Left xs
eitherUnBindRight (Right x) = (absurd x) :: (Either [()] ())

-- where (), the empty tuple, has 1 value, and Void has 0 value
-- If we have isoUnEither,
-- We have ISO () Void by calling isoUnEither isoEU
-- That is impossible, since we can get a Void by substL on ISO () Void
-- So it is impossible to have isoUnEither

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (\(ab, ba) -> (ba, ab), \(ba, ab) -> (ab, ba))

data Foo = Foo
data Bar = Bar

isoBaz :: ISO (Maybe Foo) (Maybe Bar)
isoBaz = (fwd, bwd)
  where
    fwd = maybe (Just Bar) (const Nothing)
    bwd = maybe (Just Foo) (const Nothing)
