module Support where
import Data.Bits

--data Posn = Posn !Int !Int !Int
--        deriving (Eq,Show)


--startPos = Posn 0 0 0

data Located e = L SrcLoc e
  deriving (Eq, Show)

unLoc :: Located e -> e
unLoc (L _ e) = e

getLoc :: Located e -> SrcLoc
getLoc (L l _) = l

data RealSrcLoc
  = SrcLoc      String                  -- A precise location (file name)
                {-# UNPACK #-} !Int     -- line number, begins at 1
                {-# UNPACK #-} !Int     -- column number, begins at 1
  deriving Show

data SrcLoc
  = RealSrcLoc {-# UNPACK #-}!RealSrcLoc
  | UnhelpfulLoc String     -- Just a general indication
  deriving Show


mkSrcLoc :: String -> Int -> Int -> SrcLoc
mkSrcLoc x line col = RealSrcLoc (mkRealSrcLoc x line col)

mkRealSrcLoc :: String -> Int -> Int -> RealSrcLoc
mkRealSrcLoc x line col = SrcLoc x line col

-- | Built-in "bad" 'SrcLoc' values for particular locations
noSrcLoc, generatedSrcLoc, interactiveSrcLoc :: SrcLoc
noSrcLoc          = UnhelpfulLoc "<no location info>"
generatedSrcLoc   = UnhelpfulLoc "<compiler-generated code>"
interactiveSrcLoc = UnhelpfulLoc "<interactive session>"

-- | Creates a "bad" 'SrcLoc' that has no detailed information about its location
mkGeneralSrcLoc :: String -> SrcLoc
mkGeneralSrcLoc = UnhelpfulLoc

-- | Gives the filename of the 'RealSrcLoc'
srcLocFile :: RealSrcLoc -> String
srcLocFile (SrcLoc fname _ _) = fname

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocLine :: RealSrcLoc -> Int
srcLocLine (SrcLoc _ l _) = l

-- | Raises an error when used on a "bad" 'SrcLoc'
srcLocCol :: RealSrcLoc -> Int
srcLocCol (SrcLoc _ _ c) = c

-- | Move the 'SrcLoc' down by one line if the character is a newline,
-- to the next 8-char tabstop if it is a tab, and across by one
-- character in any other case
advanceSrcLoc :: RealSrcLoc -> Char -> RealSrcLoc
advanceSrcLoc (SrcLoc f l _) '\n' = SrcLoc f  (l + 1) 1
advanceSrcLoc (SrcLoc f l c) '\t' = SrcLoc f  l (((((c - 1) `shiftR` 3) + 1)
                                                  `shiftL` 3) + 1)
advanceSrcLoc (SrcLoc f l c) _    = SrcLoc f  l (c + 1)

instance Eq SrcLoc where
  loc1 == loc2 = case loc1 `cmpSrcLoc` loc2 of
                 EQ     -> True
                 _other -> False

instance Eq RealSrcLoc where
  loc1 == loc2 = case loc1 `cmpRealSrcLoc` loc2 of
                 EQ     -> True
                 _other -> False

instance Ord SrcLoc where
  compare = cmpSrcLoc

instance Ord RealSrcLoc where
  compare = cmpRealSrcLoc

cmpSrcLoc :: SrcLoc -> SrcLoc -> Ordering
cmpSrcLoc (UnhelpfulLoc s1) (UnhelpfulLoc s2) = s1 `compare` s2
cmpSrcLoc (UnhelpfulLoc _)  (RealSrcLoc _)    = GT
cmpSrcLoc (RealSrcLoc _)    (UnhelpfulLoc _)  = LT
cmpSrcLoc (RealSrcLoc l1)   (RealSrcLoc l2)   = (l1 `compare` l2)

cmpRealSrcLoc :: RealSrcLoc -> RealSrcLoc -> Ordering
cmpRealSrcLoc (SrcLoc s1 l1 c1) (SrcLoc s2 l2 c2)
  = (s1 `compare` s2) `thenCmp` (l1 `compare` l2) `thenCmp` (c1 `compare` c2)

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering
    
