
module ParserM (
    -- Parser Monad
    P(..), AlexInput, runP,
    -- Parser state
    StartCode, alexStartCode, alexSetStartCode,
    -- Actions
    Located(..), Action, mkEOF, mkT, mkTIdent, mkIdent, mkString, mkFloat, mkInt,
    -- Positions
    getLoc, getSrcLoc, showSrcLoc, unLoc,
    -- Input
    alexGetByte, alexInputPrevChar, alexSrcLoc, alexInp,
    -- Other
    happyError,
    testInput
 ) where

import Control.Monad.Error (throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (lift)
import Data.Char (ord)
import Data.Word (Word8)
import Tokens
import qualified Data.Bits
import Support

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8

-- Parser Monad
data AlexInput = AlexInput {
        alexSrcLoc    :: !RealSrcLoc,  -- position at current input location
        alexInp       :: String,     -- the current input
        alexChar      :: !Char,      -- the character before the input
        alexBytes     :: [Byte],     -- pending bytes on current char
        alexStartCode :: !StartCode  -- the current startcode
    }
type StartCode = Int
                 
newtype P a = P { unP :: AlexInput -> Either String (AlexInput, a) }


instance Monad P where
  m >>= k  = P $ \s -> case unP m s of
                             Left msg -> Left msg
                             Right (s',a) -> unP (k a) s'
  return a = P $ \s -> Right (s,a)
  fail err = P $ \_ -> fail err


runP :: String -> String -> P a -> Either String a
runP fileName input (P f) 
   = case f (AlexInput {alexSrcLoc = alexStartSrcLoc fileName,
                        alexInp = input,       
                        alexChar= '\n',
                        alexBytes = [],
                        alexStartCode = 0}) of Left msg -> Left msg
                                               Right ( _, a ) -> Right a

--actions
type Action = (AlexInput, String) -> Either String (Located Token, AlexInput)

getSrcLoc :: P RealSrcLoc
getSrcLoc = P $ \i@(AlexInput{alexSrcLoc=l}) -> return (i, l)

srcLoc :: AlexInput -> SrcLoc
srcLoc (AlexInput{alexSrcLoc=l}) = RealSrcLoc l

mkEOF :: Action
mkEOF (p,_) = return (L (srcLoc p) TEOF, p)

mkT :: Token -> Action
mkT t (p,_) = return (L (srcLoc p) t, p)

mkFloat :: Action
mkFloat (p,s) = return (L (srcLoc p) $ TFloat (read s), p)

mkInt :: Action
mkInt (p,s) = return (L (srcLoc p) $ TInt (read s), p)

mkIdent :: Action
mkIdent (p,s) = return (L (srcLoc p) $ TIdent s, p)

mkTIdent :: Action
mkTIdent (p,s) = return (L (srcLoc p) $ TTIdent s, p)

mkString :: Action
mkString (p,s) = return (L (srcLoc p) $ TString (init (tail s)), p)

alexGetStartCode :: P StartCode
alexGetStartCode = P $ \s@AlexInput{alexStartCode=sc} -> Right (s, sc)

alexSetStartCode :: StartCode -> P ()
alexSetStartCode sc = P $ \i -> Right (i{alexStartCode=sc}, ())

-- Positions

getPos :: P RealSrcLoc
getPos = P $ \i@(AlexInput{alexSrcLoc=l}) -> return (i, l)

alexStartSrcLoc :: String -> RealSrcLoc
alexStartSrcLoc fileName = SrcLoc fileName 1 1

showSrcLoc :: RealSrcLoc -> String
showSrcLoc rl = show rl

-- Input

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i{alexBytes=[]}

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput{alexChar=c} = c


alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte i@AlexInput{alexBytes=(b:bs)} = Just (b, i{alexBytes=bs})
alexGetByte i@AlexInput{alexBytes=[],alexInp=[]} = Nothing
alexGetByte i@AlexInput{alexSrcLoc=l,alexBytes=[],alexInp=(c:s)} = let l' = advanceSrcLoc l c 
                                                                       (b:bs) = utf8Encode c
                                                                   in l' `seq`  Just (b, i{alexSrcLoc=l', alexChar=c, alexBytes=bs, alexInp=s})


happyError :: P a
happyError = do l <- getSrcLoc
                fail $ "Parse error at " ++ showSrcLoc l


testInput = (ParserM.AlexInput {alexSrcLoc = alexStartSrcLoc "bla",
                                alexInp = "lambda x:Bool. x;",       
                                alexChar= '\n',
                                alexBytes = [],
                                alexStartCode = 0})

