{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Sit.Par
  ( happyError
  , myLexer
  , pPrg
  , pDecl
  , pQualId
  , pListDecl
  , pIdU
  , pBind
  , pListBind
  , pListIdent
  , pListIdU
  , pExp2
  , pExp1
  , pExp
  ) where
import qualified Sit.Abs
import Sit.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap15 = HappyWrap15 (Sit.Abs.Ident)
happyIn15 :: (Sit.Abs.Ident) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (Integer)
happyIn16 :: (Integer) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (Sit.Abs.Prg)
happyIn17 :: (Sit.Abs.Prg) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (Sit.Abs.Decl)
happyIn18 :: (Sit.Abs.Decl) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (Sit.Abs.QualId)
happyIn19 :: (Sit.Abs.QualId) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 ([Sit.Abs.Decl])
happyIn20 :: ([Sit.Abs.Decl]) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Sit.Abs.IdU)
happyIn21 :: (Sit.Abs.IdU) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Sit.Abs.Bind)
happyIn22 :: (Sit.Abs.Bind) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ([Sit.Abs.Bind])
happyIn23 :: ([Sit.Abs.Bind]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 ([Sit.Abs.Ident])
happyIn24 :: ([Sit.Abs.Ident]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 ([Sit.Abs.IdU])
happyIn25 :: ([Sit.Abs.IdU]) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (Sit.Abs.Exp)
happyIn26 :: (Sit.Abs.Exp) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (Sit.Abs.Exp)
happyIn27 :: (Sit.Abs.Exp) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (Sit.Abs.Exp)
happyIn28 :: (Sit.Abs.Exp) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x10\x04\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00\x00\x00\x80\x00\x40\x00\x00\x00\x20\x0c\x00\x00\x00\x00\x00\x00\x84\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x08\x00\x04\x00\x00\x00\x02\x78\x65\x99\x01\x00\x00\x40\x00\xaf\x2c\x33\x00\x00\x00\x08\xe0\xbf\x65\x06\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x05\xaf\x2c\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\xfc\xb7\xcc\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\xa0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\xbf\x65\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\xbc\xb2\xcc\x00\x00\x00\x20\x80\xff\x96\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x20\x08\x00\x00\x00\x04\xf0\xdf\x32\x03\x00\x00\x80\x00\xfe\x5b\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x04\xf0\xdf\x32\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\xff\x2d\x33\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x01\xfc\xb7\xcc\x00\x00\x00\x20\x80\xff\x96\x19\x00\x00\x00\x04\xf0\xdf\x32\x03\x00\x00\x80\x00\xfe\x5b\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x20\x80\xff\x96\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x02\xf8\x6f\x99\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\xe0\xbf\x65\x06\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x08\x00\x04\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x08\xe0\xbf\x65\x06\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pPrg","%start_pDecl","%start_pQualId","%start_pListDecl","%start_pIdU","%start_pBind","%start_pListBind","%start_pListIdent","%start_pListIdU","%start_pExp2","%start_pExp1","%start_pExp","Ident","Integer","Prg","Decl","QualId","ListDecl","IdU","Bind","ListBind","ListIdent","ListIdU","Exp2","Exp1","Exp","'('","')'","'+'","'--;'","'->'","'.'","'..'","':'","';'","'='","'Nat'","'Set'","'Set1'","'Set2'","'\\\\'","'_'","'case'","'fix'","'forall'","'import'","'lsuc'","'lzero'","'of'","'oo'","'open'","'return'","'suc'","'zero'","'{'","'}'","L_Ident","L_integ","%eof"]
        bit_start = st * 61
        bit_end = (st + 1) * 61
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..60]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x47\x00\x47\x00\xf9\xff\x47\x00\xf1\xff\x03\x00\x03\x00\xf9\xff\xf1\xff\x4f\x00\x4f\x00\x19\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x02\x00\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\x00\x00\x19\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x19\x00\x02\x00\xf1\xff\x02\x00\x11\x00\x52\x00\x03\x00\x52\x00\x3f\x00\x3f\x00\x3f\x00\x7a\x00\x7a\x00\xfe\xff\x99\x00\x8b\x00\x9a\x00\x00\x00\xfd\xff\x9b\x00\x9b\x00\x00\x00\x98\x00\x98\x00\x47\x00\x19\x00\x19\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xbb\x00\x00\x00\xba\x00\xae\x00\xc8\x00\xca\x00\x60\x00\xb0\x00\x19\x00\x00\x00\x00\x00\x00\x00\x19\x00\xbd\x00\x19\x00\x19\x00\x19\x00\x19\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\xcc\x00\x00\x00\xc7\x00\x00\x00\xcf\x00\xdf\x00\xe5\x00\xed\x00\x19\x00\x00\x00\x00\x00\xfa\x00\x19\x00\x00\x00\x19\x00\xe4\x00\xef\x00\xe7\x00\xe2\x00\xf1\xff\x02\x01\x00\x01\x19\x00\xe9\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x87\x00\x3d\x00\x07\x00\x95\x00\x63\x00\xff\x00\xf3\x00\x32\x00\x15\x00\x42\x00\xeb\x00\x6c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2d\x00\x00\x00\x74\x00\x00\x00\xf5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\x00\x82\x00\x00\x00\x4c\x00\x00\x00\x36\x00\x00\x00\xf9\x00\x00\x00\x41\x00\x08\x01\x09\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0a\x01\x53\x00\xa6\x00\x85\x00\x93\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x96\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x00\x00\xa7\x00\xb5\x00\xb8\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc9\x00\x00\x00\x00\x00\x00\x00\xd7\x00\x00\x00\xda\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xed\xff\xed\xff\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf3\xff\xe8\xff\xdc\xff\xdd\xff\xcf\xff\xc7\xff\x00\x00\x00\x00\xda\xff\xd9\xff\xd8\xff\xd7\xff\x00\x00\xe7\xff\x00\x00\xd4\xff\x00\x00\xd2\xff\xd3\xff\xdb\xff\xd5\xff\xd6\xff\xf2\xff\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\xe1\xff\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\x00\x00\xec\xff\x00\x00\x00\x00\x00\x00\xf1\xff\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\xe5\xff\xe6\xff\x00\x00\xe2\xff\xe0\xff\xde\xff\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcb\xff\xc9\xff\xd1\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\xf0\xff\xe9\xff\xee\xff\xeb\xff\x00\x00\xcd\xff\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xca\xff\x00\x00\x00\x00\xcc\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x10\x00\x01\x00\x06\x00\x01\x00\x10\x00\x08\x00\x00\x00\x0a\x00\x06\x00\x07\x00\x04\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x10\x00\x1d\x00\x12\x00\x1f\x00\x00\x00\x15\x00\x16\x00\x1f\x00\x18\x00\x01\x00\x06\x00\x1b\x00\x1c\x00\x21\x00\x0a\x00\x1f\x00\x20\x00\x21\x00\x21\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x00\x00\x15\x00\x16\x00\x1f\x00\x18\x00\x00\x00\x06\x00\x1b\x00\x1c\x00\x00\x00\x0a\x00\x1f\x00\x20\x00\x01\x00\x09\x00\x03\x00\x00\x00\x05\x00\x09\x00\x03\x00\x00\x00\x00\x00\x01\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x06\x00\x10\x00\x09\x00\x12\x00\x00\x00\x0b\x00\x15\x00\x16\x00\x01\x00\x18\x00\x06\x00\x00\x00\x1b\x00\x1c\x00\x0a\x00\x04\x00\x1f\x00\x20\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x1f\x00\x10\x00\x19\x00\x12\x00\x02\x00\x00\x00\x15\x00\x16\x00\x1f\x00\x18\x00\x08\x00\x06\x00\x1b\x00\x1c\x00\x00\x00\x01\x00\x1f\x00\x20\x00\x00\x00\x01\x00\x06\x00\x21\x00\x00\x00\x01\x00\x06\x00\x0b\x00\x0c\x00\x0d\x00\x06\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x06\x00\x00\x00\x01\x00\x00\x00\x06\x00\x02\x00\x03\x00\x06\x00\x05\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x00\x00\x01\x00\x03\x00\x06\x00\x05\x00\x21\x00\x06\x00\x04\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x00\x00\x00\x00\x01\x00\x03\x00\x06\x00\x05\x00\x21\x00\x06\x00\x14\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x1f\x00\x00\x00\x01\x00\x08\x00\x06\x00\x21\x00\x02\x00\x06\x00\x05\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x1a\x00\x00\x00\x01\x00\x01\x00\x06\x00\x05\x00\x02\x00\x06\x00\x20\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x1c\x00\x00\x00\x01\x00\x06\x00\x06\x00\x17\x00\x10\x00\x06\x00\x02\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x01\x00\x05\x00\x00\x00\x01\x00\x09\x00\x06\x00\x02\x00\x01\x00\x06\x00\x10\x00\x0b\x00\x0c\x00\x0d\x00\x0b\x00\x0c\x00\x00\x00\x01\x00\x07\x00\x08\x00\x07\x00\x08\x00\x06\x00\x05\x00\x07\x00\x08\x00\x1b\x00\x0b\x00\x02\x00\x05\x00\x07\x00\x1e\x00\x00\x00\x00\x00\x00\x00\xff\xff\x01\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x1b\x00\x26\x00\x3c\x00\x2e\x00\x1b\x00\x3f\x00\x36\x00\x40\x00\x2f\x00\x30\x00\x37\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0e\x00\x1b\x00\x4c\x00\x1d\x00\x0e\x00\x0e\x00\x1f\x00\x20\x00\x0e\x00\x21\x00\x15\x00\x27\x00\x22\x00\x23\x00\xff\xff\x28\x00\x0e\x00\x24\x00\xff\xff\xff\xff\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x1e\x00\x0e\x00\x1f\x00\x20\x00\x0e\x00\x21\x00\x29\x00\x27\x00\x22\x00\x23\x00\x29\x00\x4a\x00\x0e\x00\x24\x00\x26\x00\x2a\x00\x4e\x00\x32\x00\x4f\x00\x44\x00\x38\x00\x29\x00\x0e\x00\x0f\x00\x16\x00\x17\x00\x18\x00\x19\x00\x10\x00\x1b\x00\x42\x00\x1d\x00\x0e\x00\x26\x00\x1f\x00\x20\x00\x26\x00\x21\x00\x27\x00\x36\x00\x22\x00\x23\x00\x45\x00\x5b\x00\x0e\x00\x24\x00\x16\x00\x17\x00\x18\x00\x19\x00\x0e\x00\x1b\x00\x36\x00\x1d\x00\x52\x00\x0e\x00\x1f\x00\x20\x00\x0e\x00\x21\x00\x53\x00\x31\x00\x22\x00\x23\x00\x0e\x00\x0f\x00\x0e\x00\x24\x00\x0e\x00\x0f\x00\x10\x00\xff\xff\x0e\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x10\x00\x11\x00\x12\x00\x4c\x00\x0e\x00\x11\x00\x12\x00\x49\x00\x0e\x00\x0f\x00\x71\x00\x0e\x00\x0f\x00\x32\x00\x10\x00\x39\x00\x33\x00\x10\x00\x3a\x00\x11\x00\x12\x00\x46\x00\x11\x00\x12\x00\x59\x00\x0e\x00\x0f\x00\x32\x00\x0e\x00\x0f\x00\x33\x00\x10\x00\x34\x00\xff\xff\x10\x00\x3e\x00\x11\x00\x12\x00\x58\x00\x11\x00\x12\x00\x4f\x00\x0e\x00\x0f\x00\x32\x00\x0e\x00\x0f\x00\x33\x00\x10\x00\x5a\x00\xff\xff\x10\x00\x3d\x00\x11\x00\x12\x00\x62\x00\x11\x00\x12\x00\x60\x00\x0e\x00\x0f\x00\x0e\x00\x0e\x00\x0f\x00\x58\x00\x10\x00\xff\xff\x52\x00\x10\x00\x57\x00\x11\x00\x12\x00\x5f\x00\x11\x00\x12\x00\x5e\x00\x0e\x00\x0f\x00\x56\x00\x0e\x00\x0f\x00\x54\x00\x10\x00\x55\x00\x67\x00\x10\x00\x24\x00\x11\x00\x12\x00\x5d\x00\x11\x00\x12\x00\x67\x00\x0e\x00\x0f\x00\x62\x00\x0e\x00\x0f\x00\x3c\x00\x10\x00\x66\x00\x65\x00\x10\x00\x64\x00\x11\x00\x12\x00\x6a\x00\x11\x00\x12\x00\x6c\x00\x0e\x00\x0f\x00\x6a\x00\x0e\x00\x0f\x00\x6e\x00\x10\x00\x69\x00\x6f\x00\x10\x00\x71\x00\x11\x00\x12\x00\x74\x00\x11\x00\x24\x00\x0e\x00\x0f\x00\x2b\x00\x2c\x00\x2b\x00\x48\x00\x10\x00\x6c\x00\x2b\x00\x43\x00\x70\x00\x47\x00\x73\x00\x74\x00\x30\x00\x76\x00\x41\x00\x40\x00\x5c\x00\x00\x00\x50\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (12, 56) [
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56)
	]

happy_n_terms = 34 :: Int
happy_n_nonterms = 14 :: Int

happyReduce_12 = happySpecReduce_1  0# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TV happy_var_1)) -> 
	happyIn15
		 (Sit.Abs.Ident happy_var_1
	)}

happyReduce_13 = happySpecReduce_1  1# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TI happy_var_1)) -> 
	happyIn16
		 ((read (happy_var_1)) :: Integer
	)}

happyReduce_14 = happySpecReduce_1  2# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn17
		 (Sit.Abs.Prg happy_var_1
	)}

happyReduce_15 = happySpecReduce_3  3# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn18
		 (Sit.Abs.Sig happy_var_1 happy_var_3
	)}}

happyReduce_16 = happySpecReduce_3  3# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn18
		 (Sit.Abs.Def happy_var_1 happy_var_3
	)}}

happyReduce_17 = happySpecReduce_3  3# happyReduction_17
happyReduction_17 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_3 of { (HappyWrap19 happy_var_3) -> 
	happyIn18
		 (Sit.Abs.Open happy_var_3
	)}

happyReduce_18 = happySpecReduce_0  3# happyReduction_18
happyReduction_18  =  happyIn18
		 (Sit.Abs.Blank
	)

happyReduce_19 = happySpecReduce_1  4# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn19
		 (Sit.Abs.Sg happy_var_1
	)}

happyReduce_20 = happySpecReduce_3  4# happyReduction_20
happyReduction_20 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	case happyOut15 happy_x_3 of { (HappyWrap15 happy_var_3) -> 
	happyIn19
		 (Sit.Abs.Cons happy_var_1 happy_var_3
	)}}

happyReduce_21 = happySpecReduce_1  5# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn20
		 ((:[]) happy_var_1
	)}

happyReduce_22 = happySpecReduce_3  5# happyReduction_22
happyReduction_22 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	case happyOut20 happy_x_3 of { (HappyWrap20 happy_var_3) -> 
	happyIn20
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn21
		 (Sit.Abs.Id happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  6# happyReduction_24
happyReduction_24 happy_x_1
	 =  happyIn21
		 (Sit.Abs.Under
	)

happyReduce_25 = happySpecReduce_2  7# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn22
		 (Sit.Abs.BIrrel happy_var_2
	)}

happyReduce_26 = happySpecReduce_2  7# happyReduction_26
happyReduction_26 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_2 of { (HappyWrap15 happy_var_2) -> 
	happyIn22
		 (Sit.Abs.BRel happy_var_2
	)}

happyReduce_27 = happyReduce 5# 7# happyReduction_27
happyReduction_27 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn22
		 (Sit.Abs.BAnn happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_28 = happySpecReduce_1  8# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	happyIn23
		 ((:[]) happy_var_1
	)}

happyReduce_29 = happySpecReduce_2  8# happyReduction_29
happyReduction_29 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_1 of { (HappyWrap22 happy_var_1) -> 
	case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	happyIn23
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_30 = happySpecReduce_1  9# happyReduction_30
happyReduction_30 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn24
		 ((:[]) happy_var_1
	)}

happyReduce_31 = happySpecReduce_2  9# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn24
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_32 = happySpecReduce_1  10# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn25
		 ((:[]) happy_var_1
	)}

happyReduce_33 = happySpecReduce_2  10# happyReduction_33
happyReduction_33 happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	happyIn25
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_34 = happySpecReduce_1  11# happyReduction_34
happyReduction_34 happy_x_1
	 =  case happyOut21 happy_x_1 of { (HappyWrap21 happy_var_1) -> 
	happyIn26
		 (Sit.Abs.Var happy_var_1
	)}

happyReduce_35 = happySpecReduce_1  11# happyReduction_35
happyReduction_35 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn26
		 (Sit.Abs.Int happy_var_1
	)}

happyReduce_36 = happySpecReduce_1  11# happyReduction_36
happyReduction_36 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Infty
	)

happyReduce_37 = happySpecReduce_1  11# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Nat
	)

happyReduce_38 = happySpecReduce_1  11# happyReduction_38
happyReduction_38 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Set
	)

happyReduce_39 = happySpecReduce_1  11# happyReduction_39
happyReduction_39 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Set1
	)

happyReduce_40 = happySpecReduce_1  11# happyReduction_40
happyReduction_40 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Set2
	)

happyReduce_41 = happySpecReduce_1  11# happyReduction_41
happyReduction_41 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Zero
	)

happyReduce_42 = happySpecReduce_1  11# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Suc
	)

happyReduce_43 = happySpecReduce_1  11# happyReduction_43
happyReduction_43 happy_x_1
	 =  happyIn26
		 (Sit.Abs.Fix
	)

happyReduce_44 = happySpecReduce_1  11# happyReduction_44
happyReduction_44 happy_x_1
	 =  happyIn26
		 (Sit.Abs.LZero
	)

happyReduce_45 = happySpecReduce_1  11# happyReduction_45
happyReduction_45 happy_x_1
	 =  happyIn26
		 (Sit.Abs.LSuc
	)

happyReduce_46 = happySpecReduce_3  11# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	happyIn26
		 (happy_var_2
	)}

happyReduce_47 = happySpecReduce_2  12# happyReduction_47
happyReduction_47 happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut26 happy_x_2 of { (HappyWrap26 happy_var_2) -> 
	happyIn27
		 (Sit.Abs.App happy_var_1 happy_var_2
	)}}

happyReduce_48 = happySpecReduce_1  12# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_49 = happyReduce 4# 13# happyReduction_49
happyReduction_49 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut25 happy_x_2 of { (HappyWrap25 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn28
		 (Sit.Abs.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_50 = happyReduce 4# 13# happyReduction_50
happyReduction_50 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut23 happy_x_2 of { (HappyWrap23 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	happyIn28
		 (Sit.Abs.Forall happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_51 = happyReduce 7# 13# happyReduction_51
happyReduction_51 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	case happyOut28 happy_x_7 of { (HappyWrap28 happy_var_7) -> 
	happyIn28
		 (Sit.Abs.Pi happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest}}}

happyReduce_52 = happySpecReduce_3  13# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn28
		 (Sit.Abs.Arrow happy_var_1 happy_var_3
	)}}

happyReduce_53 = happyReduce 6# 13# happyReduction_53
happyReduction_53 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut28 happy_x_4 of { (HappyWrap28 happy_var_4) -> 
	case happyOut28 happy_x_6 of { (HappyWrap28 happy_var_6) -> 
	happyIn28
		 (Sit.Abs.Case happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_54 = happySpecReduce_3  13# happyReduction_54
happyReduction_54 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut16 happy_x_3 of { (HappyWrap16 happy_var_3) -> 
	happyIn28
		 (Sit.Abs.Plus happy_var_1 happy_var_3
	)}}

happyReduce_55 = happyReduce 17# 13# happyReduction_55
happyReduction_55 (happy_x_17 `HappyStk`
	happy_x_16 `HappyStk`
	happy_x_15 `HappyStk`
	happy_x_14 `HappyStk`
	happy_x_13 `HappyStk`
	happy_x_12 `HappyStk`
	happy_x_11 `HappyStk`
	happy_x_10 `HappyStk`
	happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_8 of { (HappyWrap28 happy_var_8) -> 
	case happyOut21 happy_x_13 of { (HappyWrap21 happy_var_13) -> 
	case happyOut28 happy_x_16 of { (HappyWrap28 happy_var_16) -> 
	happyIn28
		 (Sit.Abs.ELam happy_var_8 happy_var_13 happy_var_16
	) `HappyStk` happyRest}}}

happyReduce_56 = happySpecReduce_1  13# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn28
		 (happy_var_1
	)}

happyNewToken action sts stk [] =
	happyDoAction 33# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TV happy_dollar_dollar) -> cont 31#;
	PT _ (TI happy_dollar_dollar) -> cont 32#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 33# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = ((>>=))
happyReturn :: () => a -> Either String a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either String a
happyError' = (\(tokens, _) -> happyError tokens)
pPrg tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap17 x') = happyOut17 x} in x'))

pDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap18 x') = happyOut18 x} in x'))

pQualId tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap19 x') = happyOut19 x} in x'))

pListDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap20 x') = happyOut20 x} in x'))

pIdU tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap21 x') = happyOut21 x} in x'))

pBind tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap22 x') = happyOut22 x} in x'))

pListBind tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap23 x') = happyOut23 x} in x'))

pListIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap24 x') = happyOut24 x} in x'))

pListIdU tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap25 x') = happyOut25 x} in x'))

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap26 x') = happyOut26 x} in x'))

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap27 x') = happyOut27 x} in x'))

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap28 x') = happyOut28 x} in x'))

happySeq = happyDontSeq


happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
