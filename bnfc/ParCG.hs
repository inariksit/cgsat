{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module BNFC.ParCG where
import BNFC.AbsCG
import BNFC.LexCG
import BNFC.ErrM
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts

-- parser produced by Happy Version 1.18.10

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn19 :: (String) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> (String)
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (UIdent) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (UIdent)
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: (Id) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> (Id)
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (Signed) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (Signed)
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: (Grammar) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> (Grammar)
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: ([Def]) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> ([Def])
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (Def) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (Def)
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (SetDecl) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (SetDecl)
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (SetName) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (SetName)
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: (Tag) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> (Tag)
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: (Tag) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> (Tag)
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([Tag]) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([Tag])
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (Rule) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (Rule)
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: (Cond) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> (Cond)
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: (Cond) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> (Cond)
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([Cond]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([Cond])
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (TagSet) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (TagSet)
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: (TagSet) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> (TagSet)
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (Position) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (Position)
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: (Barrier) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> (Barrier)
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x13\x01\xd8\x00\xc9\x00\x26\x00\x26\x00\x26\x00\x05\x01\x07\x00\x07\x00\x00\x00\x2c\x00\x2c\x00\xff\xff\xd2\x00\xc4\x00\x00\x00\xb3\x00\x2c\x00\x00\x00\xf7\xff\xbe\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbc\x00\xa8\x00\x2c\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x92\x00\x01\x00\x55\x00\x1e\x00\x07\x00\xff\xff\xa6\x00\xad\x00\x05\x01\xa4\x00\x2c\x00\x2c\x00\x26\x00\xa4\x00\x26\x00\xa4\x00\xa4\x00\xa4\x00\xa4\x00\xa5\x00\xa0\x00\x00\x00\x00\x00\x0c\x01\xa0\x00\x13\x01\x00\x00\xa9\x00\x18\x00\xac\x00\x00\x00\x7a\x00\x6f\x00\x00\x00\x07\x00\x1e\x00\xa3\x00\x9a\x00\x00\x00\x00\x00\x23\x00\x23\x00\x0f\x00\x98\x00\x23\x00\x00\x00\x00\x00\x92\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x26\x00\x9b\x00\x05\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x88\x00\x7d\x00\xda\x00\x70\x00\x58\x00\x12\x01\x07\x01\x03\x01\x4a\x00\xc2\x00\xc7\x00\x60\x00\x94\x00\xaa\x00\x30\x00\x59\x00\x00\x00\x00\x00\x00\x00\x89\x00\x00\x00\x00\x00\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x7e\x00\xba\x00\x0b\x00\x00\x00\x00\x00\x41\x00\x00\x00\x73\x00\x68\x00\xf7\x00\x00\x00\xf3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\xd1\x00\x00\x00\xd1\x00\x00\x00\x00\x00\xe7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x00\x5d\x00\x00\x00\x21\x00\x00\x00\x00\x00\x52\x00\x47\x00\xe7\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1c\x00\x00\x00\x00\x00\x1a\x00\x00\x00\x17\x00\x00\x00\xe2\x00\x00\x00\xb2\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xea\xff\xea\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\x00\x00\x00\x00\xc7\xff\x00\x00\x00\x00\xec\xff\xe4\xff\xe5\xff\xe3\xff\xe2\xff\xdf\xff\xc8\xff\x00\x00\x00\x00\xc9\xff\xee\xff\xed\xff\x00\x00\xce\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\x00\x00\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe8\xff\xe7\xff\x00\x00\x00\x00\xeb\xff\xe9\xff\x00\x00\x00\x00\x00\x00\xdd\xff\x00\x00\x00\x00\xd8\xff\x00\x00\x00\x00\x00\x00\xd4\xff\xc5\xff\xcf\xff\x00\x00\x00\x00\xc8\xff\x00\x00\x00\x00\xc6\xff\xc4\xff\xcc\xff\xcd\xff\xe1\xff\xcb\xff\xca\xff\xd3\xff\xd1\xff\xd5\xff\xd6\xff\xd9\xff\xd0\xff\xda\xff\xd0\xff\xe0\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\xdb\xff\xdc\xff\xe6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x0a\x00\x01\x00\x04\x00\x01\x00\x04\x00\x01\x00\x04\x00\x01\x00\x04\x00\x07\x00\x04\x00\x07\x00\x16\x00\x03\x00\x0e\x00\x01\x00\x0e\x00\x03\x00\x0e\x00\x15\x00\x0e\x00\x15\x00\x16\x00\x15\x00\x01\x00\x15\x00\x03\x00\x15\x00\x12\x00\x0f\x00\x01\x00\x02\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x0f\x00\x01\x00\x0a\x00\x0f\x00\x12\x00\x13\x00\x14\x00\x01\x00\x02\x00\x13\x00\x12\x00\x13\x00\x14\x00\x03\x00\x13\x00\x12\x00\x13\x00\x14\x00\x12\x00\x13\x00\x14\x00\x00\x00\x01\x00\x02\x00\x12\x00\x13\x00\x14\x00\x00\x00\x12\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\x00\x00\x10\x00\x11\x00\x0c\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x08\x00\x0c\x00\x10\x00\x11\x00\x01\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x08\x00\x0c\x00\x10\x00\x11\x00\x03\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x16\x00\x13\x00\x10\x00\x11\x00\x0f\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x07\x00\x07\x00\x10\x00\x11\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x07\x00\x05\x00\x10\x00\x11\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x04\x00\x05\x00\x10\x00\x11\x00\x09\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x05\x00\x06\x00\x10\x00\x11\x00\x03\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x07\x00\x09\x00\x10\x00\x11\x00\x03\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x05\x00\x06\x00\x03\x00\x11\x00\x08\x00\x08\x00\x09\x00\x0a\x00\x03\x00\x16\x00\x03\x00\x13\x00\x0c\x00\x16\x00\x11\x00\x16\x00\x03\x00\x16\x00\x0d\x00\x0e\x00\x0d\x00\x0e\x00\x03\x00\x12\x00\x03\x00\x12\x00\x0d\x00\x0e\x00\x16\x00\x03\x00\x0f\x00\x12\x00\x0d\x00\x0e\x00\x0d\x00\x0e\x00\x00\x00\x12\x00\x15\x00\x12\x00\x0e\x00\x12\x00\x06\x00\x07\x00\x12\x00\x00\x00\x09\x00\x13\x00\x0c\x00\xff\xff\xff\xff\x06\x00\x07\x00\x00\x00\x01\x00\x02\x00\x0d\x00\x0c\x00\x00\x00\x01\x00\x02\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x00\x00\x01\x00\x02\x00\xff\xff\x00\x00\x01\x00\x02\x00\xff\xff\x08\x00\x09\x00\x0a\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x10\x00\x11\x00\x12\x00\xff\xff\x0d\x00\x08\x00\x09\x00\x10\x00\x11\x00\x12\x00\xff\xff\x0d\x00\xff\xff\x16\x00\x10\x00\x11\x00\x12\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x4b\x00\x29\x00\x17\x00\x29\x00\x17\x00\x29\x00\x17\x00\x29\x00\x17\x00\x67\x00\x17\x00\x68\x00\xff\xff\x14\x00\x2a\x00\x33\x00\x2a\x00\x56\x00\x2a\x00\x18\x00\x2a\x00\x18\x00\xff\xff\x18\x00\x33\x00\x18\x00\x56\x00\x18\x00\x47\x00\x51\x00\x20\x00\x21\x00\x12\x00\x22\x00\x23\x00\x20\x00\x21\x00\x63\x00\x33\x00\x4b\x00\x64\x00\x12\x00\x22\x00\x23\x00\x20\x00\x21\x00\x65\x00\x12\x00\x22\x00\x23\x00\x14\x00\x58\x00\x12\x00\x22\x00\x23\x00\x12\x00\x22\x00\x23\x00\x18\x00\x19\x00\x1a\x00\x12\x00\x22\x00\x23\x00\x2c\x00\x15\x00\x1b\x00\x1c\x00\x4e\x00\x41\x00\x18\x00\x19\x00\x1a\x00\x2c\x00\x4f\x00\x24\x00\x45\x00\x19\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x3f\x00\x2d\x00\x56\x00\x24\x00\x19\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x35\x00\x47\x00\x57\x00\x24\x00\x51\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\xff\xff\x12\x00\x5a\x00\x24\x00\x25\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x5d\x00\x36\x00\x43\x00\x24\x00\x5e\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x5f\x00\x3b\x00\x44\x00\x24\x00\x60\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x3c\x00\x3d\x00\x49\x00\x24\x00\x14\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x4d\x00\x4e\x00\x52\x00\x24\x00\x55\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x69\x00\x14\x00\x23\x00\x24\x00\x5a\x00\x1b\x00\x1c\x00\x1d\x00\x18\x00\x19\x00\x1a\x00\x4d\x00\x4e\x00\x61\x00\x53\x00\x62\x00\x1b\x00\x1c\x00\x1d\x00\x14\x00\xff\xff\x14\x00\x22\x00\x47\x00\xff\xff\x1e\x00\xff\xff\x14\x00\xff\xff\x4b\x00\x2b\x00\x5b\x00\x2b\x00\x14\x00\x27\x00\x14\x00\x27\x00\x48\x00\x2b\x00\xff\xff\x14\x00\x51\x00\x27\x00\x4b\x00\x2b\x00\x2a\x00\x2b\x00\x2c\x00\x27\x00\x18\x00\x27\x00\x26\x00\x12\x00\x3e\x00\x39\x00\x27\x00\x2c\x00\x14\x00\x22\x00\x3a\x00\x00\x00\x00\x00\x38\x00\x39\x00\x18\x00\x19\x00\x1a\x00\x38\x00\x3a\x00\x18\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x30\x00\x62\x00\x00\x00\x1b\x00\x1c\x00\x30\x00\x42\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x1b\x00\x1c\x00\x40\x00\x41\x00\x1b\x00\x1c\x00\x30\x00\x42\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x18\x00\x19\x00\x1a\x00\x00\x00\x1b\x00\x1c\x00\x30\x00\x31\x00\x1b\x00\x1c\x00\x33\x00\x18\x00\x19\x00\x1a\x00\x2f\x00\x30\x00\x12\x00\x00\x00\x38\x00\x1b\x00\x34\x00\x2f\x00\x30\x00\x12\x00\x00\x00\x38\x00\x00\x00\xff\xff\x2f\x00\x30\x00\x12\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (16, 59) [
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
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59)
	]

happy_n_terms = 23 :: Int
happy_n_nonterms = 20 :: Int

happyReduce_16 = happySpecReduce_1  0# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn19
		 (happy_var_1
	)}

happyReduce_17 = happySpecReduce_1  1# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_UIdent happy_var_1)) -> 
	happyIn20
		 (UIdent (happy_var_1)
	)}

happyReduce_18 = happySpecReduce_1  2# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Id happy_var_1)) -> 
	happyIn21
		 (Id (happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  3# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Signed happy_var_1)) -> 
	happyIn22
		 (Signed (happy_var_1)
	)}

happyReduce_20 = happySpecReduce_1  4# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 (Defs (reverse happy_var_1)
	)}

happyReduce_21 = happySpecReduce_0  5# happyReduction_21
happyReduction_21  =  happyIn24
		 ([]
	)

happyReduce_22 = happySpecReduce_2  5# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	case happyOut25 happy_x_2 of { happy_var_2 -> 
	happyIn24
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_23 = happySpecReduce_1  6# happyReduction_23
happyReduction_23 happy_x_1
	 =  case happyOut26 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (SetDef happy_var_1
	)}

happyReduce_24 = happySpecReduce_1  6# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn25
		 (RuleDef happy_var_1
	)}

happyReduce_25 = happyReduce 5# 7# happyReduction_25
happyReduction_25 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut27 happy_x_2 of { happy_var_2 -> 
	case happyOut30 happy_x_4 of { happy_var_4 -> 
	happyIn26
		 (Set happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_26 = happySpecReduce_1  8# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn27
		 (SetName happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  9# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (Lemma happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  9# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (Tag happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  9# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut27 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 (Named happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  9# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (happy_var_2
	)}

happyReduce_31 = happySpecReduce_3  9# happyReduction_31
happyReduction_31 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn28
		 (AND happy_var_2
	)}

happyReduce_32 = happySpecReduce_1  10# happyReduction_32
happyReduction_32 happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	happyIn29
		 (happy_var_1
	)}

happyReduce_33 = happySpecReduce_1  11# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_2  11# happyReduction_34
happyReduction_34 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut30 happy_x_2 of { happy_var_2 -> 
	happyIn30
		 ((:) happy_var_1 happy_var_2
	)}}

happyReduce_35 = happyReduce 5# 12# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut34 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (SelectIf happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_36 = happyReduce 5# 12# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut34 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (RemoveIf happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_37 = happySpecReduce_3  12# happyReduction_37
happyReduction_37 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (SelectAlways happy_var_2
	)}

happyReduce_38 = happySpecReduce_3  12# happyReduction_38
happyReduction_38 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (RemoveAlways happy_var_2
	)}

happyReduce_39 = happySpecReduce_2  12# happyReduction_39
happyReduction_39 happy_x_2
	happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_2 of { happy_var_2 -> 
	happyIn31
		 (MatchLemma happy_var_1 happy_var_2
	)}}

happyReduce_40 = happySpecReduce_1  13# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (C happy_var_1
	)}

happyReduce_41 = happySpecReduce_3  14# happyReduction_41
happyReduction_41 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (Linked happy_var_1 happy_var_3
	)}}

happyReduce_42 = happySpecReduce_3  14# happyReduction_42
happyReduction_42 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (CNotPos happy_var_2 happy_var_3
	)}}

happyReduce_43 = happySpecReduce_2  14# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (CPos happy_var_1 happy_var_2
	)}}

happyReduce_44 = happySpecReduce_3  14# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_2 of { happy_var_2 -> 
	case happyOut38 happy_x_3 of { happy_var_3 -> 
	happyIn33
		 (CBarrier happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_45 = happyReduce 4# 14# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut37 happy_x_2 of { happy_var_2 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	case happyOut38 happy_x_4 of { happy_var_4 -> 
	happyIn33
		 (CNotBar happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_46 = happySpecReduce_3  14# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn33
		 (happy_var_2
	)}

happyReduce_47 = happySpecReduce_0  15# happyReduction_47
happyReduction_47  =  happyIn34
		 ([]
	)

happyReduce_48 = happySpecReduce_2  15# happyReduction_48
happyReduction_48 happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_2 of { happy_var_2 -> 
	happyIn34
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_49 = happySpecReduce_1  16# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (TagSet happy_var_1
	)}

happyReduce_50 = happySpecReduce_3  17# happyReduction_50
happyReduction_50 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (happy_var_2
	)}

happyReduce_51 = happySpecReduce_3  17# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	case happyOut36 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (OR happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_3  17# happyReduction_52
happyReduction_52 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (Diff happy_var_1 happy_var_3
	)}}

happyReduce_53 = happySpecReduce_3  17# happyReduction_53
happyReduction_53 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn36
		 (Cart happy_var_1 happy_var_3
	)}}

happyReduce_54 = happySpecReduce_1  17# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn36
		 (All
	)

happyReduce_55 = happySpecReduce_1  17# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut29 happy_x_1 of { happy_var_1 -> 
	happyIn36
		 (NilT happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  18# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (Exactly happy_var_1
	)}

happyReduce_57 = happySpecReduce_2  18# happyReduction_57
happyReduction_57 happy_x_2
	happy_x_1
	 =  case happyOut22 happy_x_2 of { happy_var_2 -> 
	happyIn37
		 (AtLeast happy_var_2
	)}

happyReduce_58 = happySpecReduce_2  18# happyReduction_58
happyReduction_58 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn37
		 (happy_var_1
	)}

happyReduce_59 = happySpecReduce_2  19# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { happy_var_2 -> 
	happyIn38
		 (Barrier happy_var_2
	)}

happyNewToken action sts stk [] =
	happyDoAction 22# notHappyAtAll action sts stk []

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
	PT _ (TL happy_dollar_dollar) -> cont 18#;
	PT _ (T_UIdent happy_dollar_dollar) -> cont 19#;
	PT _ (T_Id happy_dollar_dollar) -> cont 20#;
	PT _ (T_Signed happy_dollar_dollar) -> cont 21#;
	_ -> happyError' (tk:tks)
	}

happyError_ 22# tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pGrammar tks = happySomeParser where
  happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (happyOut23 x))

pListDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (happyOut24 x))

pDef tks = happySomeParser where
  happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (happyOut25 x))

pSetDecl tks = happySomeParser where
  happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (happyOut26 x))

pSetName tks = happySomeParser where
  happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (happyOut27 x))

pTag1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (happyOut28 x))

pTag tks = happySomeParser where
  happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (happyOut29 x))

pListTag tks = happySomeParser where
  happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (happyOut30 x))

pRule tks = happySomeParser where
  happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (happyOut31 x))

pCond tks = happySomeParser where
  happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (happyOut32 x))

pCond1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (happyOut33 x))

pListCond tks = happySomeParser where
  happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (happyOut34 x))

pTagSet tks = happySomeParser where
  happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (happyOut35 x))

pTagSet1 tks = happySomeParser where
  happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (happyOut36 x))

pPosition tks = happySomeParser where
  happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (happyOut37 x))

pBarrier tks = happySomeParser where
  happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (happyOut38 x))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
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
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n Happy_GHC_Exts.<# (0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

				     (happyReduceArr Happy_Data_Array.! rule) i tk st
				     where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where (new_state) = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where (off)    = indexShortOffAddr happyActOffsets st
         (off_i)  = (off Happy_GHC_Exts.+# i)
	 check  = if (off_i Happy_GHC_Exts.>=# (0# :: Happy_GHC_Exts.Int#))
			then (indexShortOffAddr happyCheck off_i Happy_GHC_Exts.==#  i)
			else False
         (action)
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st

{-# LINE 130 "templates/GenericTemplate.hs" #-}


indexShortOffAddr (HappyA# arr) off =
	Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 163 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@((HappyCons (st1@(action)) (_)))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

             (off) = indexShortOffAddr happyGotoOffsets st1
             (off_i) = (off Happy_GHC_Exts.+# nt)
             (new_state) = indexShortOffAddr happyTable off_i




happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where (off) = indexShortOffAddr happyGotoOffsets st
         (off_i) = (off Happy_GHC_Exts.+# nt)
         (new_state) = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

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
