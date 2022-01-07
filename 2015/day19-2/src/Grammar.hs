-- Copyright 2021 Google LLC.
-- SPDX-License-Identifier: Apache-2.0

{-# OPTIONS_GHC -w #-}
module Grammar where
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
	= HappyTerminal (Int)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,257) ([0,6664,0,40960,1,32768,161,0,5120,0,16448,2,0,0,0,0,0,0,0,0,0,256,20,16384,580,0,0,0,0,0,0,0,0,0,0,2584,0,0,0,0,0,0,0,0,0,0,0,0,0,4,49152,677,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,677,0,0,0,0,0,0,0,32768,161,0,0,0,0,0,0,0,0,0,0,0,0,16448,2,0,20,0,0,0,0,0,41344,0,0,0,0,0,0,2584,0,41344,0,512,64,8192,0,0,9284,0,16400,1,17408,36,0,0,0,0,0,41344,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,6144,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,8192,0,0,16386,0,32,0,0,0,0,0,0,2584,0,0,0,0,0,8192,0,0,0,0,0,0,512,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","N0","N1","N3","N4","N5","N6","N7","N8","N9","N10","N12","N13","N14","N2","N11","N15","t0","t1","t2","t3","t4","t5","t6","t7","t8","t9","t10","t11","t12","t13","t14","t15","%eof"]
        bit_start = st Prelude.* 36
        bit_end = (st Prelude.+ 1) Prelude.* 36
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..35]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (20) = happyShift action_11
action_0 (26) = happyShift action_5
action_0 (28) = happyShift action_6
action_0 (29) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (9) = happyGoto action_2
action_0 (11) = happyGoto action_9
action_0 (12) = happyGoto action_10
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (26) = happyShift action_5
action_1 (28) = happyShift action_6
action_1 (29) = happyShift action_7
action_1 (9) = happyGoto action_2
action_1 (11) = happyGoto action_3
action_1 (12) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (24) = happyShift action_32
action_2 (25) = happyShift action_33
action_2 (30) = happyShift action_34
action_2 (32) = happyShift action_35
action_2 (7) = happyGoto action_28
action_2 (8) = happyGoto action_29
action_2 (13) = happyGoto action_30
action_2 (14) = happyGoto action_31
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (31) = happyShift action_18
action_3 (33) = happyShift action_24
action_3 (15) = happyGoto action_27
action_3 (18) = happyGoto action_22
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (23) = happyShift action_16
action_4 (31) = happyShift action_18
action_4 (34) = happyShift action_19
action_4 (6) = happyGoto action_25
action_4 (16) = happyGoto action_26
action_4 (18) = happyGoto action_15
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_29

action_6 _ = happyReduce_34

action_7 _ = happyReduce_38

action_8 (36) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (21) = happyShift action_23
action_9 (31) = happyShift action_18
action_9 (33) = happyShift action_24
action_9 (5) = happyGoto action_20
action_9 (15) = happyGoto action_21
action_9 (18) = happyGoto action_22
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_16
action_10 (27) = happyShift action_17
action_10 (31) = happyShift action_18
action_10 (34) = happyShift action_19
action_10 (6) = happyGoto action_12
action_10 (10) = happyGoto action_13
action_10 (16) = happyGoto action_14
action_10 (18) = happyGoto action_15
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_4

action_12 (24) = happyShift action_32
action_12 (25) = happyShift action_33
action_12 (30) = happyShift action_34
action_12 (32) = happyShift action_35
action_12 (7) = happyGoto action_69
action_12 (8) = happyGoto action_70
action_12 (13) = happyGoto action_71
action_12 (14) = happyGoto action_60
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_3

action_14 (23) = happyShift action_16
action_14 (27) = happyShift action_17
action_14 (31) = happyShift action_18
action_14 (34) = happyShift action_19
action_14 (6) = happyGoto action_66
action_14 (10) = happyGoto action_67
action_14 (16) = happyGoto action_68
action_14 (18) = happyGoto action_52
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (24) = happyShift action_32
action_15 (25) = happyShift action_33
action_15 (30) = happyShift action_34
action_15 (32) = happyShift action_35
action_15 (7) = happyGoto action_56
action_15 (8) = happyGoto action_65
action_15 (13) = happyGoto action_59
action_15 (14) = happyGoto action_60
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_11

action_17 _ = happyReduce_32

action_18 _ = happyReduce_51

action_19 _ = happyReduce_49

action_20 _ = happyReduce_2

action_21 (24) = happyShift action_32
action_21 (25) = happyShift action_33
action_21 (30) = happyShift action_34
action_21 (31) = happyShift action_18
action_21 (32) = happyShift action_35
action_21 (7) = happyGoto action_62
action_21 (8) = happyGoto action_63
action_21 (13) = happyGoto action_59
action_21 (14) = happyGoto action_60
action_21 (18) = happyGoto action_64
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (23) = happyShift action_16
action_22 (24) = happyShift action_32
action_22 (25) = happyShift action_33
action_22 (27) = happyShift action_17
action_22 (30) = happyShift action_34
action_22 (32) = happyShift action_35
action_22 (34) = happyShift action_19
action_22 (6) = happyGoto action_55
action_22 (7) = happyGoto action_56
action_22 (8) = happyGoto action_57
action_22 (10) = happyGoto action_58
action_22 (13) = happyGoto action_59
action_22 (14) = happyGoto action_60
action_22 (16) = happyGoto action_61
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_7

action_24 _ = happyReduce_46

action_25 (24) = happyShift action_32
action_25 (30) = happyShift action_34
action_25 (32) = happyShift action_35
action_25 (7) = happyGoto action_53
action_25 (13) = happyGoto action_54
action_25 (14) = happyGoto action_49
action_25 _ = happyReduce_27

action_26 (23) = happyShift action_16
action_26 (31) = happyShift action_18
action_26 (34) = happyShift action_19
action_26 (6) = happyGoto action_50
action_26 (16) = happyGoto action_51
action_26 (18) = happyGoto action_52
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (24) = happyShift action_32
action_27 (30) = happyShift action_34
action_27 (32) = happyShift action_35
action_27 (7) = happyGoto action_47
action_27 (13) = happyGoto action_48
action_27 (14) = happyGoto action_49
action_27 _ = happyReduce_26

action_28 (24) = happyShift action_32
action_28 (25) = happyShift action_33
action_28 (30) = happyShift action_34
action_28 (32) = happyShift action_35
action_28 (7) = happyGoto action_43
action_28 (8) = happyGoto action_44
action_28 (13) = happyGoto action_45
action_28 (14) = happyGoto action_46
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_1

action_30 (23) = happyShift action_16
action_30 (27) = happyShift action_17
action_30 (31) = happyShift action_18
action_30 (34) = happyShift action_19
action_30 (6) = happyGoto action_39
action_30 (10) = happyGoto action_40
action_30 (16) = happyGoto action_41
action_30 (18) = happyGoto action_42
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (21) = happyShift action_23
action_31 (31) = happyShift action_18
action_31 (33) = happyShift action_24
action_31 (5) = happyGoto action_36
action_31 (15) = happyGoto action_37
action_31 (18) = happyGoto action_38
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_18

action_33 _ = happyReduce_22

action_34 _ = happyReduce_42

action_35 _ = happyReduce_44

action_36 _ = happyReduce_21

action_37 (24) = happyShift action_32
action_37 (25) = happyShift action_33
action_37 (30) = happyShift action_34
action_37 (31) = happyShift action_18
action_37 (32) = happyShift action_35
action_37 (7) = happyGoto action_62
action_37 (8) = happyGoto action_63
action_37 (13) = happyGoto action_59
action_37 (14) = happyGoto action_60
action_37 (18) = happyGoto action_64
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (23) = happyShift action_16
action_38 (24) = happyShift action_32
action_38 (25) = happyShift action_33
action_38 (27) = happyShift action_17
action_38 (30) = happyShift action_34
action_38 (32) = happyShift action_35
action_38 (34) = happyShift action_19
action_38 (6) = happyGoto action_55
action_38 (7) = happyGoto action_56
action_38 (8) = happyGoto action_87
action_38 (10) = happyGoto action_88
action_38 (13) = happyGoto action_59
action_38 (14) = happyGoto action_60
action_38 (16) = happyGoto action_61
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (24) = happyShift action_32
action_39 (25) = happyShift action_33
action_39 (30) = happyShift action_34
action_39 (32) = happyShift action_35
action_39 (7) = happyGoto action_69
action_39 (8) = happyGoto action_70
action_39 (13) = happyGoto action_71
action_39 (14) = happyGoto action_60
action_39 _ = happyReduce_13

action_40 _ = happyReduce_20

action_41 (23) = happyShift action_16
action_41 (27) = happyShift action_17
action_41 (31) = happyShift action_18
action_41 (34) = happyShift action_19
action_41 (6) = happyGoto action_66
action_41 (10) = happyGoto action_67
action_41 (16) = happyGoto action_68
action_41 (18) = happyGoto action_52
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (24) = happyShift action_32
action_42 (25) = happyShift action_33
action_42 (30) = happyShift action_34
action_42 (32) = happyShift action_35
action_42 (7) = happyGoto action_56
action_42 (8) = happyGoto action_86
action_42 (13) = happyGoto action_59
action_42 (14) = happyGoto action_60
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (24) = happyShift action_32
action_43 (25) = happyShift action_33
action_43 (30) = happyShift action_34
action_43 (32) = happyShift action_35
action_43 (7) = happyGoto action_43
action_43 (8) = happyGoto action_44
action_43 (13) = happyGoto action_45
action_43 (14) = happyGoto action_46
action_43 _ = happyReduce_12

action_44 _ = happyReduce_19

action_45 (23) = happyShift action_16
action_45 (27) = happyShift action_17
action_45 (31) = happyShift action_18
action_45 (34) = happyShift action_19
action_45 (6) = happyGoto action_39
action_45 (10) = happyGoto action_40
action_45 (16) = happyGoto action_41
action_45 (18) = happyGoto action_42
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (21) = happyShift action_23
action_46 (31) = happyShift action_18
action_46 (33) = happyShift action_24
action_46 (5) = happyGoto action_36
action_46 (15) = happyGoto action_37
action_46 (18) = happyGoto action_38
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (24) = happyShift action_32
action_47 (30) = happyShift action_34
action_47 (32) = happyShift action_35
action_47 (7) = happyGoto action_81
action_47 (13) = happyGoto action_82
action_47 (14) = happyGoto action_83
action_47 _ = happyReduce_45

action_48 (23) = happyShift action_16
action_48 (31) = happyShift action_18
action_48 (34) = happyShift action_19
action_48 (6) = happyGoto action_79
action_48 (16) = happyGoto action_80
action_48 (18) = happyGoto action_42
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (31) = happyShift action_18
action_49 (33) = happyShift action_24
action_49 (15) = happyGoto action_85
action_49 (18) = happyGoto action_38
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (24) = happyShift action_32
action_50 (30) = happyShift action_34
action_50 (32) = happyShift action_35
action_50 (7) = happyGoto action_53
action_50 (13) = happyGoto action_54
action_50 (14) = happyGoto action_49
action_50 _ = happyReduce_9

action_51 (23) = happyShift action_16
action_51 (31) = happyShift action_18
action_51 (34) = happyShift action_19
action_51 (6) = happyGoto action_50
action_51 (16) = happyGoto action_51
action_51 (18) = happyGoto action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (24) = happyShift action_32
action_52 (25) = happyShift action_33
action_52 (30) = happyShift action_34
action_52 (32) = happyShift action_35
action_52 (7) = happyGoto action_56
action_52 (8) = happyGoto action_84
action_52 (13) = happyGoto action_59
action_52 (14) = happyGoto action_60
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (24) = happyShift action_32
action_53 (30) = happyShift action_34
action_53 (32) = happyShift action_35
action_53 (7) = happyGoto action_81
action_53 (13) = happyGoto action_82
action_53 (14) = happyGoto action_83
action_53 _ = happyReduce_8

action_54 (23) = happyShift action_16
action_54 (31) = happyShift action_18
action_54 (34) = happyShift action_19
action_54 (6) = happyGoto action_79
action_54 (16) = happyGoto action_80
action_54 (18) = happyGoto action_42
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (24) = happyShift action_32
action_55 (25) = happyShift action_33
action_55 (30) = happyShift action_34
action_55 (32) = happyShift action_35
action_55 (7) = happyGoto action_69
action_55 (8) = happyGoto action_70
action_55 (13) = happyGoto action_71
action_55 (14) = happyGoto action_60
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (24) = happyShift action_32
action_56 (25) = happyShift action_33
action_56 (30) = happyShift action_34
action_56 (32) = happyShift action_35
action_56 (7) = happyGoto action_43
action_56 (8) = happyGoto action_44
action_56 (13) = happyGoto action_45
action_56 (14) = happyGoto action_46
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (22) = happyShift action_73
action_57 (35) = happyShift action_78
action_57 (17) = happyGoto action_76
action_57 (19) = happyGoto action_77
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (22) = happyShift action_73
action_58 (17) = happyGoto action_75
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (23) = happyShift action_16
action_59 (27) = happyShift action_17
action_59 (31) = happyShift action_18
action_59 (34) = happyShift action_19
action_59 (6) = happyGoto action_39
action_59 (10) = happyGoto action_40
action_59 (16) = happyGoto action_41
action_59 (18) = happyGoto action_42
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_23
action_60 (31) = happyShift action_18
action_60 (33) = happyShift action_24
action_60 (5) = happyGoto action_36
action_60 (15) = happyGoto action_37
action_60 (18) = happyGoto action_38
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (23) = happyShift action_16
action_61 (27) = happyShift action_17
action_61 (31) = happyShift action_18
action_61 (34) = happyShift action_19
action_61 (6) = happyGoto action_66
action_61 (10) = happyGoto action_67
action_61 (16) = happyGoto action_68
action_61 (18) = happyGoto action_52
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (24) = happyShift action_32
action_62 (25) = happyShift action_33
action_62 (30) = happyShift action_34
action_62 (32) = happyShift action_35
action_62 (7) = happyGoto action_43
action_62 (8) = happyGoto action_44
action_62 (13) = happyGoto action_45
action_62 (14) = happyGoto action_46
action_62 _ = happyReduce_45

action_63 _ = happyReduce_5

action_64 (24) = happyShift action_32
action_64 (25) = happyShift action_33
action_64 (30) = happyShift action_34
action_64 (32) = happyShift action_35
action_64 (7) = happyGoto action_56
action_64 (8) = happyGoto action_74
action_64 (13) = happyGoto action_59
action_64 (14) = happyGoto action_60
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (22) = happyShift action_73
action_65 (17) = happyGoto action_72
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (24) = happyShift action_32
action_66 (25) = happyShift action_33
action_66 (30) = happyShift action_34
action_66 (32) = happyShift action_35
action_66 (7) = happyGoto action_69
action_66 (8) = happyGoto action_70
action_66 (13) = happyGoto action_71
action_66 (14) = happyGoto action_60
action_66 _ = happyReduce_9

action_67 _ = happyReduce_31

action_68 (23) = happyShift action_16
action_68 (27) = happyShift action_17
action_68 (31) = happyShift action_18
action_68 (34) = happyShift action_19
action_68 (6) = happyGoto action_66
action_68 (10) = happyGoto action_67
action_68 (16) = happyGoto action_68
action_68 (18) = happyGoto action_52
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (24) = happyShift action_32
action_69 (25) = happyShift action_33
action_69 (30) = happyShift action_34
action_69 (32) = happyShift action_35
action_69 (7) = happyGoto action_43
action_69 (8) = happyGoto action_44
action_69 (13) = happyGoto action_45
action_69 (14) = happyGoto action_46
action_69 _ = happyReduce_8

action_70 _ = happyReduce_30

action_71 (23) = happyShift action_16
action_71 (27) = happyShift action_17
action_71 (31) = happyShift action_18
action_71 (34) = happyShift action_19
action_71 (6) = happyGoto action_39
action_71 (10) = happyGoto action_40
action_71 (16) = happyGoto action_41
action_71 (18) = happyGoto action_42
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_28

action_73 _ = happyReduce_50

action_74 (22) = happyShift action_73
action_74 (17) = happyGoto action_95
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_25

action_76 _ = happyReduce_36

action_77 (24) = happyShift action_32
action_77 (25) = happyShift action_33
action_77 (30) = happyShift action_34
action_77 (32) = happyShift action_35
action_77 (7) = happyGoto action_56
action_77 (8) = happyGoto action_94
action_77 (13) = happyGoto action_59
action_77 (14) = happyGoto action_60
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_52

action_79 (24) = happyShift action_32
action_79 (30) = happyShift action_34
action_79 (32) = happyShift action_35
action_79 (7) = happyGoto action_53
action_79 (13) = happyGoto action_54
action_79 (14) = happyGoto action_49
action_79 _ = happyReduce_13

action_80 (23) = happyShift action_16
action_80 (31) = happyShift action_18
action_80 (34) = happyShift action_19
action_80 (6) = happyGoto action_50
action_80 (16) = happyGoto action_51
action_80 (18) = happyGoto action_52
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (24) = happyShift action_32
action_81 (30) = happyShift action_34
action_81 (32) = happyShift action_35
action_81 (7) = happyGoto action_81
action_81 (13) = happyGoto action_82
action_81 (14) = happyGoto action_83
action_81 _ = happyReduce_12

action_82 (23) = happyShift action_16
action_82 (31) = happyShift action_18
action_82 (34) = happyShift action_19
action_82 (6) = happyGoto action_79
action_82 (16) = happyGoto action_80
action_82 (18) = happyGoto action_42
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (31) = happyShift action_18
action_83 (33) = happyShift action_24
action_83 (15) = happyGoto action_85
action_83 (18) = happyGoto action_38
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (22) = happyShift action_73
action_84 (17) = happyGoto action_93
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (24) = happyShift action_32
action_85 (30) = happyShift action_34
action_85 (32) = happyShift action_35
action_85 (7) = happyGoto action_47
action_85 (13) = happyGoto action_48
action_85 (14) = happyGoto action_49
action_85 _ = happyReduce_17

action_86 (22) = happyShift action_73
action_86 (17) = happyGoto action_92
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (22) = happyShift action_73
action_87 (35) = happyShift action_78
action_87 (17) = happyGoto action_90
action_87 (19) = happyGoto action_91
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (22) = happyShift action_73
action_88 (17) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 _ = happyReduce_16

action_90 _ = happyReduce_41

action_91 (24) = happyShift action_32
action_91 (25) = happyShift action_33
action_91 (30) = happyShift action_34
action_91 (32) = happyShift action_35
action_91 (7) = happyGoto action_56
action_91 (8) = happyGoto action_97
action_91 (13) = happyGoto action_59
action_91 (14) = happyGoto action_60
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_14

action_93 _ = happyReduce_10

action_94 (22) = happyShift action_73
action_94 (17) = happyGoto action_96
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_6

action_96 _ = happyReduce_24

action_97 (22) = happyShift action_73
action_97 (17) = happyGoto action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_15

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn4
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn4
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn4
		 (0
	)

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn5
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn5
		 (0
	)

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn6
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn6
		 (0
	)

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  7 happyReduction_13
happyReduction_13 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 7 happyReduction_14
happyReduction_14 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 7 happyReduction_15
happyReduction_15 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4 + happy_var_5 + happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 7 happyReduction_16
happyReduction_16 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn7
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn7
		 (0
	)

happyReduce_19 = happySpecReduce_2  8 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  8 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  8 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn8
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn8
		 (0
	)

happyReduce_23 = happySpecReduce_2  9 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 6 9 happyReduction_24
happyReduction_24 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4 + happy_var_5 + happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 9 happyReduction_25
happyReduction_25 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  9 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  9 happyReduction_27
happyReduction_27 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 9 happyReduction_28
happyReduction_28 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_30 = happySpecReduce_2  10 happyReduction_30
happyReduction_30 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn10
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  10 happyReduction_31
happyReduction_31 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  10 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn10
		 (0
	)

happyReduce_33 = happySpecReduce_2  11 happyReduction_33
happyReduction_33 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  11 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn11
		 (0
	)

happyReduce_35 = happySpecReduce_2  12 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn12
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 4 12 happyReduction_36
happyReduction_36 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_2  12 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  12 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn12
		 (0
	)

happyReduce_39 = happySpecReduce_2  13 happyReduction_39
happyReduction_39 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn13
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  13 happyReduction_40
happyReduction_40 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 13 happyReduction_41
happyReduction_41 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyAbsSyn14  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (1 + happy_var_1 + happy_var_2 + happy_var_3 + happy_var_4
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  13 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn13
		 (0
	)

happyReduce_43 = happySpecReduce_2  14 happyReduction_43
happyReduction_43 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn14
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  14 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn14
		 (0
	)

happyReduce_45 = happySpecReduce_2  15 happyReduction_45
happyReduction_45 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  15 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn15
		 (0
	)

happyReduce_47 = happySpecReduce_2  16 happyReduction_47
happyReduction_47 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn16
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  16 happyReduction_48
happyReduction_48 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (1 + happy_var_1 + happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  16 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn16
		 (0
	)

happyReduce_50 = happySpecReduce_1  17 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn17
		 (0
	)

happyReduce_51 = happySpecReduce_1  18 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn18
		 (0
	)

happyReduce_52 = happySpecReduce_1  19 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn19
		 (0
	)

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	0 -> cont 20;
	1 -> cont 21;
	2 -> cont 22;
	3 -> cont 23;
	4 -> cont 24;
	5 -> cont 25;
	6 -> cont 26;
	7 -> cont 27;
	8 -> cont 28;
	9 -> cont 29;
	10 -> cont 30;
	11 -> cont 31;
	12 -> cont 32;
	13 -> cont 33;
	14 -> cont 34;
	15 -> cont 35;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 36 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Int)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Int] -> a
parseError tks = error (show tks)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
