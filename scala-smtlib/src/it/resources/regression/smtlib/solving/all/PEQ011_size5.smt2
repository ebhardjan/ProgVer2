(set-option :print-success true)
(set-logic QF_UF)
(set-info :source |
CADE ATP System competition. See http://www.cs.miami.edu/~tptp/CASC
 for more information. 

This benchmark was obtained by trying to find a finite model of a first-order 
formula (Albert Oliveras).
|)
(set-info :smt-lib-version 2.0)
(set-info :category "crafted")
(set-info :status unsat)
(declare-sort U 0)
(declare-fun f1 (U U) U)
(declare-fun c2 () U)
(declare-fun c3 () U)
(declare-fun c_0 () U)
(declare-fun c_1 () U)
(declare-fun c_2 () U)
(declare-fun c_3 () U)
(declare-fun c_4 () U)
(assert (let ((?v_1 (= c_0 c_0)) (?v_0 (f1 c_0 c_0))) (let ((?v_55 (or ?v_1 (not (= ?v_0 ?v_0)))) (?v_2 (f1 c_0 c_1))) (let ((?v_60 (not (= ?v_2 ?v_2))) (?v_3 (f1 c_0 c_2))) (let ((?v_65 (not (= ?v_3 ?v_3))) (?v_4 (f1 c_0 c_3))) (let ((?v_70 (not (= ?v_4 ?v_4))) (?v_5 (f1 c_0 c_4))) (let ((?v_75 (not (= ?v_5 ?v_5))) (?v_6 (= c_0 c_1)) (?v_10 (f1 c_1 c_0)) (?v_12 (f1 c_1 c_1)) (?v_13 (f1 c_1 c_2)) (?v_14 (f1 c_1 c_3)) (?v_15 (f1 c_1 c_4)) (?v_7 (= c_0 c_2)) (?v_17 (f1 c_2 c_0)) (?v_19 (f1 c_2 c_1)) (?v_20 (f1 c_2 c_2)) (?v_21 (f1 c_2 c_3)) (?v_22 (f1 c_2 c_4)) (?v_8 (= c_0 c_3)) (?v_23 (f1 c_3 c_0)) (?v_25 (f1 c_3 c_1)) (?v_26 (f1 c_3 c_2)) (?v_27 (f1 c_3 c_3)) (?v_28 (f1 c_3 c_4)) (?v_9 (= c_0 c_4)) (?v_29 (f1 c_4 c_0)) (?v_31 (f1 c_4 c_1)) (?v_32 (f1 c_4 c_2)) (?v_33 (f1 c_4 c_3)) (?v_34 (f1 c_4 c_4)) (?v_11 (= c_1 c_0)) (?v_16 (= c_1 c_1))) (let ((?v_56 (not (= ?v_10 ?v_10))) (?v_61 (or ?v_16 (not (= ?v_12 ?v_12)))) (?v_66 (not (= ?v_13 ?v_13))) (?v_71 (not (= ?v_14 ?v_14))) (?v_76 (not (= ?v_15 ?v_15))) (?v_18 (= c_1 c_2)) (?v_24 (= c_1 c_3)) (?v_30 (= c_1 c_4)) (?v_35 (= c_2 c_0)) (?v_36 (= c_2 c_1)) (?v_37 (= c_2 c_2)) (?v_57 (not (= ?v_17 ?v_17))) (?v_62 (not (= ?v_19 ?v_19)))) (let ((?v_67 (or ?v_37 (not (= ?v_20 ?v_20)))) (?v_72 (not (= ?v_21 ?v_21))) (?v_77 (not (= ?v_22 ?v_22))) (?v_38 (= c_2 c_3)) (?v_39 (= c_2 c_4)) (?v_40 (= c_3 c_0)) (?v_41 (= c_3 c_1)) (?v_42 (= c_3 c_2)) (?v_43 (= c_3 c_3)) (?v_58 (not (= ?v_23 ?v_23))) (?v_63 (not (= ?v_25 ?v_25))) (?v_68 (not (= ?v_26 ?v_26)))) (let ((?v_73 (or ?v_43 (not (= ?v_27 ?v_27)))) (?v_78 (not (= ?v_28 ?v_28))) (?v_44 (= c_3 c_4)) (?v_45 (= c_4 c_0)) (?v_46 (= c_4 c_1)) (?v_47 (= c_4 c_2)) (?v_48 (= c_4 c_3)) (?v_49 (= c_4 c_4)) (?v_59 (not (= ?v_29 ?v_29))) (?v_64 (not (= ?v_31 ?v_31))) (?v_69 (not (= ?v_32 ?v_32))) (?v_74 (not (= ?v_33 ?v_33)))) (let ((?v_79 (or ?v_49 (not (= ?v_34 ?v_34)))) (?v_80 (f1 c_0 ?v_0))) (let ((?v_50 (f1 c_0 (f1 c_0 ?v_80))) (?v_82 (f1 c_0 ?v_12)) (?v_81 (f1 c_0 ?v_10)) (?v_84 (f1 c_0 ?v_20)) (?v_83 (f1 c_0 ?v_17)) (?v_86 (f1 c_0 ?v_27)) (?v_85 (f1 c_0 ?v_23)) (?v_88 (f1 c_0 ?v_34)) (?v_87 (f1 c_0 ?v_29)) (?v_89 (f1 c_1 ?v_0)) (?v_90 (f1 c_1 ?v_2)) (?v_91 (f1 c_1 ?v_12))) (let ((?v_51 (f1 c_1 (f1 c_1 ?v_91))) (?v_93 (f1 c_1 ?v_20)) (?v_92 (f1 c_1 ?v_19)) (?v_95 (f1 c_1 ?v_27)) (?v_94 (f1 c_1 ?v_25)) (?v_97 (f1 c_1 ?v_34)) (?v_96 (f1 c_1 ?v_31)) (?v_98 (f1 c_2 ?v_0)) (?v_99 (f1 c_2 ?v_3)) (?v_100 (f1 c_2 ?v_12)) (?v_101 (f1 c_2 ?v_13)) (?v_102 (f1 c_2 ?v_20))) (let ((?v_52 (f1 c_2 (f1 c_2 ?v_102))) (?v_104 (f1 c_2 ?v_27)) (?v_103 (f1 c_2 ?v_26)) (?v_106 (f1 c_2 ?v_34)) (?v_105 (f1 c_2 ?v_32)) (?v_107 (f1 c_3 ?v_0)) (?v_108 (f1 c_3 ?v_4)) (?v_109 (f1 c_3 ?v_12)) (?v_110 (f1 c_3 ?v_14)) (?v_111 (f1 c_3 ?v_20)) (?v_112 (f1 c_3 ?v_21)) (?v_113 (f1 c_3 ?v_27))) (let ((?v_53 (f1 c_3 (f1 c_3 ?v_113))) (?v_115 (f1 c_3 ?v_34)) (?v_114 (f1 c_3 ?v_33)) (?v_116 (f1 c_4 ?v_0)) (?v_117 (f1 c_4 ?v_5)) (?v_118 (f1 c_4 ?v_12)) (?v_119 (f1 c_4 ?v_15)) (?v_120 (f1 c_4 ?v_20)) (?v_121 (f1 c_4 ?v_22)) (?v_122 (f1 c_4 ?v_27)) (?v_123 (f1 c_4 ?v_28)) (?v_124 (f1 c_4 ?v_34))) (let ((?v_54 (f1 c_4 (f1 c_4 ?v_124)))) (and (distinct c_0 c_1 c_2 c_3 c_4) ?v_55 (or ?v_1 ?v_60) (or ?v_1 ?v_65) (or ?v_1 ?v_70) (or ?v_1 ?v_75) (or ?v_6 (not (= ?v_0 ?v_10))) (or ?v_6 (not (= ?v_2 ?v_12))) (or ?v_6 (not (= ?v_3 ?v_13))) (or ?v_6 (not (= ?v_4 ?v_14))) (or ?v_6 (not (= ?v_5 ?v_15))) (or ?v_7 (not (= ?v_0 ?v_17))) (or ?v_7 (not (= ?v_2 ?v_19))) (or ?v_7 (not (= ?v_3 ?v_20))) (or ?v_7 (not (= ?v_4 ?v_21))) (or ?v_7 (not (= ?v_5 ?v_22))) (or ?v_8 (not (= ?v_0 ?v_23))) (or ?v_8 (not (= ?v_2 ?v_25))) (or ?v_8 (not (= ?v_3 ?v_26))) (or ?v_8 (not (= ?v_4 ?v_27))) (or ?v_8 (not (= ?v_5 ?v_28))) (or ?v_9 (not (= ?v_0 ?v_29))) (or ?v_9 (not (= ?v_2 ?v_31))) (or ?v_9 (not (= ?v_3 ?v_32))) (or ?v_9 (not (= ?v_4 ?v_33))) (or ?v_9 (not (= ?v_5 ?v_34))) (or ?v_11 (not (= ?v_10 ?v_0))) (or ?v_11 (not (= ?v_12 ?v_2))) (or ?v_11 (not (= ?v_13 ?v_3))) (or ?v_11 (not (= ?v_14 ?v_4))) (or ?v_11 (not (= ?v_15 ?v_5))) (or ?v_16 ?v_56) ?v_61 (or ?v_16 ?v_66) (or ?v_16 ?v_71) (or ?v_16 ?v_76) (or ?v_18 (not (= ?v_10 ?v_17))) (or ?v_18 (not (= ?v_12 ?v_19))) (or ?v_18 (not (= ?v_13 ?v_20))) (or ?v_18 (not (= ?v_14 ?v_21))) (or ?v_18 (not (= ?v_15 ?v_22))) (or ?v_24 (not (= ?v_10 ?v_23))) (or ?v_24 (not (= ?v_12 ?v_25))) (or ?v_24 (not (= ?v_13 ?v_26))) (or ?v_24 (not (= ?v_14 ?v_27))) (or ?v_24 (not (= ?v_15 ?v_28))) (or ?v_30 (not (= ?v_10 ?v_29))) (or ?v_30 (not (= ?v_12 ?v_31))) (or ?v_30 (not (= ?v_13 ?v_32))) (or ?v_30 (not (= ?v_14 ?v_33))) (or ?v_30 (not (= ?v_15 ?v_34))) (or ?v_35 (not (= ?v_17 ?v_0))) (or ?v_35 (not (= ?v_19 ?v_2))) (or ?v_35 (not (= ?v_20 ?v_3))) (or ?v_35 (not (= ?v_21 ?v_4))) (or ?v_35 (not (= ?v_22 ?v_5))) (or ?v_36 (not (= ?v_17 ?v_10))) (or ?v_36 (not (= ?v_19 ?v_12))) (or ?v_36 (not (= ?v_20 ?v_13))) (or ?v_36 (not (= ?v_21 ?v_14))) (or ?v_36 (not (= ?v_22 ?v_15))) (or ?v_37 ?v_57) (or ?v_37 ?v_62) ?v_67 (or ?v_37 ?v_72) (or ?v_37 ?v_77) (or ?v_38 (not (= ?v_17 ?v_23))) (or ?v_38 (not (= ?v_19 ?v_25))) (or ?v_38 (not (= ?v_20 ?v_26))) (or ?v_38 (not (= ?v_21 ?v_27))) (or ?v_38 (not (= ?v_22 ?v_28))) (or ?v_39 (not (= ?v_17 ?v_29))) (or ?v_39 (not (= ?v_19 ?v_31))) (or ?v_39 (not (= ?v_20 ?v_32))) (or ?v_39 (not (= ?v_21 ?v_33))) (or ?v_39 (not (= ?v_22 ?v_34))) (or ?v_40 (not (= ?v_23 ?v_0))) (or ?v_40 (not (= ?v_25 ?v_2))) (or ?v_40 (not (= ?v_26 ?v_3))) (or ?v_40 (not (= ?v_27 ?v_4))) (or ?v_40 (not (= ?v_28 ?v_5))) (or ?v_41 (not (= ?v_23 ?v_10))) (or ?v_41 (not (= ?v_25 ?v_12))) (or ?v_41 (not (= ?v_26 ?v_13))) (or ?v_41 (not (= ?v_27 ?v_14))) (or ?v_41 (not (= ?v_28 ?v_15))) (or ?v_42 (not (= ?v_23 ?v_17))) (or ?v_42 (not (= ?v_25 ?v_19))) (or ?v_42 (not (= ?v_26 ?v_20))) (or ?v_42 (not (= ?v_27 ?v_21))) (or ?v_42 (not (= ?v_28 ?v_22))) (or ?v_43 ?v_58) (or ?v_43 ?v_63) (or ?v_43 ?v_68) ?v_73 (or ?v_43 ?v_78) (or ?v_44 (not (= ?v_23 ?v_29))) (or ?v_44 (not (= ?v_25 ?v_31))) (or ?v_44 (not (= ?v_26 ?v_32))) (or ?v_44 (not (= ?v_27 ?v_33))) (or ?v_44 (not (= ?v_28 ?v_34))) (or ?v_45 (not (= ?v_29 ?v_0))) (or ?v_45 (not (= ?v_31 ?v_2))) (or ?v_45 (not (= ?v_32 ?v_3))) (or ?v_45 (not (= ?v_33 ?v_4))) (or ?v_45 (not (= ?v_34 ?v_5))) (or ?v_46 (not (= ?v_29 ?v_10))) (or ?v_46 (not (= ?v_31 ?v_12))) (or ?v_46 (not (= ?v_32 ?v_13))) (or ?v_46 (not (= ?v_33 ?v_14))) (or ?v_46 (not (= ?v_34 ?v_15))) (or ?v_47 (not (= ?v_29 ?v_17))) (or ?v_47 (not (= ?v_31 ?v_19))) (or ?v_47 (not (= ?v_32 ?v_20))) (or ?v_47 (not (= ?v_33 ?v_21))) (or ?v_47 (not (= ?v_34 ?v_22))) (or ?v_48 (not (= ?v_29 ?v_23))) (or ?v_48 (not (= ?v_31 ?v_25))) (or ?v_48 (not (= ?v_32 ?v_26))) (or ?v_48 (not (= ?v_33 ?v_27))) (or ?v_48 (not (= ?v_34 ?v_28))) (or ?v_49 ?v_59) (or ?v_49 ?v_64) (or ?v_49 ?v_69) (or ?v_49 ?v_74) ?v_79 (not (= (f1 c2 (f1 c2 (f1 c3 (f1 c3 c3)))) (f1 c3 (f1 c2 (f1 c3 (f1 c3 c2)))))) (= ?v_50 ?v_50) (= (f1 c_0 (f1 c_0 ?v_82)) (f1 c_1 (f1 c_0 ?v_81))) (= (f1 c_0 (f1 c_0 ?v_84)) (f1 c_2 (f1 c_0 ?v_83))) (= (f1 c_0 (f1 c_0 ?v_86)) (f1 c_3 (f1 c_0 ?v_85))) (= (f1 c_0 (f1 c_0 ?v_88)) (f1 c_4 (f1 c_0 ?v_87))) (= (f1 c_1 (f1 c_1 ?v_89)) (f1 c_0 (f1 c_1 ?v_90))) (= ?v_51 ?v_51) (= (f1 c_1 (f1 c_1 ?v_93)) (f1 c_2 (f1 c_1 ?v_92))) (= (f1 c_1 (f1 c_1 ?v_95)) (f1 c_3 (f1 c_1 ?v_94))) (= (f1 c_1 (f1 c_1 ?v_97)) (f1 c_4 (f1 c_1 ?v_96))) (= (f1 c_2 (f1 c_2 ?v_98)) (f1 c_0 (f1 c_2 ?v_99))) (= (f1 c_2 (f1 c_2 ?v_100)) (f1 c_1 (f1 c_2 ?v_101))) (= ?v_52 ?v_52) (= (f1 c_2 (f1 c_2 ?v_104)) (f1 c_3 (f1 c_2 ?v_103))) (= (f1 c_2 (f1 c_2 ?v_106)) (f1 c_4 (f1 c_2 ?v_105))) (= (f1 c_3 (f1 c_3 ?v_107)) (f1 c_0 (f1 c_3 ?v_108))) (= (f1 c_3 (f1 c_3 ?v_109)) (f1 c_1 (f1 c_3 ?v_110))) (= (f1 c_3 (f1 c_3 ?v_111)) (f1 c_2 (f1 c_3 ?v_112))) (= ?v_53 ?v_53) (= (f1 c_3 (f1 c_3 ?v_115)) (f1 c_4 (f1 c_3 ?v_114))) (= (f1 c_4 (f1 c_4 ?v_116)) (f1 c_0 (f1 c_4 ?v_117))) (= (f1 c_4 (f1 c_4 ?v_118)) (f1 c_1 (f1 c_4 ?v_119))) (= (f1 c_4 (f1 c_4 ?v_120)) (f1 c_2 (f1 c_4 ?v_121))) (= (f1 c_4 (f1 c_4 ?v_122)) (f1 c_3 (f1 c_4 ?v_123))) (= ?v_54 ?v_54) ?v_55 (or ?v_1 ?v_56) (or ?v_1 ?v_57) (or ?v_1 ?v_58) (or ?v_1 ?v_59) (or ?v_6 (not (= ?v_0 ?v_2))) (or ?v_6 (not (= ?v_10 ?v_12))) (or ?v_6 (not (= ?v_17 ?v_19))) (or ?v_6 (not (= ?v_23 ?v_25))) (or ?v_6 (not (= ?v_29 ?v_31))) (or ?v_7 (not (= ?v_0 ?v_3))) (or ?v_7 (not (= ?v_10 ?v_13))) (or ?v_7 (not (= ?v_17 ?v_20))) (or ?v_7 (not (= ?v_23 ?v_26))) (or ?v_7 (not (= ?v_29 ?v_32))) (or ?v_8 (not (= ?v_0 ?v_4))) (or ?v_8 (not (= ?v_10 ?v_14))) (or ?v_8 (not (= ?v_17 ?v_21))) (or ?v_8 (not (= ?v_23 ?v_27))) (or ?v_8 (not (= ?v_29 ?v_33))) (or ?v_9 (not (= ?v_0 ?v_5))) (or ?v_9 (not (= ?v_10 ?v_15))) (or ?v_9 (not (= ?v_17 ?v_22))) (or ?v_9 (not (= ?v_23 ?v_28))) (or ?v_9 (not (= ?v_29 ?v_34))) (or ?v_11 (not (= ?v_2 ?v_0))) (or ?v_11 (not (= ?v_12 ?v_10))) (or ?v_11 (not (= ?v_19 ?v_17))) (or ?v_11 (not (= ?v_25 ?v_23))) (or ?v_11 (not (= ?v_31 ?v_29))) (or ?v_16 ?v_60) ?v_61 (or ?v_16 ?v_62) (or ?v_16 ?v_63) (or ?v_16 ?v_64) (or ?v_18 (not (= ?v_2 ?v_3))) (or ?v_18 (not (= ?v_12 ?v_13))) (or ?v_18 (not (= ?v_19 ?v_20))) (or ?v_18 (not (= ?v_25 ?v_26))) (or ?v_18 (not (= ?v_31 ?v_32))) (or ?v_24 (not (= ?v_2 ?v_4))) (or ?v_24 (not (= ?v_12 ?v_14))) (or ?v_24 (not (= ?v_19 ?v_21))) (or ?v_24 (not (= ?v_25 ?v_27))) (or ?v_24 (not (= ?v_31 ?v_33))) (or ?v_30 (not (= ?v_2 ?v_5))) (or ?v_30 (not (= ?v_12 ?v_15))) (or ?v_30 (not (= ?v_19 ?v_22))) (or ?v_30 (not (= ?v_25 ?v_28))) (or ?v_30 (not (= ?v_31 ?v_34))) (or ?v_35 (not (= ?v_3 ?v_0))) (or ?v_35 (not (= ?v_13 ?v_10))) (or ?v_35 (not (= ?v_20 ?v_17))) (or ?v_35 (not (= ?v_26 ?v_23))) (or ?v_35 (not (= ?v_32 ?v_29))) (or ?v_36 (not (= ?v_3 ?v_2))) (or ?v_36 (not (= ?v_13 ?v_12))) (or ?v_36 (not (= ?v_20 ?v_19))) (or ?v_36 (not (= ?v_26 ?v_25))) (or ?v_36 (not (= ?v_32 ?v_31))) (or ?v_37 ?v_65) (or ?v_37 ?v_66) ?v_67 (or ?v_37 ?v_68) (or ?v_37 ?v_69) (or ?v_38 (not (= ?v_3 ?v_4))) (or ?v_38 (not (= ?v_13 ?v_14))) (or ?v_38 (not (= ?v_20 ?v_21))) (or ?v_38 (not (= ?v_26 ?v_27))) (or ?v_38 (not (= ?v_32 ?v_33))) (or ?v_39 (not (= ?v_3 ?v_5))) (or ?v_39 (not (= ?v_13 ?v_15))) (or ?v_39 (not (= ?v_20 ?v_22))) (or ?v_39 (not (= ?v_26 ?v_28))) (or ?v_39 (not (= ?v_32 ?v_34))) (or ?v_40 (not (= ?v_4 ?v_0))) (or ?v_40 (not (= ?v_14 ?v_10))) (or ?v_40 (not (= ?v_21 ?v_17))) (or ?v_40 (not (= ?v_27 ?v_23))) (or ?v_40 (not (= ?v_33 ?v_29))) (or ?v_41 (not (= ?v_4 ?v_2))) (or ?v_41 (not (= ?v_14 ?v_12))) (or ?v_41 (not (= ?v_21 ?v_19))) (or ?v_41 (not (= ?v_27 ?v_25))) (or ?v_41 (not (= ?v_33 ?v_31))) (or ?v_42 (not (= ?v_4 ?v_3))) (or ?v_42 (not (= ?v_14 ?v_13))) (or ?v_42 (not (= ?v_21 ?v_20))) (or ?v_42 (not (= ?v_27 ?v_26))) (or ?v_42 (not (= ?v_33 ?v_32))) (or ?v_43 ?v_70) (or ?v_43 ?v_71) (or ?v_43 ?v_72) ?v_73 (or ?v_43 ?v_74) (or ?v_44 (not (= ?v_4 ?v_5))) (or ?v_44 (not (= ?v_14 ?v_15))) (or ?v_44 (not (= ?v_21 ?v_22))) (or ?v_44 (not (= ?v_27 ?v_28))) (or ?v_44 (not (= ?v_33 ?v_34))) (or ?v_45 (not (= ?v_5 ?v_0))) (or ?v_45 (not (= ?v_15 ?v_10))) (or ?v_45 (not (= ?v_22 ?v_17))) (or ?v_45 (not (= ?v_28 ?v_23))) (or ?v_45 (not (= ?v_34 ?v_29))) (or ?v_46 (not (= ?v_5 ?v_2))) (or ?v_46 (not (= ?v_15 ?v_12))) (or ?v_46 (not (= ?v_22 ?v_19))) (or ?v_46 (not (= ?v_28 ?v_25))) (or ?v_46 (not (= ?v_34 ?v_31))) (or ?v_47 (not (= ?v_5 ?v_3))) (or ?v_47 (not (= ?v_15 ?v_13))) (or ?v_47 (not (= ?v_22 ?v_20))) (or ?v_47 (not (= ?v_28 ?v_26))) (or ?v_47 (not (= ?v_34 ?v_32))) (or ?v_48 (not (= ?v_5 ?v_4))) (or ?v_48 (not (= ?v_15 ?v_14))) (or ?v_48 (not (= ?v_22 ?v_21))) (or ?v_48 (not (= ?v_28 ?v_27))) (or ?v_48 (not (= ?v_34 ?v_33))) (or ?v_49 ?v_75) (or ?v_49 ?v_76) (or ?v_49 ?v_77) (or ?v_49 ?v_78) ?v_79 (= (f1 ?v_0 c_0) ?v_80) (= (f1 ?v_0 c_1) (f1 c_0 ?v_2)) (= (f1 ?v_0 c_2) (f1 c_0 ?v_3)) (= (f1 ?v_0 c_3) (f1 c_0 ?v_4)) (= (f1 ?v_0 c_4) (f1 c_0 ?v_5)) (= (f1 ?v_2 c_0) ?v_81) (= (f1 ?v_2 c_1) ?v_82) (= (f1 ?v_2 c_2) (f1 c_0 ?v_13)) (= (f1 ?v_2 c_3) (f1 c_0 ?v_14)) (= (f1 ?v_2 c_4) (f1 c_0 ?v_15)) (= (f1 ?v_3 c_0) ?v_83) (= (f1 ?v_3 c_1) (f1 c_0 ?v_19)) (= (f1 ?v_3 c_2) ?v_84) (= (f1 ?v_3 c_3) (f1 c_0 ?v_21)) (= (f1 ?v_3 c_4) (f1 c_0 ?v_22)) (= (f1 ?v_4 c_0) ?v_85) (= (f1 ?v_4 c_1) (f1 c_0 ?v_25)) (= (f1 ?v_4 c_2) (f1 c_0 ?v_26)) (= (f1 ?v_4 c_3) ?v_86) (= (f1 ?v_4 c_4) (f1 c_0 ?v_28)) (= (f1 ?v_5 c_0) ?v_87) (= (f1 ?v_5 c_1) (f1 c_0 ?v_31)) (= (f1 ?v_5 c_2) (f1 c_0 ?v_32)) (= (f1 ?v_5 c_3) (f1 c_0 ?v_33)) (= (f1 ?v_5 c_4) ?v_88) (= (f1 ?v_10 c_0) ?v_89) (= (f1 ?v_10 c_1) ?v_90) (= (f1 ?v_10 c_2) (f1 c_1 ?v_3)) (= (f1 ?v_10 c_3) (f1 c_1 ?v_4)) (= (f1 ?v_10 c_4) (f1 c_1 ?v_5)) (= (f1 ?v_12 c_0) (f1 c_1 ?v_10)) (= (f1 ?v_12 c_1) ?v_91) (= (f1 ?v_12 c_2) (f1 c_1 ?v_13)) (= (f1 ?v_12 c_3) (f1 c_1 ?v_14)) (= (f1 ?v_12 c_4) (f1 c_1 ?v_15)) (= (f1 ?v_13 c_0) (f1 c_1 ?v_17)) (= (f1 ?v_13 c_1) ?v_92) (= (f1 ?v_13 c_2) ?v_93) (= (f1 ?v_13 c_3) (f1 c_1 ?v_21)) (= (f1 ?v_13 c_4) (f1 c_1 ?v_22)) (= (f1 ?v_14 c_0) (f1 c_1 ?v_23)) (= (f1 ?v_14 c_1) ?v_94) (= (f1 ?v_14 c_2) (f1 c_1 ?v_26)) (= (f1 ?v_14 c_3) ?v_95) (= (f1 ?v_14 c_4) (f1 c_1 ?v_28)) (= (f1 ?v_15 c_0) (f1 c_1 ?v_29)) (= (f1 ?v_15 c_1) ?v_96) (= (f1 ?v_15 c_2) (f1 c_1 ?v_32)) (= (f1 ?v_15 c_3) (f1 c_1 ?v_33)) (= (f1 ?v_15 c_4) ?v_97) (= (f1 ?v_17 c_0) ?v_98) (= (f1 ?v_17 c_1) (f1 c_2 ?v_2)) (= (f1 ?v_17 c_2) ?v_99) (= (f1 ?v_17 c_3) (f1 c_2 ?v_4)) (= (f1 ?v_17 c_4) (f1 c_2 ?v_5)) (= (f1 ?v_19 c_0) (f1 c_2 ?v_10)) (= (f1 ?v_19 c_1) ?v_100) (= (f1 ?v_19 c_2) ?v_101) (= (f1 ?v_19 c_3) (f1 c_2 ?v_14)) (= (f1 ?v_19 c_4) (f1 c_2 ?v_15)) (= (f1 ?v_20 c_0) (f1 c_2 ?v_17)) (= (f1 ?v_20 c_1) (f1 c_2 ?v_19)) (= (f1 ?v_20 c_2) ?v_102) (= (f1 ?v_20 c_3) (f1 c_2 ?v_21)) (= (f1 ?v_20 c_4) (f1 c_2 ?v_22)) (= (f1 ?v_21 c_0) (f1 c_2 ?v_23)) (= (f1 ?v_21 c_1) (f1 c_2 ?v_25)) (= (f1 ?v_21 c_2) ?v_103) (= (f1 ?v_21 c_3) ?v_104) (= (f1 ?v_21 c_4) (f1 c_2 ?v_28)) (= (f1 ?v_22 c_0) (f1 c_2 ?v_29)) (= (f1 ?v_22 c_1) (f1 c_2 ?v_31)) (= (f1 ?v_22 c_2) ?v_105) (= (f1 ?v_22 c_3) (f1 c_2 ?v_33)) (= (f1 ?v_22 c_4) ?v_106) (= (f1 ?v_23 c_0) ?v_107) (= (f1 ?v_23 c_1) (f1 c_3 ?v_2)) (= (f1 ?v_23 c_2) (f1 c_3 ?v_3)) (= (f1 ?v_23 c_3) ?v_108) (= (f1 ?v_23 c_4) (f1 c_3 ?v_5)) (= (f1 ?v_25 c_0) (f1 c_3 ?v_10)) (= (f1 ?v_25 c_1) ?v_109) (= (f1 ?v_25 c_2) (f1 c_3 ?v_13)) (= (f1 ?v_25 c_3) ?v_110) (= (f1 ?v_25 c_4) (f1 c_3 ?v_15)) (= (f1 ?v_26 c_0) (f1 c_3 ?v_17)) (= (f1 ?v_26 c_1) (f1 c_3 ?v_19)) (= (f1 ?v_26 c_2) ?v_111) (= (f1 ?v_26 c_3) ?v_112) (= (f1 ?v_26 c_4) (f1 c_3 ?v_22)) (= (f1 ?v_27 c_0) (f1 c_3 ?v_23)) (= (f1 ?v_27 c_1) (f1 c_3 ?v_25)) (= (f1 ?v_27 c_2) (f1 c_3 ?v_26)) (= (f1 ?v_27 c_3) ?v_113) (= (f1 ?v_27 c_4) (f1 c_3 ?v_28)) (= (f1 ?v_28 c_0) (f1 c_3 ?v_29)) (= (f1 ?v_28 c_1) (f1 c_3 ?v_31)) (= (f1 ?v_28 c_2) (f1 c_3 ?v_32)) (= (f1 ?v_28 c_3) ?v_114) (= (f1 ?v_28 c_4) ?v_115) (= (f1 ?v_29 c_0) ?v_116) (= (f1 ?v_29 c_1) (f1 c_4 ?v_2)) (= (f1 ?v_29 c_2) (f1 c_4 ?v_3)) (= (f1 ?v_29 c_3) (f1 c_4 ?v_4)) (= (f1 ?v_29 c_4) ?v_117) (= (f1 ?v_31 c_0) (f1 c_4 ?v_10)) (= (f1 ?v_31 c_1) ?v_118) (= (f1 ?v_31 c_2) (f1 c_4 ?v_13)) (= (f1 ?v_31 c_3) (f1 c_4 ?v_14)) (= (f1 ?v_31 c_4) ?v_119) (= (f1 ?v_32 c_0) (f1 c_4 ?v_17)) (= (f1 ?v_32 c_1) (f1 c_4 ?v_19)) (= (f1 ?v_32 c_2) ?v_120) (= (f1 ?v_32 c_3) (f1 c_4 ?v_21)) (= (f1 ?v_32 c_4) ?v_121) (= (f1 ?v_33 c_0) (f1 c_4 ?v_23)) (= (f1 ?v_33 c_1) (f1 c_4 ?v_25)) (= (f1 ?v_33 c_2) (f1 c_4 ?v_26)) (= (f1 ?v_33 c_3) ?v_122) (= (f1 ?v_33 c_4) ?v_123) (= (f1 ?v_34 c_0) (f1 c_4 ?v_29)) (= (f1 ?v_34 c_1) (f1 c_4 ?v_31)) (= (f1 ?v_34 c_2) (f1 c_4 ?v_32)) (= (f1 ?v_34 c_3) (f1 c_4 ?v_33)) (= (f1 ?v_34 c_4) ?v_124) (or (= ?v_0 c_0) (= ?v_0 c_1) (= ?v_0 c_2) (= ?v_0 c_3) (= ?v_0 c_4)) (or (= ?v_2 c_0) (= ?v_2 c_1) (= ?v_2 c_2) (= ?v_2 c_3) (= ?v_2 c_4)) (or (= ?v_3 c_0) (= ?v_3 c_1) (= ?v_3 c_2) (= ?v_3 c_3) (= ?v_3 c_4)) (or (= ?v_4 c_0) (= ?v_4 c_1) (= ?v_4 c_2) (= ?v_4 c_3) (= ?v_4 c_4)) (or (= ?v_5 c_0) (= ?v_5 c_1) (= ?v_5 c_2) (= ?v_5 c_3) (= ?v_5 c_4)) (or (= ?v_10 c_0) (= ?v_10 c_1) (= ?v_10 c_2) (= ?v_10 c_3) (= ?v_10 c_4)) (or (= ?v_12 c_0) (= ?v_12 c_1) (= ?v_12 c_2) (= ?v_12 c_3) (= ?v_12 c_4)) (or (= ?v_13 c_0) (= ?v_13 c_1) (= ?v_13 c_2) (= ?v_13 c_3) (= ?v_13 c_4)) (or (= ?v_14 c_0) (= ?v_14 c_1) (= ?v_14 c_2) (= ?v_14 c_3) (= ?v_14 c_4)) (or (= ?v_15 c_0) (= ?v_15 c_1) (= ?v_15 c_2) (= ?v_15 c_3) (= ?v_15 c_4)) (or (= ?v_17 c_0) (= ?v_17 c_1) (= ?v_17 c_2) (= ?v_17 c_3) (= ?v_17 c_4)) (or (= ?v_19 c_0) (= ?v_19 c_1) (= ?v_19 c_2) (= ?v_19 c_3) (= ?v_19 c_4)) (or (= ?v_20 c_0) (= ?v_20 c_1) (= ?v_20 c_2) (= ?v_20 c_3) (= ?v_20 c_4)) (or (= ?v_21 c_0) (= ?v_21 c_1) (= ?v_21 c_2) (= ?v_21 c_3) (= ?v_21 c_4)) (or (= ?v_22 c_0) (= ?v_22 c_1) (= ?v_22 c_2) (= ?v_22 c_3) (= ?v_22 c_4)) (or (= ?v_23 c_0) (= ?v_23 c_1) (= ?v_23 c_2) (= ?v_23 c_3) (= ?v_23 c_4)) (or (= ?v_25 c_0) (= ?v_25 c_1) (= ?v_25 c_2) (= ?v_25 c_3) (= ?v_25 c_4)) (or (= ?v_26 c_0) (= ?v_26 c_1) (= ?v_26 c_2) (= ?v_26 c_3) (= ?v_26 c_4)) (or (= ?v_27 c_0) (= ?v_27 c_1) (= ?v_27 c_2) (= ?v_27 c_3) (= ?v_27 c_4)) (or (= ?v_28 c_0) (= ?v_28 c_1) (= ?v_28 c_2) (= ?v_28 c_3) (= ?v_28 c_4)) (or (= ?v_29 c_0) (= ?v_29 c_1) (= ?v_29 c_2) (= ?v_29 c_3) (= ?v_29 c_4)) (or (= ?v_31 c_0) (= ?v_31 c_1) (= ?v_31 c_2) (= ?v_31 c_3) (= ?v_31 c_4)) (or (= ?v_32 c_0) (= ?v_32 c_1) (= ?v_32 c_2) (= ?v_32 c_3) (= ?v_32 c_4)) (or (= ?v_33 c_0) (= ?v_33 c_1) (= ?v_33 c_2) (= ?v_33 c_3) (= ?v_33 c_4)) (or (= ?v_34 c_0) (= ?v_34 c_1) (= ?v_34 c_2) (= ?v_34 c_3) (= ?v_34 c_4)) (or (= c2 c_0) (= c2 c_1) (= c2 c_2) (= c2 c_3) (= c2 c_4)) (or (= c3 c_0) (= c3 c_1) (= c3 c_2) (= c3 c_3) (= c3 c_4)))))))))))))))))))
(check-sat)
(exit)
