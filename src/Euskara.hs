{-# LANGUAGE TemplateHaskell #-}

module Euskara where

import Derive
import Data.Maybe ( fromJust )
import Test.Feat ( Enumerable, Enumerate
                 , enumerate, consts, unary, funcurry 
                 , values )

generateReadings :: IO ()
generateReadings = 
  writeFile ("/tmp/eus.readings.new") $ unlines $
   concatMap (map show.snd) 
             (values :: [(Integer,[PartOfSpeech])])

--------------------------------------------------------------------------------

enumBounded :: (Enum a, Bounded a) => Enumerate a
enumBounded = consts $ map pure [minBound..maxBound]

--Tags present in corpus but not in the documents

data ZERO = ZERO deriving (Show,Eq,Enum,Bounded)
instance Enumerable ZERO where enumerate = enumBounded

data NOTDEK = NOTDEK deriving (Show,Eq,Enum,Bounded)
instance Enumerable NOTDEK where enumerate = enumBounded

data AORG = AORG deriving (Show,Eq,Enum,Bounded)
instance Enumerable AORG where enumerate = enumBounded

data AL = AL deriving (Show,Eq,Enum,Bounded)
instance Enumerable AL where enumerate = enumBounded

data ZALE = ZALE  deriving (Show,Eq,Enum,Bounded)
instance Enumerable ZALE where enumerate = enumBounded
--------------------------------------------------------------------------------

--------------------------------------------
-- 1    Tokenizazioan ezarritako etiketak --
--------------------------------------------

-- 1.1    Puntuazio-markak

data Puntuazio = PUNT_PUNT | PUNT_KOMA | PUNT_PUNT_KOMA 
               | PUNT_BI_PUNT | PUNT_ESKL | PUNT_GALD | PUNT_HIRU
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Puntuazio where
  enumerate = enumBounded

-- 1.2    Ortografia
data Ortografia = HAS_MAI | DEN_MAI | DEN_MAI_DEK
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Ortografia where
  enumerate = enumBounded

---------------------------------
-- 2    EDBLko etiketa-sistema --
---------------------------------

--------------------------------------------
-- 2.1    Kategoria lexikalak
-- Kategoria (KAT) eta azpikategoria (AZP) nagusiak


type Case = Either CoreCase (OtherCase, Maybe ZERO)

-- This is like KategoriaLex but only the constructor names.
-- Just a test to do some simple thing with SAT.
--TODO: use MorphCat
data PartOfSpeech = IZE AzpIZE -- Noun
                        Case
                        DefNum
                        (Maybe AORG)

                  | ADI_v_ AzpADI 
                           ADOIN
                           (Either Aspect NOTDEK)

                  | ADI_n_ AzpADI 
                           PART_ADIZE
                           (Either (Case,DefNum) NOTDEK)
                           (Maybe Erlazioak)

                  | ADJ AzpADB
                        Case
                        DefNum           
                  | ADB AzpADB
                  | DET AzpDET Case
                  | IOR AzpIOR Case -- Pronoun
                  | LOT AzpLOT -- Connective
                  | PRT  -- Partikula
                  | ITJ -- Interjekzioa
                  | BST -- Bestelakoa
    -- Kategoria lagungarriak
                  | ADL AgrADL
                      --  (Maybe Erlazioak)
                        TenseMood
                --  | ADT AgrADL
                        --TenseMood


 deriving (Eq)

instance Enumerable PartOfSpeech where
--  enumerate = enumBounded
  enumerate = consts ( unary (funcurry (funcurry
                             (funcurry IZE))):
                       unary (funcurry (funcurry 
                             ( ADI_v_))):
                       unary (funcurry (funcurry 
                             (funcurry ADI_n_))):
                       unary (funcurry (funcurry ADJ)):
                       unary ADB:
                       unary (funcurry DET):
                       unary (funcurry IOR):
                       unary LOT:
                       unary (funcurry ( ADL)):
                   --    unary (funcurry ADT):
                       map pure [PRT,ITJ,BST] )



data KategoriaLex = KategoriaLex
{- First version of lexical categories; too fine-grained, millions of values
                    -- Noun
data KategoriaLex = IZE AzpIZE  -- ARR | IZB | LIB | ZKI 
--                        (Maybe Meta)

                    -- Verb
                  | ADI AzpADI   -- SIN | ADK | FAK
                        (Maybe Degree) -- SUP
                        (Maybe VerbType)  -- PART | ADOIN | ADIZE
                        (Maybe Aspect)  -- BURU | EZBU | GERO | PNT 
                        (Maybe Erlazioak) -- BALD | DENB | ..
                        (Maybe Case)   -- ABS | INS | .. 
                        (Maybe DefNum)  -- MG | MUGM NUMS | .. 
                        (Maybe ZERO)
                        (Maybe NOTDEK) -- Shortest I've found: ADI ADK NOTDEK
                   --  e.g.  ADI ADK ADOIN BURU MOD

                  | ADJ AzpADB -- Adjective. OBS. same subcat as adv
                        (Maybe AdjPosizioak) -- IZAUR+ | IZAUR-
                        (Maybe Animacy) -- BIZ+ | BIZ-
                        (Maybe Degree) 
                        (Maybe (Case, DefNum)) -- GEN PH MUGM | INE MG | ...
                        (Maybe AORG)
                        (Maybe ZERO) -- Shortest I've found: ADJ ARR ZERO
                        --TODO the rest

                  | ADB AzpADB -- Adverb.
                        Def 
                    -- only in ADB can Def (Mugatasuna) appear without Num! 
                    -- e.g. "hemendik_aurrera" ADB ARR BIZ- MUGM AORG

                  | DET AzpDET
                  | IOR AzpIOR -- Pronoun
                  | LOT AzpLOT -- Connective
                  | PRT Mod -- Partikula
                  | ITJ -- Interjekzioa
                  | BST -- Bestelakoa

    -- Kategoria lagungarriak
                  | ADL (Maybe (Case,DefNum))  -- OBS. If the case is GEL, then no DefNum??
                        TenseMood 
                        AgrADL   --TODO is there more?                        
                   -- Aditz laguntzailea: du

                  | ADT Aspect               -- PNT 
                        (Maybe Erlazioak)    -- BALD
                        (Maybe (Case,DefNum)) -- ABS MG  -- OBS. If the case is GEL, then no DefNum
                        TenseMood             -- B1 
                        AgrADL               -- NOR NR_HURA                        
                        (Maybe Mod)  --TODO is there more?
                   -- Aditz trinkoa: dator
 deriving (Eq)


instance Enumerable KategoriaLex where
  enumerate = consts (
                       unary (funcurry IZE):
                       unary (funcurry (funcurry (funcurry 
                             (funcurry (funcurry (funcurry
                             (funcurry (funcurry ADI)))))))):
                       unary (funcurry (funcurry (funcurry 
                             (funcurry (funcurry 
                             (funcurry ADJ)))))):
                       unary (funcurry ADB):
                       unary DET:                       
                       unary IOR:
                       unary LOT:
                       unary (funcurry (funcurry ADL)):
                       unary (funcurry (funcurry (funcurry
                             (funcurry (funcurry ADT))))):
                       map pure [ ITJ, BST ] )
-}

-- (MTKAT) -- I imagine only for nouns?
data Meta = SIG | SNB | LAB
 deriving (Show,Eq,Enum,Bounded)                                  
instance Enumerable Meta where
  enumerate = enumBounded

data AgrADL = NOR Nor | NOR_NORI Nor Nori | NOR_NORK Nor Nork
            | NOR_NORI_NORK Nor Nori Nork deriving (Eq)

instance Enumerable AgrADL where
  enumerate = consts [ unary NOR
                     , unary (funcurry NOR_NORI)
                     , unary (funcurry NOR_NORK)
                     , unary (funcurry (funcurry NOR_NORI_NORK)) ]

data AzpIZE = ARR -- Izen arrunta 
            | IZB -- Pertsona-izen berezia
            | LIB -- Leku-izen berezia
            | ZKI -- Izen zenbakia
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable AzpIZE where
  enumerate = enumBounded

data AzpADB = ARR_adb -- Adberbio arrunta
            | GAL --Adberbio galdetzailea
 deriving (Eq,Enum,Bounded)
instance Enumerable AzpADB where
  enumerate = enumBounded

-- The tag ARR is in both Ize and Adb, hence different constructor and custom Show instance
instance Show AzpADB where 
    show ARR_adb = "ARR"
    show GAL = "GAL"


data AzpADI = SIN -- Aditz sinplea
            | ADK -- Aditz konposatua
            | FAK --Aditz faktitiboa
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable AzpADI where
  enumerate = enumBounded

data AzpDET = ERKARR -- Determinatzailea: erakusle arrunta
            | ERKIND -- Determinatzailea: erakusle indartua
--            | NOLARR -- Det.: nolakotzaile arrunta
            | NOLGAL --Det.: nolakotzaile galdetzailea
            | DZH -- Det.: zenbatzaile zehaztua
--            | BAN -- Det.: zenbatzaile banatzailea
            | ORD --Det.: zenbatzaile ordinala
            | DZG --Det.: zenbatzaile zehaztugabea
--            | ORO --Det.: zenbatzaile orokorra
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable AzpDET where
  enumerate = enumBounded

data AzpIOR = IZGMGB -- Izenordain zehaztugabe mugagabea
--            | PERIND -- Pertsona-izenordain indartua
--            | PERARR -- Pertsona-izenordain arrunta
--            | IZGGAL -- Izenordain zehaztugabe galdetzailea
--            | BIH -- Izenordain bihurkaria
--            | ELK --Izenordain elkarkaria
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable AzpIOR where
  enumerate = enumBounded

data AzpLOT = LOK -- Loturazko lokailuak
            | JNT -- Loturazko juntagailua
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable AzpLOT where
  enumerate = enumBounded
--------------------------------------------
-- 2.2 Kategoria morfologikoak (KAT)
data MorphCat = AMM -- Aditz-mota morfema
              | ASP -- Aspektu-morfema
              | ATZ -- Atzizki lexikala
--              | AUR -- Aurrizki lexikala
              | DEK -- Deklinabide-morfema
              | ELI -- Elipsia
              | ERL -- Erlazio-atzizkia
              | GRA -- Graduatzailea 
              | MAR -- Marratxoa
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable MorphCat where
  enumerate = enumBounded

--------------------------------------------
-- 2.3 Morfologia-ezaugarriak

-- Kasumarkak

data CoreCase = ABS | ERG | DAT deriving (Show,Eq,Enum,Bounded)
instance Enumerable CoreCase where
  enumerate = enumBounded

data OtherCase = ABL | ABU | ABZ | ALA
               | BNK | DES | DESK | GEL | GEN
               | INE | INS | MOT | PAR | PRO | SOZ
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable OtherCase where
  enumerate = enumBounded

-- Usually Mugatasuna is accompanied by number 
data DefNum = Indef | Def Number deriving (Eq)
instance Show DefNum where
  show Indef = "MG"
  show (Def n) = show n ++ " MUGM" 

instance Enumerable DefNum where
  enumerate = consts [pure Indef, unary Def]


-- Mugatasuna may appear without number in Adverbs.
-- Also there is a MG tag as a part of DetDefNum; 
-- the constructor is called NMG to distinguish from this.
-- Mugatasuna (MUG)
data Def = MUGM -- Definite
         | MG   -- Indefinite
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Def where
  enumerate = enumBounded

data Number = NUMS | NUMP | PH deriving (Show,Eq,Enum,Bounded)
instance Enumerable Number where
  enumerate = enumBounded

-- Gradu maila (MAI)
data Degree = KONP | SUP | GEHI | IND deriving (Show,Eq,Enum,Bounded)
instance Enumerable Degree where
  enumerate = enumBounded

-- Aditz mota (ADM)
-- Splitting this, because PART and ADIZE may take case, def, ...
-- ADOIN (base of verb) may not (????)
--data VerbType = PART | ADOIN | ADIZE deriving (Show,Eq,Enum,Bounded)
--instance Enumerable VerbType where
--  enumerate = enumBounded

data ADOIN = ADOIN deriving (Show,Eq,Enum,Bounded)
instance Enumerable ADOIN where
  enumerate = enumBounded

data PART_ADIZE = PART | ADIZE deriving (Show,Eq,Enum,Bounded)
instance Enumerable PART_ADIZE where
  enumerate = enumBounded


-- Aspektua (ASP)
data Aspect = BURU | EZBU | GERO | PNT deriving (Show,Eq,Enum,Bounded)
instance Enumerable Aspect where
  enumerate = enumBounded

-- Modu-denbora (MDN)
data TenseMood =
     A1 -- Indikatibozko orainaldia: naiz
--   | A2 -- Indikatibozko geroaldi arkaikoa: naizateke  -- Not used in the latest grammar
   | A3 -- Subjuntibozko orainaldia: nadi(n)
   | A4 -- Subjuntibozko baldintza: (ba)nadi
   | A5 -- Ahalezko orainaldia: naiteke
   | B1 -- Indikatibozko lehenaldia: nintzen
   | B2 -- Indik. baldintza (ondorioa, orain-gero): nintzateke
   | B3 -- Indik. baldintza (ondorioa, lehen): nintzatekeen
   | B4 -- Indik. baldintza (aurrekoa): (ba)nintz
   | B5A -- Subjuntibozko lehenaldia: nendin, zedin
   | B5B -- Subjuntibozko alegiazkoa: ledi(n)
--   | B6 -- Subjuntibozko baldintza (lehenaldia): banendi  -- Not used in the latest grammar
   | B7 -- Ahalezko lehenaldia: ninteke
   | B8 -- Ahalezko lehenaldi urruna: nintekeen
   | MDNC -- Aginterazko orainaldia: hadi -- or just C?
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable TenseMood where
  enumerate = enumBounded


-- Modality
data Mod = ZIU -- ziurtasunezkoa  Possibility
         | EGI -- egiatasunezkoa  Certainty
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Mod where
  enumerate = enumBounded

-- Verb agreement: 
-- has 1-3 of the following, either Nor, Nor-Nori, Nor-Nork or Nor-Nori-Nork
data Nor =
         NR_NI 
         | NR_HI 
         | NR_HURA 
         -- | NR_GU | NR_ZU | NR_ZUEK 
         | NR_HAIEK deriving (Eq,Enum,Bounded)
instance Enumerable Nor where
  enumerate = enumBounded


data Nori = NI_NIRI 
          | NI_HIRI
          -- | NI_HIRI_dash_TO 
          -- | NI_HIRI_dash_NO 
          | NI_HARI
          | NI_GURI | NI_ZURI | NI_ZUEI | NI_HAIEI deriving (Eq,Enum,Bounded)
instance Enumerable Nori where
  enumerate = enumBounded


data Nork = NK_NIK 
          | NK_HIK -- | NK_HIK_dash_TO 
          | NK_HIK_dash_NO | NK_HARK
          | NK_GUK | NK_ZUK 
          | NK_ZUEK_dash_K | NK_HAIEK_dash_K deriving (Eq,Enum,Bounded)
instance Enumerable Nork where
  enumerate = enumBounded


--Hitanozko forma alokutiboak (HIT)
data Hitano = TO -- Gizonezkoen hitanoa
            | NO -- Andrazkoen hitanoa
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Hitano where
  enumerate = enumBounded

-- Menderagailuen erlazioak (ERL) -- subordinate morphemes
data Erlazioak = BALD -- Baldintzakoa
               | DENB -- Denborazkoa
               | ERLT -- Erlatibozkoa
               | ESPL -- Esplikatiboa
               | HELB -- Helburuzkoa
               | KAUS -- Kausazkoa 
               | KONPL -- Konpletiboa
--               | KONT -- Kontzesiboa
               | MOD -- Moduzkoa
               | MOD_slash_DENB -- Moduzkoa/Denborazkoa (orig. name MOD/DENB)
               | MOS -- Mendeko osagaia
               | ZHG -- Zehar-galdera
-- Lokailuen eta juntagailuen erlazioak (ERL)
               | AURK -- Aurkaritzakoa
--               | EMEN -- Emendiozkoa 
--               | HAUT -- Hautazkoa
--               | ONDO --Ondoriozkoa
 deriving (Show,Eq,Enum,Bounded)
instance Enumerable Erlazioak where
  enumerate = enumBounded

--------------------------------------------
-- 2.4 Ezaugarri lexiko-semantikoak
-- Determinatzaileen numero-mugatasuna (NMG)
data DetDefNum = NMG -- Mugabea
               | NMGS -- Singularra
               | NMGP -- Plurala
 deriving (Eq,Enum,Bounded)
instance Enumerable DetDefNum where
  enumerate = enumBounded

instance Show DetDefNum where
    show NMG = "MG"
    show NMGS = "NMGS"
    show NMGP = "NMGP"

-- Izenordainen numeroa (NUM)
-- TODO do we need this?
-- data IzenNumeroa = 

--Izenordainen pertsona (PER)
data Person = NI | HI | HURA | GU | ZU | ZUEK | HAIEK deriving (Show,Eq,Enum,Bounded)
instance Enumerable Person where
  enumerate = enumBounded

-- Izenen biziduntasuna (BIZ) - Oraindik EDBLn sistematikoki landu gabe.
data Animacy = BIZ_plus_ -- Biziduna 
             | BIZ_minus_ -- Bizigabea
 deriving (Eq,Enum,Bounded)
instance Enumerable Animacy where
  enumerate = enumBounded


-- Izenen zenbagarritasuna (ZENB) - Oraindik EDBLn landu gabe.
data Zenbagarritasuna = ZENB_plus_ | ZENB_minus_ deriving (Eq,Enum,Bounded)
instance Enumerable Zenbagarritasuna where
  enumerate = enumBounded

-- Izenen neurgarritasuna (NEUR) - - Oraindik EDBLn landu gabe.
data Neurgarritasuna = NEUR_plus_ | NEUR_minus_ deriving (Eq,Enum,Bounded)
instance Enumerable Neurgarritasuna where
  enumerate = enumBounded

--Pluralia tantum izenak (PLU) - Oraindik EDBLn sistematikoki landu gabe.
data PluraliaTantum = PLU_plus_ | PLU_minus_ deriving (Eq,Enum,Bounded)
instance Enumerable PluraliaTantum where
  enumerate = enumBounded


--Aditz nagusiaren laguntzaile-mota (LAGM) - Oraindik EDBLn sistematikoki landu gabe.
-- Main verb agreement type -- OBS. There's a separate tag for synthetic verbs!
data AuxType = DA --NOR
             | DU --NOR-NORK
             | DA_dash_DU --NOR eta NOR-NORK
             | ZAIO --NOR-NORI
             | DIO --NOR-NORI-NORK
 deriving (Eq,Enum,Bounded)


instance Enumerable AuxType where
  enumerate = enumBounded


--Errore kodeak
data Errorekodeak = A_FAK --Aditz Faktitiboa
                  | DE_DI --Dialektalak(deklinabideari dagozkionak, adib. zugaitik)
                  | DE_ER --Erakusleen deklinabidean sortzen direnak
                  | DE_LE --Leku-izenen deklinabidean sortzen direnak
                  | DIAL --Aho-hizkerak nahiz dialektalismoak eragindakoak
                  | ERAT -- Eratorpenean ematen direnak
                  | FO_OK --Forma okerra
                  | LD_FO --Arazo fonetikoek eragindako lema desberdinak
                  | LD_MA --Maileguen egokitzapenean sortzen diren lema desberdinak
                  | NEOL --Neologismoak
                  | KONPOS --Konposizioan ematen direnak
                  | ATZKI --Atzizkia
                  | AZTERTU_GABEA --Aztertu gabea (Lexikoaren Behatokian landutako sarrerak)
 deriving (Show,Eq,Enum,Bounded)
--------------------------------------------
-- 2.5 Ezaugarri sintaktikoak
-- Adjektiboen posizioa (IZAUR)
data AdjPosizioak = IZAUR_plus_ | IZAUR_minus_ deriving (Eq,Enum,Bounded)
instance Enumerable AdjPosizioak where
  enumerate = enumBounded


--Determinatzailearen posizioa sintagman (POS)  - Oraindik EDBLn landu gabe.
data DetPos = ATZE  -- Atzetik derrigorrez
            | AURRE -- Aurretik derrigorrez
            | NN    -- Nonahi, aurretik nahiz atzetik
 deriving (Show,Eq,Enum,Bounded) 
instance Enumerable DetPos where
  enumerate = enumBounded

-- Loturazkoak - Klausula muga (KLM) ) - Oraindik EDBLn landu gabe.
data Loturazkoak = HAS --Klausula-hasiera markatzen duen loturazkoa: ezen
                 | AM -- Klausula-amaiera markatzen duen loturazkoa: arren
                 | HA -- Klausula-hasiera zein -amaiera marka dezakeen loturazkoa: ?
 deriving (Show,Eq,Enum,Bounded) 
instance Enumerable Loturazkoak where
  enumerate = enumBounded


------------------------------
-- 3   Funtzio sintaktikoak --
------------------------------

data Sintaktikoak
 = Sin__esk_ADILOK | Sin_ADILOK_ezk_ | Sin_ADLG | Sin_ATRIB | Sin_BST
 | Sin__esk_GRAD | Sin_Grad_ezk_
 | Sin_HAOS 
 | Sin__esk_IA | Sin_IA_ezk_
 | Sin__esk_ID | Sin_ID_ezk_
 | Sin_ITJ 
 | Sin__esk_IZLG | Sin_IZLG_ezk_
 | Sin_KM_ezk_
 | Sin_LAB | Sin_LOK 
 | Sin_MP | Sin_MD_ADLG | Sin_MD_OBJ
 | Sin_OBJ | Sin_PJ | Sin_PRED | Sin_PRT | Sin_SIGLA
 | Sin_SINBOLOA | Sin_SUBJ | Sin_ZOBJ | Sin_IS | Sin_FSG

-- Jadlag
 | Sin__plus_JADLAG | Sin__minus_JADLAG
 | Sin__plus_JADLAG_MP | Sin__minus_JADLAG_MP
 | Sin_JADLAG_MP_ADLG | Sin_JADLAG_IZLG_ezk_
 | Sin_JADLAG_MP_IZLG_ezk_ | Sin__esk_JADLAG_MP_IZLG
 | Sin_JADLAG_MP_OBJ | Sin_JADLAG_MP_SUBJ | Sin_JADLAG_MP_PRED

-- Jadnag
 | Sin__plus_JADNAG | Sin__minus_JADNAG
 | Sin__plus_JADNAG_MP | Sin__minus_JADNAG_MP
 | Sin__plus_JADNAG_MP_ADLG | Sin__minus_JADNAG_MP_ADLG
 | Sin_JADNAG_IZLG
 | Sin__plus_JADNAG_MP_IZLG_ezk_ | Sin__minus_JADNAG_MP_IZLG_ezk_
 | Sin__esk__plus_JADNAG_MP_IZLG | Sin__esk__minus_JADNAG_MP_IZLG
 | Sin__plus_JADNAG_MP_OBJ | Sin__minus_JADNAG_MP_OBJ
 | Sin__plus_JADNAG_MP_SUBJ | Sin__minus_JADNAG_MP_SUBJ
 | Sin__plus_JADNAG_MP_PRED | Sin__minus_JADNAG_MP_PRED
 | Sin_JADNAG_MP_ZOBJ | Sin_JADNAG_MP_KM
 deriving (Eq,Enum,Bounded)

instance Enumerable Sintaktikoak where
  enumerate = enumBounded


{- Only these are in the morph. grammar
 @+JADLAG 
 @+JADLAG_IZLG> 
 @+JADLAG_MP 
 @+JADLAG_MP_ADLG 
 @+JADLAG_MP_ADLG> 
 @+JADNAG 
 @+JADNAG_IZLG> 
 @+JADNAG_MP 
 @+JADNAG_MP_ADLG 
 @+JADNAG_MP_ADLG> 
 @-JADNAG 
 @-JADNAG> 
 @-JADNAG_MP 
 @-JADNAG_MP_IZLG> 
 @-JADNAG_MP_KM>
 @<GRAD 
 @<IA 
 @IA> 
 @<ID 
 @ID> 
 @<IZ 
 @<IZLG 
 @ADLG 
 @ATRIB 
 @IZLG> 
 @KM> 
 @LOK 
 @OBJ 
 @PJ 
 @PRED 
 @SUBJ 
 @SUBJ> 
-}


$( deriveShow ''AgrADL )
$( deriveShow ''Nor )
$( deriveShow ''Nori )
$( deriveShow ''Nork )

$( deriveShow ''AdjPosizioak )
$( deriveShow ''Animacy )
$( deriveShow ''AuxType )

$(deriveShow ''KategoriaLex)
$( deriveShow ''PartOfSpeech )

$(deriveShow ''Zenbagarritasuna)

$(deriveShow ''Neurgarritasuna)

$(deriveShow ''PluraliaTantum)

$(deriveShow ''Sintaktikoak)

