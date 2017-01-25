module Euskara where

import Data.Maybe ( fromJust )


{-
Tags present in corpus but not in the documents
AORG
ZERO
NOTDEK
-}


data ZERO = ZERO deriving (Show,Eq,Enum,Bounded)
data NOTDEK = NOTDEK deriving (Show,Eq,Enum,Bounded)
data AORG = AORG deriving (Show,Eq,Enum,Bounded)
data AL = AL deriving (Show,Eq,Enum,Bounded)
data ZALE = ZALE  deriving (Show,Eq,Enum,Bounded)
--------------------------------------------------------------------------------

--------------------------------------------
-- 1    Tokenizazioan ezarritako etiketak --
--------------------------------------------

-- 1.1    Puntuazio-markak

data Puntuazio = PUNT_PUNT | PUNT_KOMA | PUNT_PUNT_KOMA 
               | PUNT_BI_PUNT | PUNT_ESKL | PUNT_GALD | PUNT_HIRU
 deriving (Show,Eq,Enum,Bounded)

-- 1.2    Ortografia
data Ortografia = HAS_MAI | DEN_MAI | DEN_MAI_DEK
 deriving (Show,Eq,Enum,Bounded)


---------------------------------
-- 2    EDBLko etiketa-sistema --
---------------------------------

--------------------------------------------
-- 2.1    Kategoria lexikalak
-- Kategoria (KAT) eta azpikategoria (AZP) nagusiak
                    -- Noun
data KategoriaLex = IZE AzpIZE  -- ARR | IZB | LIB | ZKI 
                        (Maybe Meta)

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
 deriving (Show,Eq)

-- (MTKAT) -- I imagine only for nouns?
data Meta = SIG -- Sigla -- "<CFTC>"<DEN_MAI>" "CFTC" IZE IZB *SIG* ZERO DEN_MAI
                          -- "<EHNE>"<DEN_MAI>" "EHNE" IZE IZB *SIG* ABS NUMS MUGM DEN_MAI 
                          -- "<ELAren>"<DEN_MAI_DEK>" "ELA" IZE IZB BIZ- ZENB- NEUR- PLU- *SIG* GEN NUMS MUGM ZERO DEN_MAI_DEK 
          | SNB --Sinboloa: km, cm, g --"<M.>"<ERROM>" "m" IZE ARR BIZ- SNB ABS NUMS MUGM ERROM 
          | LAB --Laburdura: etab. -- "<al>" "al." IZE ARR LAB ZERO
                                    -- "<min>" "min." IZE ARR BIZ+ LAB ZERO 
 deriving (Show,Eq,Enum,Bounded)                                  

data AgrADL = NOR Nor | NORI Nor Nori | NOR_NORK Nor Nork
            | NOR_NORI Nor Nori | NOR_NORI_NORK Nor Nori Nork deriving (Show,Eq)

instance Enum AgrADL where

-- Azpikategoriak

data AzpIZE = ARR -- Izen arrunta 
            | IZB -- Pertsona-izen berezia
            | LIB -- Leku-izen berezia
            | ZKI -- Izen zenbakia
 deriving (Show,Eq,Enum,Bounded)


data AzpADB = ARR_adb -- Adberbio arrunta
            | GAL --Adberbio galdetzailea
 deriving (Eq,Enum,Bounded)

-- The tag ARR is in both Ize and Adb, hence different constructor and custom Show instance
instance Show AzpADB where 
    show ARR_adb = "ARR"
    show GAL = "GAL"


data AzpADI = SIN -- Aditz sinplea
            | ADK -- Aditz konposatua
            | FAK --Aditz faktitiboa
 deriving (Show,Eq,Enum,Bounded)

data AzpDET = ERKARR -- Determinatzailea: erakusle arrunta
            | ERKIND -- Determinatzailea: erakusle indartua
            | NOLARR -- Det.: nolakotzaile arrunta
            | NOLGAL --Det.: nolakotzaile galdetzailea
            | DZH -- Det.: zenbatzaile zehaztua
            | BAN -- Det.: zenbatzaile banatzailea
            | ORD --Det.: zenbatzaile ordinala
            | DZG --Det.: zenbatzaile zehaztugabea
            | ORO --Det.: zenbatzaile orokorra
 deriving (Show,Eq,Enum,Bounded)

data AzpIOR = PERARR -- Pertsona-izenordain arrunta
            | PERIND -- Pertsona-izenordain indartua
            | IZGMGB -- Izenordain zehaztugabe mugagabea
            | IZGGAL -- Izenordain zehaztugabe galdetzailea
            | BIH -- Izenordain bihurkaria
            | ELK --Izenordain elkarkaria
 deriving (Show,Eq,Enum,Bounded)

data AzpLOT = LOK -- Loturazko lokailuak
            | JNT -- Loturazko juntagailua
 deriving (Show,Eq,Enum,Bounded)

--------------------------------------------
-- 2.2 Kategoria morfologikoak (KAT)
data KategoriaMor = AMM -- Aditz-mota morfema
                  | ASP -- Aspektu-morfema
                  | ATZ -- Atzizki lexikala
                  | AUR -- Aurrizki lexikala
                  | DEK -- Deklinabide-morfema
                  | ELI -- Elipsia
                  | ERL -- Erlazio-atzizkia
                  | GRA -- Graduatzailea 
                  | MAR -- Marratxoa
 deriving (Show,Eq,Enum,Bounded)


--------------------------------------------
-- 2.3 Morfologia-ezaugarriak

-- Kasumarkak
data Case = ABL | ABS | ABU | ABZ | ALA | BNK 
          | DAT | DES | DESK | ERG | GEL | GEN
          | INE | INS | MOT | PAR | PRO | SOZ
 deriving (Show,Eq,Enum,Bounded)

-- Usually Mugatasuna is accompanied by number 
data DefNum = Indef | Def Number deriving (Eq) --,Enum,Bounded)
instance Show DefNum where
  show Indef = "MG"
  show (Def n) = "MUGM " ++ show n

-- Mugatasuna may appear without number in Adverbs.
-- Also there is a MG tag as a part of DetDefNum; 
-- the constructor is called NMG to distinguish from this.
-- Mugatasuna (MUG)
data Def = MUGM -- Definite
         | MG   -- Indefinite
 deriving (Show,Eq,Enum,Bounded)

data Number = NUMS | NUMP | PH deriving (Show,Eq,Enum,Bounded)

-- Gradu maila (MAI)
data Degree = KONP | SUP | GEHI | IND deriving (Show,Eq,Enum,Bounded)

-- Aditz mota (ADM)
data VerbType = PART | ADOIN | ADIZE deriving (Show,Eq,Enum,Bounded)

-- Aspektua (ASP)
data Aspect = BURU | EZBU | GERO | PNT deriving (Show,Eq,Enum,Bounded)

-- Modu-denbora (MDN)
data TenseMood =
     A1 -- Indikatibozko orainaldia: naiz
   | A2 -- Indikatibozko geroaldi arkaikoa: naizateke
   | A3 -- Subjuntibozko orainaldia: nadi(n)
   | A4 -- Subjuntibozko baldintza: (ba)nadi
   | A5 -- Ahalezko orainaldia: naiteke
   | B1 -- Indikatibozko lehenaldia: nintzen
   | B2 -- Indik. baldintza (ondorioa, orain-gero): nintzateke
   | B3 -- Indik. baldintza (ondorioa, lehen): nintzatekeen
   | B4 -- Indik. baldintza (aurrekoa): (ba)nintz
   | B5A -- Subjuntibozko lehenaldia: nendin, zedin
   | B5B -- Subjuntibozko alegiazkoa: ledi(n)
   | B6 -- Subjuntibozko baldintza (lehenaldia): banendi
   | B7 -- Ahalezko lehenaldia: ninteke
   | B8 -- Ahalezko lehenaldi urruna: nintekeen
   | MDNC -- Aginterazko orainaldia: hadi -- or just C?
 deriving (Show,Eq,Enum,Bounded)

-- Modality
data Mod = ZIU -- ziurtasunezkoa  Possibility
         | EGI -- egiatasunezkoa  Certainty
 deriving (Show,Eq,Enum,Bounded)

-- Verb agreement: 
-- has 1-3 of the following, either Nor, Nor-Nori, Nor-Nork or Nor-Nori-Nork
data Nor = NR_NI | NR_HI | NR_HURA | NR_GU 
         | NR_ZU | NR_ZUEK | NR_HAIEK deriving (Show,Eq,Enum,Bounded)

data Nori = NI_NIRI | NI_HIRI Hitano | NI_HARI | NI_GURI 
          | NI_ZURI | NI_ZUEI | NI_HAIEI deriving (Eq)

instance Show Nori where
    show = snd . fromJust . flip lookup noriTable

instance Bounded Nori where
  minBound = NI_NIRI
  maxBound = NI_HAIEI

instance Enum Nori where
    fromEnum = fst . fromJust . flip lookup noriTable
    toEnum = fromJust . flip lookup (map (\(x, (y,z)) -> (y,x)) noriTable)

noriTable = [ (NI_NIRI,    (0, "NI_NIRI"))
            , (NI_HIRI TO, (1, "NI_HIRI-TO"))
            , (NI_HIRI NO, (2, "NI_HIRI-NO"))
            , (NI_HARI,    (3, "NI_HARI"))
            , (NI_GURI,    (4, "NI_GURI"))
            , (NI_ZURI,    (5, "NI_ZURI"))
            , (NI_ZUEI,    (6, "NI_ZUEI-RI"))
            , (NI_HAIEI,   (7, "NI_HAIEI-RI")) ]


data Nork = NK_NIK | NK_HIK Hitano | NK_HARK | NK_GUK 
          | NK_ZUK | NK_ZUEK_K | NK_HAIEK_K deriving (Eq)

instance Show Nork where
    show = snd . fromJust . flip lookup norkTable

instance Bounded Nork where
  minBound = NK_NIK
  maxBound = NK_HAIEK_K

instance Enum Nork where
    fromEnum = fst . fromJust . flip lookup norkTable
    toEnum = fromJust . flip lookup (map (\(x, (y,z)) -> (y,x)) norkTable)

norkTable = [ (NK_NIK,    (0, "NK_NIK"))
            , (NK_HIK TO, (1, "NK_HIK-TO"))
            , (NK_HIK NO, (2, "NK_HIK-NO"))
            , (NK_HARK,    (3, "NK_HARK"))
            , (NK_GUK,    (4, "NK_GUK"))
            , (NK_ZUK,    (5, "NK_ZUK"))
            , (NK_ZUEK_K,    (6, "NK_ZUEK-K"))
            , (NK_HAIEK_K,   (7, "NI_HAIEK-K")) ]


--Hitanozko forma alokutiboak (HIT)
data Hitano = TO -- Gizonezkoen hitanoa
            | NO -- Andrazkoen hitanoa
 deriving (Show,Eq,Enum,Bounded)

-- Menderagailuen erlazioak (ERL) -- subordinate morphemes
data Erlazioak = BALD -- Baldintzakoa
               | DENB -- Denborazkoa
               | ERLT -- Erlatibozkoa
               | ESPL -- Esplikatiboa
               | HELB -- Helburuzkoa
               | KAUS -- Kausazkoa 
               | KONPL -- Konpletiboa
               | KONT -- Kontzesiboa
               | MOD -- Moduzkoa
               | MOD_DENB -- Moduzkoa/Denborazkoa (orig. name MOD/DENB)
               | MOS -- Mendeko osagaia
               | ZHG -- Zehar-galdera
-- Lokailuen eta juntagailuen erlazioak (ERL)
               | AURK -- Aurkaritzakoa
               | EMEN -- Emendiozkoa 
               | HAUT -- Hautazkoa
               | ONDO --Ondoriozkoa
 deriving (Show,Eq,Enum,Bounded)

--------------------------------------------
-- 2.4 Ezaugarri lexiko-semantikoak
-- Determinatzaileen numero-mugatasuna (NMG)
data DetDefNum = NMG -- Mugabea
               | NMGS -- Singularra
               | NMGP -- Plurala
 deriving (Eq,Enum,Bounded)

instance Show DetDefNum where
    show NMG = "MG"
    show NMGS = "NMGS"
    show NMGP = "NMGP"

-- Izenordainen numeroa (NUM)
-- TODO do we need this?
-- data IzenNumeroa = 

--Izenordainen pertsona (PER)
data Person = NI | HI | HURA | GU | ZU | ZUEK | HAIEK deriving (Show,Eq,Enum,Bounded)

-- Izenen biziduntasuna (BIZ) - Oraindik EDBLn sistematikoki landu gabe.
data Animacy = BIZ_plus -- Biziduna 
             | BIZ_minus -- Bizigabea
 deriving (Eq,Enum,Bounded)

instance Show Animacy where
    show BIZ_plus = "BIZ+"
    show BIZ_minus = "BIZ-"


-- Izenen zenbagarritasuna (ZENB) - Oraindik EDBLn landu gabe.
data Zenbagarritasuna = ZENB_plus | ZENB_minus deriving (Eq,Enum,Bounded)
instance Show Zenbagarritasuna where
    show ZENB_plus = "ZENB+"
    show ZENB_minus = "ZENB-"

-- Izenen neurgarritasuna (NEUR) - - Oraindik EDBLn landu gabe.
data Neurgarritasuna = NEUR_plus | NEUR_minus deriving (Eq,Enum,Bounded)
instance Show Neurgarritasuna where
    show NEUR_plus = "NEUR+"
    show NEUR_minus = "NEUR-"

--Pluralia tantum izenak (PLU) - Oraindik EDBLn sistematikoki landu gabe.
data PluraliaTantum = PLU_plus | PLU_minus deriving (Eq,Enum,Bounded)
instance Show PluraliaTantum where
    show PLU_plus = "PLU+"
    show PLU_minus = "PLU-"

--Aditz nagusiaren laguntzaile-mota (LAGM) - Oraindik EDBLn sistematikoki landu gabe.
-- Main verb agreement type -- OBS. There's a separate tag for synthetic verbs!
data AuxType = DA --NOR
             | DU --NOR-NORK
             | DA_DU --NOR eta NOR-NORK
             | ZAIO --NOR-NORI
             | DIO --NOR-NORI-NORK
 deriving (Eq,Enum,Bounded)

instance Show AuxType where
    show DA = "DA"
    show DU = "DU"
    show DA_DU = "DA-DU"
    show ZAIO = "ZAIO"
    show DIO = "DIO"


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

--------------------------------------------
-- 2.5 Ezaugarri sintaktikoak
-- Adjektiboen posizioa (IZAUR)
data AdjPosizioak = IZAUR_plus | IZAUR_minus deriving (Eq,Enum,Bounded)

instance Show AdjPosizioak where
    show IZAUR_plus = "IZAUR+"
    show IZAUR_minus = "IZAUR-"

--Determinatzailearen posizioa sintagman (POS)  - Oraindik EDBLn landu gabe.
data DetPos = ATZE  -- Atzetik derrigorrez
            | AURRE -- Aurretik derrigorrez
            | NN    -- Nonahi, aurretik nahiz atzetik


-- Loturazkoak - Klausula muga (KLM) ) - Oraindik EDBLn landu gabe.
data Loturazkoak = HAS --Klausula-hasiera markatzen duen loturazkoa: ezen
                 | AM -- Klausula-amaiera markatzen duen loturazkoa: arren
                 | HA -- Klausula-hasiera zein -amaiera marka dezakeen loturazkoa: ?


------------------------------
-- 3   Funtzio sintaktikoak --
------------------------------

-- I guess this means if it is on the left or on the right of something?
-- So, Eskuineko ("on the right of") points to the left <, 
-- because the word itself is on the right. Same for Ezkerreko.
data Dir = Eskuineko | Ezkerreko deriving (Eq,Enum,Bounded)
instance Show Dir where
    show Eskuineko = "<"
    show Ezkerreko = ">"

data PlusMinus = Plus | Minus deriving (Eq,Enum,Bounded)
instance Show PlusMinus where
    show Plus = "+"
    show Minus = "-"

data Sintaktikoak
 = ADILOK Dir | ADLG | ATRIB | BST_sin | GRAD Dir | HAOS | IA Dir | ID Dir
 | ITJ_sin | IZLG Dir | KM_ezk | LAB_sin | LOK_sin | MP | MD_ADLG | MD_OBJ
 | OBJ | PJ | PRED | PRT_sin | SIGLA_sin | SINBOLOA | SUBJ | ZOBJ | IS | FSG

-- Jadlag
 | JADLAG PlusMinus | JADLAG_MP PlusMinus | JADLAG_MP_ADLG | JADLAG_IZLG_ezk
 | JADLAG_MP_IZLG_ezk | JADLAG_MP_IZLG_esk | JADLAG_MP_OBJ | JADLAG_MP_SUBJ | JADLAG_MP_PRED

-- Jadnag
 | JADNAG PlusMinus | JADNAG_MP PlusMinus | JADNAG_MP_ADLG PlusMinus
 | JADNAG_IZLG | JADNAG_MP_IZLG_ezk PlusMinus | JADNAG_MP_IZLG_esk PlusMinus
 | JADNAG_MP_OBJ PlusMinus | JADNAG_MP_SUBJ PlusMinus | JADNAG_MP_PRED PlusMinus
 | JADNAG_MP_ZOBJ | JADNAG_MP_KM
 deriving (Eq) -- ,Enum,Bounded)

instance Show Sintaktikoak where
    show (ADILOK Ezkerreko) = "@ADILOK>" -- Aditz konposatuen funtzioa
    show (ADILOK Eskuineko) = "<@ADILOK" -- Aditz konposatuen funtzioa
    show ADLG     = "@ADLG" --Adizlaguna
    show ATRIB    = "@ATRIB" --Atributoa (EDBLtik desagertua)
    show BST_sin  = "@BST" --Bestelakoa
    show (GRAD Ezkerreko) = "@GRAD>" -- Ezkerreko graduatzailea
    show (GRAD Eskuineko) = "@<GRAD" -- Eskuineko graduatzailea
    show HAOS     = "@HAOS" -- Hitz Anitzeko Unitatearen osagaia
    show (IA Ezkerreko) = "@IA>" --Ezkerreko adjektiboa
    show (IA Eskuineko) = "@<IA" --Eskuineko adjektiboa
    show (ID Ezkerreko) = "@ID>" --Ezkerreko determinatzailea
    show (ID Eskuineko) = "@<ID" --Eskuineko determinatzailea
    show ITJ_sin  = "@ITJ" --Interjekzioa
    show (IZLG Ezkerreko) = "@IZLG>" --Ezkerreko izenlaguna
    show (IZLG Eskuineko) = "@<IZLG" --Eskuineko izenlaguna
    show KM_ezk   = "@KM>" --Kasua daraman formaren modifikatzailea
    show LAB_sin  = "@LAB" --Laburdura
    show LOK_sin  = "@LOK" --Lokailua
    show MP       = "@MP" --Mendeko perpausa: menderagailu askea
    show MD_ADLG  = "@MD_ADLG" --Mendeko perpausa adizlagun funtzioan: menderagailu askea2
    show MD_OBJ   = "@MD_OBJ" --Mendeko perpausa objektu funtzioan: lokailu menderagailu askea3
    show OBJ      = "@OBJ" --Objektua
    show PJ       = "@PJ" --Perpaus-juntadura (koordinazioa)
    show PRED     = "@PRED" --Subjektu edo objektuaren osagarri predikatiboa
    show PRT_sin  = "@PRT" --Partikula
    show SIGLA_sin = "@SIGLA" --Sigla
    show SINBOLOA  = "@SINBOLOA" --Sinboloa / @SINBOLOA> --Sinboloa
    show SUBJ      = "@SUBJ" --Subjektua
    show ZOBJ      = "@ZOBJ" --Zehar-objektua
    show IS        = "@IS" --Funtzio jakinik gabeko izen-sintagma
    show FSG       = "@FSG" --Funtzio sintaktiko jakinik gabe

-- Jadlag
    show (JADLAG Plus) = "@+JADLAG"    -- Aditz laguntzaile jokatua (etorri naiz)
    show (JADLAG Minus) = "@-JADLAG"    -- Aditz laguntzaile jokatugabea (?etorri izan naiz)
    show (JADLAG_MP Plus) = "@+JADLAG_MP"  -- Aditz laguntzaile jokatua, mendeko perpausa
    show (JADLAG_MP Minus) = "@-JADLAG_MP"  -- Aditz laguntzaile jokatugabea, mendeko perpausa (EDBLtik desagertua1)
    show JADLAG_MP_ADLG     = "@+JADLAG_MP_ADLG" -- Aditz laguntzaile jokatua, adizlagun funtzioan1
    show JADLAG_IZLG_ezk    = "@+JADLAG_IZLG>"  -- Aditz laguntzaile jokatua, izenlagun funtzioan (desagertua)2
    show JADLAG_MP_IZLG_ezk = "@+JADLAG_MP_IZLG>" -- Aditz laguntzaile jokatua, izenlagun funtzioan
    show JADLAG_MP_IZLG_esk = "@<+JADLAG_MP_IZLG" -- Aditz laguntzaile jokatua, eskuineko izenlagun funtzioan3
    show JADLAG_MP_OBJ   = "@+JADLAG_MP_OBJ"   -- Aditz laguntzaile jokatua, mendeko objektua (duzun, dadin)
    show JADLAG_MP_SUBJ  = "@+JADLAG_MP_SUBJ"  -- Aditz laguntzaile jokatua, mendeko subjektua (EDBLn sarrerarik ez)
    show JADLAG_MP_PRED  = "@+JADLAG_MP_PRED"  -- Aditz laguntzaile jokatua, mendeko predikatzailea

-- Jadnag
    show (JADNAG Plus) = "@+JADNAG"           -- Aditz nagusi jokatua (nator)
    show (JADNAG Minus) = "@-JADNAG"          -- Aditz nagusi jokatugabea (etorri naiz)
    show (JADNAG_MP Plus) = "@+JADNAG_MP"     -- Aditz nagusi jokatua, mendeko perpausa
    show (JADNAG_MP Minus) = "@-JADNAG_MP"    -- Aditz nagusi jokatugabea, mendeko perpausa
    show (JADNAG_MP_ADLG Plus) = "@+JADNAG_MP_ADLG"  -- Aditz nagusi jokatua, adizlagun funtzioan
    show (JADNAG_MP_ADLG Minus) = "@-JADNAG_MP_ADLG"  -- Aditz nagusi jokatugabea, adizlagun funtzioan (-keran)
    show JADNAG_IZLG = "@+JADNAG_IZLG>"       -- Aditz nagusi jokatua, izenlagun funtzioan (desagertua)2
    show (JADNAG_MP_IZLG_ezk Plus) = "@+JADNAG_MP_IZLG>" -- Aditz nagusi jokatua, izenlagun funtzioan
    show (JADNAG_MP_IZLG_ezk Minus) = "@-JADNAG_MP_IZLG>" -- Aditz nagusi jokatugabea, izenlagun funtzioan (EDBLn sarrerarik ez)3
    show (JADNAG_MP_IZLG_esk Plus) = "@<+JADNAG_MP_IZLG" -- Aditz nagusi jokatua, eskuineko izenlagun funtzioan
    show (JADNAG_MP_IZLG_esk Minus) = "@<-JADNAG_MP_IZLG" -- Aditz nagusi jokatugabea, eskuinekoizenlagun funtzioan
    show (JADNAG_MP_OBJ Plus) = "@+JADNAG_MP_OBJ"   -- Aditz nagusi jokatua, objektu funtzioan (dakigun, gatozen)
    show (JADNAG_MP_OBJ Minus) = "@-JADNAG_MP_OBJ"   -- Aditz nagusi jokatugabea, objektu funtzioan (EDBLn sarrerarik ez)4
    show (JADNAG_MP_SUBJ Plus) = "@+JADNAG_MP_SUBJ"  -- Aditz nagusi jokatua, subjektu funtzioan
    show (JADNAG_MP_SUBJ Minus) = "@-JADNAG_MP_SUBJ"  -- Aditz nagusi jokatugabea, subjektu funtzioan (EDBLn sarrerarik ez)5
    show (JADNAG_MP_PRED Plus) = "@+JADNAG_MP_PRED"  -- Aditz nagusi jokatua, predikatu funtzioan
    show (JADNAG_MP_PRED Minus) = "@-JADNAG_MP_PRED"  -- Aditz nagusi jokatugabea, predikatu funtzioan
    show JADNAG_MP_ZOBJ = "@-JADNAG_MP_ZOBJ"  -- Aditz nagusi jokatugabea, zehar-objektu funtzioan
    show JADNAG_MP_KM   = "@-JADNAG_MP_KM"    -- Aditz nagusi jokatugabea, Kasua daraman formaren modifikatzailea


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
