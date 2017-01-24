
module Euskara where

--------------------------------------------
-- 1    Tokenizazioan ezarritako etiketak --
--------------------------------------------

-- 1.1    Puntuazio-markak

data Puntuazio = PUNT_PUNT | PUNT_KOMA | PUNT_PUNT_KOMA 
               | PUNT_BI_PUNT | PUNT_ESKL | PUNT_GALD | PUNT_HIRU
 deriving (Show,Eq)

-- 1.2    Ortografia
data Ortografia = HAS_MAI | DEN_MAI
 deriving (Show,Eq)

--------------------------------------------------------------------------------

---------------------------------
-- 2    EDBLko etiketa-sistema --
---------------------------------

--------------------------------------------
-- 2.1    Kategoria lexikalak
-- Kategoria (KAT) eta azpikategoria (AZP) nagusiak
data KategoriaLex = IZE IzeAZP -- Noun
                  | ADI AdiAZP -- Verb
                  | ADJ AdbAZP -- Adjective. OBS. same subcat!
                  | ADB AdbAZP -- Adverb
                  | DET DetAZP 
                  | IOR IorAZP -- Pronoun
                  | LOT LotAZP -- Connective
                  | PRT -- Partikula
                  | ITJ -- Interjekzioa
                  | BST -- Bestelakoa

    -- Kategoria lagungarriak
                  | ADL -- Aditz laguntzailea: du
                  | ADT -- Aditz trinkoa: dator
                  | SIG -- Sigla
                  | EHU
                  | SNB --Sinboloa: km, cm, g
                  | LAB --Laburdura: etab.
 deriving (Show,Eq)

-- Azpikategoria
data IzeAZP = ARR -- Izen arrunta 
            | IZB -- Pertsona-izen berezia
            | LIB -- Leku-izen berezia
            | ZKI -- Izen zenbakia
 deriving (Show,Eq)


data AdbAZP = ARR_adb -- Adberbio arrunta
            | GAL --Adberbio galdetzailea
 deriving (Eq)
-- The tag ARR is in both Ize and Adb, hence different constructor and custom Show instance
instance Show AdbAZP where 
    show ARR_adb = "ARR"
    show GAL = "GAL"


data AdiAZP = SIN -- Aditz sinplea
            | ADK -- Aditz konposatua
            | FAK --Aditz faktitiboa
 deriving (Show,Eq)

data DetAZP = ERKARR -- Determinatzailea: erakusle arrunta
            | ERKIND -- Determinatzailea: erakusle indartua
            | NOLARR -- Det.: nolakotzaile arrunta
            | NOLGAL --Det.: nolakotzaile galdetzailea
            | DZH -- Det.: zenbatzaile zehaztua
            | BAN -- Det.: zenbatzaile banatzailea
            | ORD --Det.: zenbatzaile ordinala
            | DZG --Det.: zenbatzaile zehaztugabea
            | ORO --Det.: zenbatzaile orokorra
 deriving (Show,Eq)

data IorAZP = PERARR -- Pertsona-izenordain arrunta
            | PERIND -- Pertsona-izenordain indartua
            | IZGMGB -- Izenordain zehaztugabe mugagabea
            | IZGGAL -- Izenordain zehaztugabe galdetzailea
            | BIH -- Izenordain bihurkaria
            | ELK --Izenordain elkarkaria
 deriving (Show,Eq)

data LotAZP = LOK -- Loturazko lokailuak
            | JNT -- Loturazko juntagailua
 deriving (Show,Eq)

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
 deriving (Show,Eq)


--------------------------------------------
-- 2.3 Morfologia-ezaugarriak
data Kasumarkak = ABL | ABS | ABU | ABZ | ALA | BNK 
                | DAT | DES | DESK | ERG | GEL | GEN
                | INE | INS | MOT | PAR | PRO | SOZ
 deriving (Show,Eq)

data Mugatasuna = MUGM | MG deriving (Show,Eq)

data Numeroa = NUMS | NUMP | PH deriving (Show,Eq)

data Gradumaila = KONP | SUP | GEHI deriving (Show,Eq)

data Aditzmota = PART | ADOIN | ADIZE deriving (Show,Eq)

data Aspektua = BURU | EZBURU | GERO | PNT deriving (Show,Eq)

data Modudenbora =
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
   | C -- Aginterazko orainaldia: hadi
 deriving (Show,Eq)

-- Verb agreement: 
-- has 1-3 of the following, either Nor, Nor-Nori, Nor-Nork or Nor-Nori-Nork
data Nor = NR_NI | NR_HI | NR_HURA | NR_GU 
         | NR_ZU | NR_ZUEK | NR_HAIEK deriving (Show,Eq)

data Nori = NI_NIRI | NI_HIRI | NI_HARI | NI_GURI 
          | NI_ZURI | NI_ZUEI | NI_HAIEI deriving (Show,Eq)
 
data Nork = NK_NIK | NK_HIK | NK_HARK | NK_GUK 
          | NK_ZUK | NK_ZUEK_K | NK_HAIEK_K deriving (Eq)

instance Show Nork where
    show NK_NIK = "NK_NIK"
    show NK_HIK = "NK_HIK"
    show NK_HARK = "NK_HARK" 
    show NK_GUK  = "NK_GUK"
    show NK_ZUK  = "NK_ZUK"
    show NK_ZUEK_K = "NK_ZUEK-K"
    show NK_HAIEK_K = "NK_HAIEK-K"


--Hitanozko forma alokutiboak (HIT)
data Hitano = TO -- Gizonezkoen hitanoa
            | NO -- Andrazkoen hitanoa
 deriving (Show,Eq)

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
--               | BALD -- Baldintzazkoa
--               | DENB -- Denborazkoa
               | EMEN -- Emendiozkoa 
--               | ESPL -- Esplikatiboa 
               | HAUT -- Hautazkoa
--               | KAUS -- Kausazkoa
--               | KONPL -- Konpletiboa
--               | KONT -- Kontzesiboa
--               | MOD  -- Moduzkoa
               | ONDO --Ondoriozkoa
 deriving (Show,Eq)

--------------------------------------------
-- 2.4 Ezaugarri lexiko-semantikoak
-- Determinatzaileen numero-mugatasuna (NMG)
data DetMugatasuna = NMG -- Mugabea
                   | NMGS -- Singularra
                   | NMGP -- Plurala
 deriving (Eq)

instance Show DetMugatasuna where
    show NMG = "MG"
    show NMGS = "NMGS"
    show NMGP = "NMGP"

-- Izenordainen numeroa (NUM)
-- TODO do we need this?
-- data IzenNumero = 

--Izenordainen pertsona (PER)
data Pertsona = NI | HI | HURA | GU | ZU | ZUEK | HAIEK deriving (Show,Eq)

-- Izenen biziduntasuna (BIZ) - Oraindik EDBLn sistematikoki landu gabe.
data Biziduntasuna = BIZ_plus -- Biziduna 
                   | BIZ_minus -- Bizigabea
 deriving (Eq)

instance Show Biziduntasuna where
    show BIZ_plus = "BIZ+"
    show BIZ_minus = "BIZ-"


-- Izenen zenbagarritasuna (ZENB) - Oraindik EDBLn landu gabe.
data Zenbagarritasuna = ZENB_plus | ZENB_minus deriving (Eq)
instance Show Zenbagarritasuna where
    show ZENB_plus = "ZENB+"
    show ZENB_minus = "ZENB-"

-- Izenen neurgarritasuna (NEUR) - - Oraindik EDBLn landu gabe.
data Neurgarritasuna = NEUR_plus | NEUR_minus deriving (Eq)
instance Show Neurgarritasuna where
    show NEUR_plus = "NEUR+"
    show NEUR_minus = "NEUR-"

--Pluralia tantum izenak (PLU) - Oraindik EDBLn sistematikoki landu gabe.
data PluraliaTantum = PLU_plus | PLU_minus deriving (Eq)
instance Show PluraliaTantum where
    show PLU_plus = "PLU+"
    show PLU_minus = "PLU-"

--Aditz nagusiaren laguntzaile-mota (LAGM) - Oraindik EDBLn sistematikoki landu gabe.
-- Verb agreement type
data AditzLaguntzaile = DA --NOR
                      | DU --NOR-NORK
                      | DA_DU --NOR eta NOR-NORK
                      | ZAIO --NOR-NORI
                      | DIO --NOR-NORI-NORK
 deriving (Eq)

instance Show AditzLaguntzaile where
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
data AdjPosizioak = IZAUR_plus | IZAUR_minus deriving (Eq)

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

--------------------------------------------------------------------------------

------------------------------
-- 3   Funtzio sintaktikoak --
------------------------------

data Dir = Eskuina | Ezkerra deriving (Eq)
instance Show Dir where
    show Eskuina = "<"
    show Ezkerra = ">"

data PlusMinus = Plus | Minus deriving (Eq)
instance Show PlusMinus where
    show Plus = "+"
    show Minus = "-"

data Sintaktikoak = ADILOK Dir -- @ADILOK> -- Aditz konposatuen funtzioa
                               -- <@ADILOK -- Aditz konposatuen funtzioa
                 | ADLG     -- @ADLG --Adizlaguna
                 | ATRIB    -- @ATRIB --Atributoa (EDBLtik desagertua)
                 | BST_sin  -- @BST --Bestelakoa
                 | GRAD Dir -- @GRAD> -- Ezkerreko graduatzailea
                            --  @<GRAD -- Eskuineko graduatzailea
                 | HAOS     -- @HAOS -- Hitz Anitzeko Unitatearen osagaia
                 | IA Dir   -- @<IA --Eskuineko adjektiboa
                            -- @IA> --Ezkerreko adjektiboa
                 | ID Dir   -- @ID> --Ezkerreko determinatzailea
                            -- @<ID --Eskuineko determinatzailea
                 | ITJ_sin  -- @ITJ --Interjekzioa
                 | IZLG Dir -- @<IZLG --Eskuineko izenlaguna
                            -- @IZLG> --Ezkerreko izenlaguna
                 | KM_ezk   -- @KM> --Kasua daraman formaren modifikatzailea
                 | LAB_sin  -- @LAB --Laburdura
                 | LOK_sin  -- @LOK --Lokailua
                 | MP       -- @MP --Mendeko perpausa: menderagailu askea
                 | MD_ADLG  -- @MD_ADLG --Mendeko perpausa adizlagun funtzioan: menderagailu askea2
                 | MD_OBJ   -- @MD_OBJ --Mendeko perpausa objektu funtzioan: lokailu menderagailu askea3
                 | OBJ      -- @OBJ --Objektua
                 | PJ       -- @PJ --Perpaus-juntadura (koordinazioa)
                 | PRED     -- @PRED --Subjektu edo objektuaren osagarri predikatiboa
                 | PRT_sin  -- @PRT --Partikula
                 | SIGLA_sin -- @SIGLA --Sigla
                 | SINBOLOA  -- @SINBOLOA --Sinboloa / @SINBOLOA> --Sinboloa
                 | SUBJ      -- @SUBJ --Subjektua
                 | ZOBJ      -- @ZOBJ --Zehar-objektua
                 | IS        -- @IS --Funtzio jakinik gabeko izen-sintagma
                 | FSG       -- @FSG --Funtzio sintaktiko jakinik gabe

--Jadlag
                 | JADLAG PlusMinus    -- @+JADLAG     -- Aditz laguntzaile jokatua (etorri naiz)
                                       -- @-JADLAG     -- Aditz laguntzaile jokatugabea (?etorri izan naiz)
                 | JADLAG_MP PlusMinus -- @+JADLAG_MP  -- Aditz laguntzaile jokatua, mendeko perpausa
                                       -- @-JADLAG_MP  -- Aditz laguntzaile jokatugabea, mendeko perpausa (EDBLtik desagertua1)
                 | JADLAG_MP_ADLG     -- @+JADLAG_MP_ADLG -- Aditz laguntzaile jokatua, adizlagun funtzioan1
                 | JADLAG_IZLG_ezk    -- @+JADLAG_IZLG>   -- Aditz laguntzaile jokatua, izenlagun funtzioan (desagertua)2
                 | JADLAG_MP_IZLG_ezk -- @+JADLAG_MP_IZLG> -- Aditz laguntzaile jokatua, izenlagun funtzioan
                 | JADLAG_MP_IZLG_esk -- @<+JADLAG_MP_IZLG -- Aditz laguntzaile jokatua, eskuineko izenlagun funtzioan3
                 | JADLAG_MP_OBJ   -- @+JADLAG_MP_OBJ   -- Aditz laguntzaile jokatua, mendeko objektua (duzun, dadin)
                 | JADLAG_MP_SUBJ  -- @+JADLAG_MP_SUBJ  -- Aditz laguntzaile jokatua, mendeko subjektua (EDBLn sarrerarik ez)
                 | JADLAG_MP_PRED  -- @+JADLAG_MP_PRED  -- Aditz laguntzaile jokatua, mendeko predikatzailea

-- Jadnag
                 | JADNAG PlusMinus -- @+JADNAG          -- Aditz nagusi jokatua (nator)
                                    -- @-JADNAG          -- Aditz nagusi jokatugabea (etorri naiz)
                 | JADNAG_MP PlusMinus -- @+JADNAG_MP    -- Aditz nagusi jokatua, mendeko perpausa
                                       -- @-JADNAG_MP    -- Aditz nagusi jokatugabea, mendeko perpausa
                 | JADNAG_MP_ADLG PlusMinus -- @+JADNAG_MP_ADLG  -- Aditz nagusi jokatua, adizlagun funtzioan
                                            -- @-JADNAG_MP_ADLG  -- Aditz nagusi jokatugabea, adizlagun funtzioan (-keran)
                 | JADNAG_IZLG -- @+JADNAG_IZLG>    -- Aditz nagusi jokatua, izenlagun funtzioan (desagertua)2
                 | JADNAG_MP_IZLG_ezk PlusMinus -- @+JADNAG_MP_IZLG> -- Aditz nagusi jokatua, izenlagun funtzioan
                                                -- @-JADNAG_MP_IZLG> -- Aditz nagusi jokatugabea, izenlagun funtzioan (EDBLn sarrerarik ez)3
                 | JADNAG_MP_IZLG_esk PlusMinus -- @<+JADNAG_MP_IZLG -- Aditz nagusi jokatua, eskuineko izenlagun funtzioan
                                                -- @<-JADNAG_MP_IZLG -- Aditz nagusi jokatugabea, eskuinekoizenlagun funtzioan
                 | JADNAG_MP_OBJ PlusMinus -- @+JADNAG_MP_OBJ   -- Aditz nagusi jokatua, objektu funtzioan (dakigun, gatozen)
                                           -- @-JADNAG_MP_OBJ   -- Aditz nagusi jokatugabea, objektu funtzioan (EDBLn sarrerarik ez)4
                 | JADNAG_MP_SUBJ PlusMinus -- @+JADNAG_MP_SUBJ  -- Aditz nagusi jokatua, subjektu funtzioan
                                            -- @-JADNAG_MP_SUBJ  -- Aditz nagusi jokatugabea, subjektu funtzioan (EDBLn sarrerarik ez)5
                 | JADNAG_MP_PRED PlusMinus -- @+JADNAG_MP_PRED  -- Aditz nagusi jokatua, predikatu funtzioan
                                            -- @-JADNAG_MP_PRED  -- Aditz nagusi jokatugabea, predikatu funtzioan
                 | JADNAG_MP_ZOBJ -- @-JADNAG_MP_ZOBJ  -- Aditz nagusi jokatugabea, zehar-objektu funtzioan
                 | JADNAG_MP_KM   -- @-JADNAG_MP_KM    -- Aditz nagusi jokatugabea, Kasua daraman formaren modifikatzailea


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