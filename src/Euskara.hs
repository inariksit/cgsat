
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


data AdbAZP = ARRadb -- Adberbio arrunta
            | GAL --Adberbio galdetzailea
 deriving (Eq)
-- The tag ARR is in both Ize and Adb, hence different constructor and custom Show instance
instance Show AdbAZP where 
    show ARRadb = "ARR"
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

data Nor = NI | HI | HURA | GU | ZU | ZUEK | HAIEK deriving (Show,Eq)

data Nori = NIRI | HIRI | HARI | GURI | ZURI | ZUEI | HAIEI deriving (Show,Eq)

data Nork = NIK | HIK | HARK | GUK | ZUK | ZUEK_K | HAIEK_K deriving (Eq)

instance Show Nork where
    show NIK = "NIK"
    show HIK = "HIK"
    show HARK = "HARK" 
    show GUK  = "GUK"
    show ZUK  = "ZUK"
    show ZUEK_K = "ZUEK-K"
    show HAIEK_K = "HAIEK-K"


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
data Numeromugatasuna = NMG -- Mugabea
                      | NMGS -- Singularra
                      | NMGP -- Plurala
 deriving (Eq)

instance Show Numeromugatasuna where
    show NMG = "MG"
    show NMGS = "NMGS"
    show NMGP = "NMGP"


