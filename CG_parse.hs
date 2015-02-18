module CG_parse where

import CG
import Data.Attoparsec.Text
import Prelude hiding (takeWhile)

parse :: String -> Rule
parse str = undefined

ex1 = "(-1 (\"Mari\"))" 
ex2 = "(1C Verb OR (<adj>) OR Noun)"

parseCondition :: Parser Condition
parseCondition = do
  skipWhile ((=='(') || (==' '))
  num <- signed
  skipWhile (flip elem "C*") --TODO handle *
  skipSpace
  char '('
  tags <- takeWhile ((flip notElem " ("))
  
  return $ C (Exactly num) (True,lookup (unpack tags) ctx)

ctx = [("Verb", CG.verb), ("\"Mari\"", [Lem "Mari"])]



{-
Inline OR
---------

Allowed:
REMOVE ("nem" ij) IF ((-1 (np)) OR (1 (adj)))

Not allowed:
REMOVE ("nem" ij) IF (-1 (np)) OR (1 (adj))


AND is implicit
---------------

Allowed:
REMOVE ("nem" ij) IF (-1 (np))  (1 (adj))

Not allowed:
REMOVE ("nem" ij) IF ((-1 (np))  (1 (adj)))


NOT affects one context at a time:



NEGATE affects whole context set:

REMOVE ("nem" ij) IF (NEGATE  (-1 (np)) )

REMOVE ("nem" ij) IF (NEGATE  (-1 (np)) OR (1 (adj)) )



Relation of NOT and NEGATE

* Single condition:

REMOVE ("nem" ij) IF (NEGATE  -1 ("Mari"))   === REMOVE ("nem" ij) IF (NOT  -1 ("Mari"))
REMOVE ("nem" ij) IF (NEGATE  -1 ("hargle")) === REMOVE ("nem" ij) IF (NOT  -1 ("hargle"))


* Single non-inverted disjunction:

Let's start with the positive disjunction:

   REMOVE ("nem" ij) IF ((-1 ("Mari")) OR (1 (adj)))

This will remove ij reading from "nem" in the following:

  "<Mari>"
	"Mari" np ant f sg nom
  "<nem>"
	"nem" adv
  ;	"nem" ij REMOVE:213
  "<tudja>"
	"tud" vblex pres def p3 sg

First clause is true, second is not, but for disjunction it's fine.

Let's negate the disjunction with NEGATE.

  REMOVE ("nem" ij) IF (NEGATE ((-1 ("Mari")) OR (1 (adj))))

  "<Mari>"
	"Mari" np ant f sg nom
  "<nem>"
	"nem" adv
	"nem" ij

Negate distributes over disjunction and requires both to be negative in order to apply REMOVE. This would be fine: REMOVE ("nem" ij) IF (NEGATE (((-1 ("HARGLE")) OR (1 (adj)))))


Next let's check NOT.

  REMOVE ("nem" ij) IF (NOT  ((-1 ("Mari")) OR (1 (adj))) )

  "<Mari>"
	"Mari" np ant f sg nom
  "<nem>"
	"nem" adv
  ;	"nem" ij REMOVE:215

WTF? Asked Tino Didriksen about it and he said this should be a parse error. 







-}