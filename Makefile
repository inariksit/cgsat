testprog=dist/build/test-cgsat/test-cgsat

default: grammars rest

grammars:
	bnfc -d bnfc/Apertium.cf
	bnfc -d bnfc/CG.cf
	happy -gca CG/Par.y
	alex -g CG/Lex.x
	happy -gca Apertium/Par.y
	alex -g Apertium/Lex.x
	ghc --make CG/Test.hs -o CG/Test
	ghc --make Apertium/Test.hs -o Apertium/Test

rest:
	cabal configure --enable-tests
	cabal build

test-pride:
	$(testprog) data/eng_cg2.rlx data/pride.txt 2>/dev/null 

test-spanish:
	$(testprog) data/spa_cg3.rlx data/spa_data.txt 2>/dev/null 
	$(testprog) data/spa_cg3.rlx data/spa_pobres.txt  2>/dev/null 

test-hungarian:
	$(testprog) data/hun_cg2.rlx data/hun_data.txt -v 2>/dev/null

test-grammars:
	cat data/eng_cg2.rlx | ./CG/Test | grep Successful
	cat data/spa_cg3.rlx | ./CG/Test | grep Successful
	cat data/hun_cg2.rlx | ./CG/Test | grep Successful
	cat data/bre_cg2.rlx | ./CG/Test| grep Successful


clean:
	-rm -f CG/*.log CG/*.aux CG/*.hi CG/*.o CG/*.dvi
	-rm -f CG/Doc.ps
	-rm -f Apertium/*.log Apertium/*.aux Apertium/*.hi Apertium/*.o Apertium/*.dvi
	-rm -f Apertium/Doc.ps

distclean: clean
	-rm -f CG/Doc.* CG/Lex.* CG/Par.* CG/Layout.* CG/Skel.* CG/Print.* CG/Test.* CG/Abs.* CG/Test CG/ErrM.* CG/SharedString.* CG/ComposOp.* CG/CG.dtd CG/XML.* Makefile*
		-rmdir -p CG/

	-rm -f Apertium/Doc.* Apertium/Lex.* Apertium/Par.* Apertium/Layout.* Apertium/Skel.* Apertium/Print.* Apertium/Test.* Apertium/Abs.* Apertium/Test Apertium/ErrM.* Apertium/SharedString.* Apertium/ComposOp.* Apertium/CG.dtd Apertium/XML.* Makefile*
		-rmdir -p Apertium/
