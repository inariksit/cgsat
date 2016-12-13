testprog=dist/build/analyse/analyse

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
	cabal configure --user 
	cp executable/Analyse.hs AnalyseProf.hs
	cabal build

prof:
	cabal configure --enable-library-profiling --enable-executable-profiling --user
	cp executable/Analyse.hs AnalyseProf.hs
	cabal build
	cabal run profiling spa

spanish:
	runghc Matrix.hs data/spa/spa.expanded data/spa/spa.readings.withsub > data/spa/spa-ambiguity-classes
	./testSpa.sh


test-grammars:
	cat data/spa/spa.rlx | ./CG/Test | grep Successful
	cat data/nld/nld.rlx | ./CG/Test | grep Successful
	cat data/fin/fin.rlx | ./CG/Test | grep Successful
	cat data/hun_cg2.rlx | ./CG/Test | grep Successful
	cat data/eng_cg2.rlx | ./CG/Test | grep Successful

test-small:
	cat data/small/ex-C-OR-.txt | cg-conv -A > /tmp/ex-C-OR-.ape
	sudo cabal run cgsat data/small/C-OR-.rlx /tmp/ex-C-OR-.ape v 2>/dev/null | grep ";" 
	echo "VISL CG-3:"
	cat data/small/ex-C-OR-.txt | vislcg3 --trace -g data/small/C-OR-.rlx | grep ";"


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
