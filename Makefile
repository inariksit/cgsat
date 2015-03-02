all:
	bnfc -d bnfc/Apertium.cf
	bnfc -d bnfc/CG.cf
	happy -gca CG/Par.y
	alex -g CG/Lex.x
	happy -gca Apertium/Par.y
	alex -g Apertium/Lex.x
	ghc --make CG/Test.hs -o CG/Test
	cabal configure --enable-tests
	cabal build

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
