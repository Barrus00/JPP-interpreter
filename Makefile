all:
	mkdir -p build
	ghc --make -outputdir build -o interpreter -isrc/ -iparser/ Main.hs

clean:
	rm -r build/ interpreter
