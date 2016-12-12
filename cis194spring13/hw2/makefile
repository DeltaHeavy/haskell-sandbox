all:
	ghc -Wall main.hs && rm *.hi *.o
prof:
	ghc -prof -fprof-auto main.hs && rm *.hi *.o
runprof:
	./main +RTS -p
