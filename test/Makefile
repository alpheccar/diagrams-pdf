all:
	ghc -o mytest --make test.hs -package-db ../dist/package.conf.inplace 

clean:
	rm -f mytest 
	rm -f *.o 
	rm -f *.hi
	rm -f *.pdf

run:
	./mytest --compressed -o circle.pdf