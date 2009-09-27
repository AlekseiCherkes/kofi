PROGRAM := kofi
HSFLAGS :=

all:
	ghc --make $(HSFLAGS) -o $(PROGRAM) Main.hs

create_db: CreateDB.hs
	runghc CreateDB.hs

clean:
	rm -R -f *.hc *.hi *.o *.ho
	rm -f $(PROGRAM)
	rm -f create_db
	rm -f fill_db