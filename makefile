PROGRAM := kofi
HSFLAGS :=

all: create_db fill_db
	ghc --make $(HSFLAGS) -o $(PROGRAM) Main.hs

create_db: CreateDB.hs
	ghc --make $(HSFLAGS) -o create_db CreateDB.hs

fill_db: FillDB.hs
	ghc --make $(HSFLAGS) -o fill_db FillDB.hs

clean:
	rm -R -f *.hc *.hi *.o *.ho
	rm -f $(PROGRAM)
	rm -f create_db
	rm -f fill_db