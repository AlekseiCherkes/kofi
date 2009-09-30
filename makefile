PROGRAM := kofi
HSFLAGS :=

all:
	ghc --make $(HSFLAGS) -o $(PROGRAM) Main.hs

create_db: CreateDB.hs
	runghc CreateDB.hs
	sqlite3 -init fill.sql kofi.db

clean:
	rm -f -R *.hc *.hi *.o *.ho
	rm -f $(PROGRAM)
	rm -f create_db
	rm -f fill_db