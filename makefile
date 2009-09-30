HSFLAGS :=

all: client server

server: ServerMain.hs
	ghc --make $(HSFLAGS) -o server ServerMain.hs

client: ClientMain.hs
	ghc --make $(HSFLAGS) -o client ClientMain.hs

create_db: CreateDB.hs
	runghc CreateDB.hs
	sqlite3 -init fill.sql kofi.db

clean:
	rm -f -R *.hc *.hi *.o *.ho
	rm -f client server
	rm -f create_db
	rm -f fill_db