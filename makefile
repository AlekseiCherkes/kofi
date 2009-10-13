HSFLAGS :=

all: client server

server: ServerMain.hs
	ghc --make $(HSFLAGS) -o server ServerMain.hs

client: ClientMain.hs
	ghc --make $(HSFLAGS) -o client ClientMain.hs

# create_db: hsc2hs CreateDB.hs
# 	runghc CreateDB.hs
# 	sqlite3 -init fill.sql kofi.db

# hsc2hs: 
# 	hsc2hs $(HSC2HSFLAGS) ThirdParty/Database/HSQL/SQLite3.hsc
# 	hsc2hs $(HSC2HSFLAGS) ThirdParty/Database/HSQL.hsc

clean:
	rm -f -R *.hc *.hi *.o *.ho
	rm -f client server