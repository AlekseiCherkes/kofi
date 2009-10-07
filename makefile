HSFLAGS :=	--make  	\
		-Wall		\
		-iThirdParty	\
		-fglasgow-exts 	\
		-fallow-undecidable-instances \
		-XCPP




# CFILES := ThirdParty/CBits/sqlite3.c \
# 	ThirdParty/CBits/sqlite3-local.c

CFILES := ThirdParty/CBits/network/ancilData.c \
ThirdParty/CBits/network/asyncAccept.c \
ThirdParty/CBits/network/HsNet.c \
ThirdParty/CBits/network/initWinSock.c \
ThirdParty/CBits/network/winSockErr.c \

CFLAGS := -I./ThirdParty/CBits/network

HSC2HSFLAGS := -I ../CBits/network

all: client_new

server: ServerMain.hs
	ghc --make $(HSFLAGS) -o server ServerMain.hs

client: ClientMain.hs
	ghc --make $(HSFLAGS) -o client ClientMain.hs

hsc2hs:
	hsc2hs $(HSC2HSFLAGS) ThirdParty/Network/Socket.hsc
#	hsc2hs ThirdParty/Network/Socket/Internal.hsc
#	hsc2hs TirdParty/Network/BSD.hsc
#	hsc2hs ThirdParty/Database/HSQL/SQLite3.hsc
#	hsc2hs ThirdParty/Database/HSQL.hsc

client_new:
	ghc $(HSFLAGS) $(CFILES) -optP $(CFLAGS) ClientMain.hs

create_db: CreateDB.hs
	runghc CreateDB.hs
	sqlite3 -init fill.sql kofi.db

clean:
	rm -f -R *.hc *.hi *.o *.ho
	rm -f client server