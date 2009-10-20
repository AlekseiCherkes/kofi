################################################################################
################################################################################
#
# Main part
#
################################################################################
################################################################################

HSFLAGS := # -O2

HSFLAGS_OUR := $(HSFLAGS) -XDisambiguateRecordFields -W -iCommon -i./ -iThirdParty -XCPP
CFLAGS_UOR := -IThirdParty/CBits/sqlite3

.PHONY: all
all: server client

server: third_party Server/ServerMain.hs
	ghc --make -iServer $(DATABASE_FLAGS) -optP $(CFLAGS) -o server $(HSFLAGS_OUR) Server/ServerMain.hs $(CBITS_O)

client: third_party Client/ClientMain.hs
	ghc --make -optP $(CFLAGS) -o client $(HSFLAGS_OUR) Client/ClientMain.hs $(CBITS_O)

.PHONY: server_db
server_db: third_party Server/CreateDB.hs
	ghc --make -o server_db -iServer $(HSFLAGS_OUR) Server/CreateDB.hs $(CBITS_O)
	./server_db
	sqlite3 -init Server/fill.sql server.db

.PHONY: clean_db
clean_db:
	rm -f server.db server_db Server/ServerDB.hs Server/ServerDB/*
	rmdir Server/ServerDB

.PHONY: clean
clean:
	rm -f $(wildcard $(addprefix Server/, *.hc, *.hi, *.o, *.ho))
	rm -f client server
	echo $(wildcard $(addprefix Server/, *.hc, *.hi, *.o, *.ho))

.PHONY : clean_all
clean_all: clean_system clean_database clean_cbits clean_db clean_our clean_db

################################################################################
################################################################################
#
# Third party libraries
#
################################################################################
################################################################################

# $< - name of the prerequisite
# $@ - name of target

.PHONY: third_party
third_party: cbits system database

THIRD_PARTY_FLAGS := -iThirdParty

################################################################################
# CBits
################################################################################

CBITS_C := $(addprefix ThirdParty/CBits/sqlite3/, sqlite3.c sqlite3-local.c)
CBITS_O := $(patsubst %.c, %.o, $(CBITS_C))

CFLAGS := -DSQLITE_ENABLE_FTS3=1 # -O3 -DNDEBUG=1

.PHONY: cbits
cbits: $(CBITS_O)

$(CBITS_O): %.o: %.c
	ghc -c  -optc $(CFLAGS) $<

.PHONY: clean_cbits
clean_cbits:
	rm -f $(CBITS_O)

################################################################################
# System.Log
################################################################################

SYSTEM_DIRS := $(addprefix ThirdParty/System/, ./ Log/ Log/Handler/)

SYSTEM_HS := $(wildcard $(addsuffix *.hs, $(SYSTEM_DIRS)))
SYSTEM_HI := $(patsubst %.hs, %.hi, $(SYSTEM_HS))
SYSTEM_O  := $(patsubst %.hs, %.o,  $(SYSTEM_HS))

SYSTEM_FLAGS := $(THIRD_PARTY_FLAGS) $(HSFLAGS) \
		 -XCPP 	\
		 -XExistentialQuantification

.PHONY: system
system: $(SYSTEM_HI) $(SYSTEM_O)

$(SYSTEM_HI): %.hi: %.hs
	ghc --make -c $(SYSTEM_FLAGS) $<

$(SYSTEM_O): %.o: %.hs
	ghc --make -c $(SYSTEM_FLAGS) $<

.PHOBY: clean_system
clean_system:
	rm -f $(SYSTEM_HI) $(SYSTEM_O)

################################################################################
# Database
################################################################################

DATABASE_DIRS := $(addprefix ThirdParty/Database/, ./ HSQL/ HaskellDB/ HaskellDB/DBSpec/ HaskellDB/HSQL/ HaskellDB/Sql/)

DATABASE_HSC := $(addprefix ThirdParty/Database/, HSQL.hsc HSQL/SQLite3.hsc)
DATABASE_HS_FROM_HSC := $(patsubst %.hsc, %.hs, $(DATABASE_HSC))
DATABASE_HS := $(wildcard $(addsuffix *.hs, $(DATABASE_DIRS)))
DATABASE_HS += $(DATABASE_HS_FROM_HSC)
DATABASE_HI := $(patsubst %.hs, %.hi, $(DATABASE_HS))
DATABASE_O  := $(patsubst %.hs, %.o,  $(DATABASE_HS))

DATABASE_FLAGS := $(THIRD_PARTY_FLAGS) $(HSFLAGS) \
		  -fglasgow-exts 		  \
		  -XTypeSynonymInstances	  \
		  -XCPP				  \
		  -XRankNTypes			  \
		  -XDeriveDataTypeable		  \
		  -XOverlappingInstances	  \
		  -XFlexibleInstances		  \
		  -fallow-undecidable-instances	  \
		  -XMultiParamTypeClasses	  \
		  -XGeneralizedNewtypeDeriving    \
		  -XIncoherentInstances	          \
		  -XFunctionalDependencies	  \
		  -XFlexibleContexts	          \
		  -XPolymorphicComponents


.PHONY: database
database: $(DATABASE_HS_FROM_HSC) $(DATABASE_HI) $(DATABASE_O)

$(DATABASE_HI): %.hi: %.hs
	ghc --make -c $(DATABASE_FLAGS) $<

$(DATABASE_O): %.o: %.hs
	ghc --make -c $(DATABASE_FLAGS) $<

$(DATABASE_HS_FROM_HSC): %.hs: %.hsc
	hsc2hs $<

.PHOBY: clean_database
clean_database:
	rm -f $(DATABASE_HI) $(DATABASE_O) $(DATABASE_HS_FROM_HSC)

################################################################################