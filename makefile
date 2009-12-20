################################################################################
################################################################################
#
# Main part
#
################################################################################
################################################################################

HSFLAGS := # -O2

HSFLAGS_OUR := $(HSFLAGS) -XDisambiguateRecordFields -iSrc/Common -i./ -iSrc/ThirdParty -XCPP
CFLAGS_UOR := -ISrc/ThirdParty/CBits/sqlite3

.PHONY: all
all: server server_utils client bin

.PHONY : clean
clean: clean_database clean_cbits clean
	rm -f $(wildcard $(addprefix Src/Server/, *.hc, *.hi, *.o, *.ho))

################################################################################
# Server
################################################################################

HSFLAGS_SERVER := --make -iSrc/Server $(DATABASE_FLAGS) -optP $(CFLAGS) $(HSFLAGS_OUR)

.PHONY: server
server: Src/Server/Main.hs third_party
	ghc $(HSFLAGS_SERVER) -o Bin/Server/server Src/Server/Main.hs $(CBITS_O)

.PHONY: server_clean
server_clean:
	rm -f Bin/Server/server.exe

.PHONY: server_utils	
server_utils:
	ghc $(HSFLAGS_SERVER) -o Bin/Server/create_company Src/Server/CreateCompany.hs $(CBITS_O)

.PHONY: server_utils_clean
server_utils_clean:
	rm -f Bin/Server/server.exe	
	
################################################################################
# Client
################################################################################

HS_FLAGS_CLIENT := --make $(HSFLAGS_OUR) -iSrc/Client -optP $(CFLAGS) --

.PHONY: client
client: 
	ghc $(HS_FLAGS_CLIENT) -o Bin/Client/client Src/Client/Main.hs $(CBITS_O)

################################################################################
# Bin
################################################################################

.PHONY: bin
bin:
	make -C Bin

################################################################################
# Debug
################################################################################

.PHONY: debug
debug: third_party
	ghci -i./ -iSrc/ThirdParty -iSrc/Server -iSrc/Client -iSrc/Common

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
third_party: cbits database # system

THIRD_PARTY_FLAGS := -iSrc/ThirdParty

################################################################################
# CBits
################################################################################

CBITS_C := $(addprefix Src/ThirdParty/CBits/sqlite3/, sqlite3.c sqlite3-local.c)
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
# Database
################################################################################

DATABASE_DIRS := $(addprefix Src/ThirdParty/Database/, ./ HSQL/ HaskellDB/ HaskellDB/DBSpec/ HaskellDB/HSQL/ HaskellDB/Sql/)

DATABASE_HSC := $(addprefix Src/ThirdParty/Database/, HSQL.hsc HSQL/SQLite3.hsc)
DATABASE_HS_FROM_HSC := $(patsubst %.hsc, %.hs, $(DATABASE_HSC))
DATABASE_HS := $(wildcard $(addsuffix *.hs, $(DATABASE_DIRS)))
DATABASE_HS += $(DATABASE_HS_FROM_HSC)
DATABASE_HI := $(patsubst %.hs, %.hi, $(DATABASE_HS))
DATABASE_O  := $(patsubst %.hs, %.o,  $(DATABASE_HS))

HSC2HS_FLAGS := -ISrc/ThirdParty/CBits/sqlite3/

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
	hsc2hs $(HSC2HS_FLAGS) $<

.PHOBY: clean_database
clean_database:
	rm -f $(DATABASE_HI) $(DATABASE_O) $(DATABASE_HS_FROM_HSC)

################################################################################