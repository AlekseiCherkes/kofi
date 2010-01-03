################################################################################
################################################################################
#
# Main part
#
################################################################################
################################################################################

HSFLAGS := # -O2

HSFLAGS_OUR := $(HSFLAGS) -XDisambiguateRecordFields -W -iSrc/Common -i./ -iSrc/ThirdParty -XCPP
CFLAGS_UOR := -ISrc/ThirdParty/CBits/sqlite3

.PHONY: all
all: third_party create_company create_account server client

.PHONY: server
server: Src/Server/Main.hs
	ghc --make -iSrc/Server $(DATABASE_FLAGS) -optP $(CFLAGS) -o Bin/Server/server $(HSFLAGS_OUR) Src/Server/Main.hs $(CBITS_O)

.PHONY: server_create_company
create_company: Src/Server/CreateCompany.hs
	ghc --make -iSrc/Server $(DATABASE_FLAGS) -optP $(CFLAGS) -o Bin/Server/create_company $(HSFLAGS_OUR) Src/Server/CreateCompany.hs $(CBITS_O)

.PHONY: server_create_account
create_account: Src/Server/CreateAccount.hs
	ghc --make -iSrc/Server $(DATABASE_FLAGS) -optP $(CFLAGS) -o Bin/Server/create_account $(HSFLAGS_OUR) Src/Server/CreateAccount.hs $(CBITS_O)

.PHONY: client
client: third_party Src/Client/Main.hs
	ghc --make -iSrc/Client -optP $(CFLAGS) -o Bin/Client/client $(HSFLAGS_OUR) Src/Client/Main.hs $(CBITS_O)

.PHONY: dummy_client
dummy_client: Src/DummyClient/Main.hs
	ghc --make -iSrc/DummyClient -o Bin/DummyClient/client $(HSFLAGS_OUR) Src/DummyClient/Main.hs $(CBITS_O)

# .PHONY : clean
# clean: #clean_database clean # clean_cbits 
# 	rm -f $(wildcard $(addprefix Src/Server/, *.hc, *.hi, *.o, *.ho))
# 	rm -f $(wildcard $(addprefix Src/Client/, *.hc, *.hi, *.o, *.ho))

.PHONY: debug
debug: third_party
	ghci -i./ -iSrc/ThirdParty -iSrc/Server -iSrc/Client -iSrc/Common

# clean_system

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
# utf8-string
################################################################################



################################################################################
# System.Log
################################################################################

# SYSTEM_DIRS := $(addprefix SThirdParty/System/, ./ Log/ Log/Handler/)

# SYSTEM_HS := $(wildcard $(addsuffix *.hs, $(SYSTEM_DIRS)))
# SYSTEM_HI := $(patsubst %.hs, %.hi, $(SYSTEM_HS))
# SYSTEM_O  := $(patsubst %.hs, %.o,  $(SYSTEM_HS))

# SYSTEM_FLAGS := $(THIRD_PARTY_FLAGS) $(HSFLAGS) \
# 		 -XCPP 	\
# 		 -XExistentialQuantification

# .PHONY: system
# system: $(SYSTEM_HI) $(SYSTEM_O)

# $(SYSTEM_HI): %.hi: %.hs
# 	ghc --make -c $(SYSTEM_FLAGS) $<

# $(SYSTEM_O): %.o: %.hs
# 	ghc --make -c $(SYSTEM_FLAGS) $<

# .PHOBY: clean_system
# clean_system:
# 	rm -f $(SYSTEM_HI) $(SYSTEM_O)

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
		  -XUndecidableInstances	  \
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