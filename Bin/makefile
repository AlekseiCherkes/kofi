################################################################################
################################################################################
#
# Main part
#
################################################################################
################################################################################

.PHONY: all
all: clean propagate_banks_manual create testing

.PHONY : clean
clean:
	make -C Server/ clean
	rm -f Server/*.db
	rm -f Client/*.db
	rm -f Client/Profiles/*.db

.PHONY: propagate_banks_manual
propagate_banks_manual:
	cp Common/BanksManual/manual.db Server
	cp Common/BanksManual/manual.db Client	

.PHONY: create
create:
	make -C Server/ create

################################################################################
# testing
################################################################################
	
.PHONY: testing
testing: testing_fill_server_db testing_generate_client_configs

.PHONY: testing_fill_server_db
testing_fill_server_db:
	python testing_fill_server_db.py

.PHONY: testing_generate_client_configs
testing_generate_client_configs:
	python testing_generate_client_configs.py

################################################################################