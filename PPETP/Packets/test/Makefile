test: binary_test data_test  utility_test  control_test
GNATMAKE=gnatmake
GNATOPT=-q -Ptest

binary_test:
	@echo '*** Binary ***'
	@$(GNATMAKE) $(GNATOPT) binary_test && obj/binary_test
	@echo

data_test:
	@echo '*** Data ***'
	@$(GNATMAKE) $(GNATOPT) data_building_parsing_test  && obj/data_building_parsing_test
	@echo

control_test:
	@echo '*** Control ***'
	@$(GNATMAKE) $(GNATOPT) control_building_parsing_test  && obj/control_building_parsing_test
	@echo

utility_test:
	@echo '*** Utility ***'
	@$(GNATMAKE) $(GNATOPT) utility_test  && obj/utility_test
	@echo
