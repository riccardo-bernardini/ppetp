default: test

.PHONY: test
test: test_udp test_utilities

.PHONY: test_udp

test_udp: 
	@echo "--- UDP test: "
	@./test_udp.sh

.PHONY: test_utilities
test_utilities:
	@echo "--- Utilities test: "
	@gnatmake -q -Ptest test_net_utilities
	@obj/test_net_utilities
