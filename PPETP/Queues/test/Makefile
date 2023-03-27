default: test

.PHONY: test
test: test_queue test_indefinite_queue

.PHONY: test_queue
test_queue: 
	@gnatmake -q -Ptest test_shared_queue
	@obj/test_shared_queue

.PHONY: test_indefinite_queue
test_indefinite_queue: 
	@gnatmake -q -Ptest test_shared_indefinite_queue
	@obj/test_shared_indefinite_queue

