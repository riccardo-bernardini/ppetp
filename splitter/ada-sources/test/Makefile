all: test-network

test-network: 
	gnatmake -c -Ptest test_network.adb
	cd obj ; \
	gnatbind test_network.ali ; \
	gnatlink -g -fprofile-generate test_network.ali ../../interface_to_ip.o	 ; \
	mv *.gcno /tmp ; \
	../test_network-server.rb
