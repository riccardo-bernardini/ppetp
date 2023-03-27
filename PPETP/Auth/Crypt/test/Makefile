all: test

test: test_sha1 test_hmac

test_sha1: 
	@echo '*** SHA1 ***'
	@gnatmake -q -Ptest test_crypt_sha1
	@obj/test_crypt_sha1

test_hmac: 
	@echo '*** HMAC-SHA1 ***'
	@gnatmake -q -Ptest test_hmac_with_sha1
	@obj/test_hmac_with_sha1
