all: body-syntax.xsd  body_grammar.adb

%.xsd: %.rnc
	java -jar ../bin/trang/trang.jar $< $@

body_grammar.adb: body-syntax.xsd
	./xsd2ada.rb $< $@
