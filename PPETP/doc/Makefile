all: ppetp-api.tex body-syntax.tex 

.PHONY : ppetp-api.tex  
ppetp-api.tex:
	./update-tex-image.sh ppetp-api.xds  $@

.PHONY : body-syntax.tex 
body-syntax.tex:
	./update-tex-image.sh body-syntax.rnc  $@
