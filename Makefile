SRVDIR=/srv/http/site/

all:
	stack run site build
	rm -rf $(SRVDIR)
	mv _site/* $(SRVDIR)
