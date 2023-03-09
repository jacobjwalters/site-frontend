SRVDIR=/srv/http/site/

all:
	rm -rf _cache
	stack run site build
	rm -rf $(SRVDIR)
	mv _site $(SRVDIR)
