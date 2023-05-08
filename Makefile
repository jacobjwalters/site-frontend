SRVDIR=/srv/http/site/

all: build deploy

build:
	rm -rf _cache
	stack run site build

deploy:
	rm -rf $(SRVDIR)/*
	mv _site/* $(SRVDIR)


