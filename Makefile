
all:
	(cd cufp-api ; hpack)
	(cd reference-node ; hpack)
	(cd libclient ; hpack)
	stack test --pedantic
	stack build --pedantic
