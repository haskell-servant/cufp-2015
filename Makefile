
all:
	(cd cufp-api ; hpack)
	(cd reference-node ; hpack)
	(cd example-client ; hpack)
	stack test --pedantic
	stack build --pedantic
