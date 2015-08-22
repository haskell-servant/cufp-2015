
all:
	(cd cufp-api ; hpack)
	(cd reference-node ; hpack)
	(cd example-client ; hpack)
	(cd node-template ; hpack)
	stack test --pedantic
	stack build --pedantic
