This repo is used for the tutorial

# T13: Web Programming with Servant

at CUFP-2015.

Link to the tutorial: http://cufp.org/2015/t13-julian-arni-servant.html

Link to this repo: https://github.com/haskell-servant/cufp-2015

## Setup Script

There is a script that tries to simplify installing all needed tools and
dependencies. It uses stack (https://github.com/commercialhaskell/stack)

``` shell ./setup.sh ```

## Contained Projects

This repo consist of multiple projects:

- *cufp-api*: The encoding of an API that all other projects use.

- *example-client*: A command line client to access a server through the
defined API.

- *reference-node*: A reference implementation of a server serving the defined
API.

- *node-template*: A skeleton of a server implementation to make it easier for
you to start writing your own.  (You could copy or rename the folder, but you
don't have to.)
