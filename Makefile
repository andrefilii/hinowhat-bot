run:
	set -a && . ./.env && set +a && cabal run

build:
	cabal build