all: cabal-pack-opt.gif

cabal-pack-opt.gif: cabal-pack.gif
	gifsicle --lossy=80 -k 128 -O2 -Okeep-empty cabal-pack.gif -o cabal-pack-opt.gif

cabal-pack.gif: cabal-pack.cast
	agg --theme asciinema cabal-pack.cast cabal-pack.gif

cabal-pack.cast: cabal-pack.scenario
	asciinema-scenario cabal-pack.scenario > cabal-pack.cast

clean:
	rm cabal-pack-opt.gif cabal-pack.gif cabal-pack.cast
