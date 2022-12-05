all: cargo-cabal-opt.gif

cargo-cabal-opt.gif: cargo-cabal.gif
	gifsicle --lossy=80 -k 128 -O2 -Okeep-empty cargo-cabal.gif -o cargo-cabal-opt.gif

cargo-cabal.gif: cargo-cabal.cast
	agg --theme asciinema cargo-cabal.cast cargo-cabal.gif --font-family="Fira Code"

cargo-cabal.cast: cargo-cabal.scenario
	asciinema-scenario cargo-cabal.scenario > cargo-cabal.cast

clean:
	rm cargo-cabal-opt.gif cargo-cabal.gif cargo-cabal.cast
