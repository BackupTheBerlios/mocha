GHC = ghc

all: URLFetcher NSMovieTest NSArrayTest

NSArrayTest: NSArrayTest.hs
	-rm Main.o
	$(GHC) -o $@ -package mocha --make -framework Cocoa $<

NSMovieTest: NSMovieTest.hs
	-rm Main.o
	$(GHC) -o $@ -package mocha --make -framework Cocoa $<

URLFetcher: URLFetcher.hs
	-rm Main.o
	$(GHC) -o $@ -package mocha --make -framework Foundation $<

clean:
	-rm *{.o,.hi,_stub.*} NSMovieTest URLFetcher NSArrayTest

