# executables
EXECUTABLES = ProxyTest.app/Contents/MacOS/ProxyTest

$(EXECUTABLES): MyObject.hs main.o
	ghc -i.. -no-hs-main --make -package mocha -framework Cocoa -o $@ $^

all: ProxyTest

main.o: main.m
	gcc -c main.m

clean: 
	-rm ProxyTest.app/Contents/MacOS/ProxyTest *{.o,.hi,_stub.*}

.DUMMY: ProxyTest

ProxyTest: ProxyTest.app/Contents/MacOS/ProxyTest

