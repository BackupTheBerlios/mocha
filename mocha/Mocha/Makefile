GHC = ghc
GHC_PKG = ghc-pkg
GHC_LIB_DIR = $(shell $(GHC) --print-libdir)

all: build

.DUMMY: all build clean

build:
	mkdir build
	cp -R source build
	mkdir -p build/imports/Mocha
	mkdir -p build/libs
	mkdir -p build/objects
	make build/objects/MochaBridge.o
	$(GHC) --make \
		-isource \
		-odir build/objects \
		-hidir build/imports \
		-lobjc \
		-framework Foundation \
		build/objects/MochaBridge.o \
		-package-name mocha \
		Mocha
	make build/libs/libHSProxy.a
	make build/libs/libHSmocha.a

build/libs/libHSmocha.a: $(wildcard build/objects/*.o)
	find build/objects -name '*.o' | xargs ar rcs $@
	ranlib $@

build/libs/libHSProxy.a: build/libs/HSProxy.o
	ar rcs $@ $^
	ranlib $@

build/objects/MochaBridge.o: source/Mocha/MochaBridge.m
	gcc -g -c -o $@ -Wall \
		-I/System/Library/Frameworks/Foundation.framework/Headers \
		-I$(GHC_LIB_DIR)/include $<

build/libs/HSProxy.o: source/Mocha/HSProxy.m
	gcc -g -c -o $@ -Wall \
		-I/System/Library/Frameworks/Foundation.framework/Headers \
		-I$(GHC_LIB_DIR)/include $<

clean:
	[ -d build ] && rm -rf build || exit 0

install: build install.copy install.package

install.devel: build install.symlink install.package

install.copy:
	cp -R build $(GHC_LIB_DIR)/mocha

install.symlink:
	[ -L "$(GHC_LIB_DIR)/mocha" ] && rm $(GHC_LIB_DIR)/mocha || exit 0
	(cd build; ln -s "`pwd`" $(GHC_LIB_DIR)/mocha)

install.package:
	libdir=$(GHC_LIB_DIR) $(GHC_PKG) -g -a < mocha.package.conf
	ranlib $(GHC_LIB_DIR)/mocha/libs/libHSmocha.a
	ranlib $(GHC_LIB_DIR)/mocha/libs/libHSProxy.a

remove: uninstall

uninstall:
	rm -rf $(GHC_LIB_DIR)/mocha
	$(GHC_PKG) -r mocha

