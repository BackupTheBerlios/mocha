default: ListFoundationClasses ListCocoaClasses

ClassListTemplateTest: ClassListTemplateTest.hs lookup_loaded_objc_classes.o
	ghc -o $@ --make -fglasgow-exts lookup_loaded_objc_classes.o -lobjc -framework Foundation $<

ListFoundationClasses: lookup_loaded_objc_classes.o
	ghc -o $@ --make -fglasgow-exts lookup_loaded_objc_classes.o -lobjc -framework Foundation ClassList.hs

ListCocoaClasses: lookup_loaded_objc_classes.o
	ghc -o $@ --make -fglasgow-exts lookup_loaded_objc_classes.o -lobjc -framework Cocoa ClassList.hs

lookup_loaded_objc_classes.o: lookup_loaded_objc_classes.m
	gcc -g -c -o $@ -Wall $<

clean:
	-rm *.hi *.o ListFoundationClasses ListCocoaClasses ClassListTemplateTest

