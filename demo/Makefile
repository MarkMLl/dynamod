# This is a test for defToSDUnits, this is NOT a viable asound interface.

all: testDefs.inc test-macros.inc demo.pas Makefile
	../defToSDUnits testDefs.inc libasound
	cp testDefs.static test.pas
	cp testDefs.dynamic test_dynamic.pas
	fpc -Fu.. demo.pas

testDefs.static: testDefs.inc test-macros.inc Makefile
	defToSDUnits testDefs.inc libasound
	cp testDefs.static test.pas

testDefs.dynamic: testDefs.inc test-macros.inc Makefile
	defToSDUnits testDefs.inc libasound
	cp testDefs.dynamic test_dynamic.pas

demo: testDefs.inc test-macros.inc demo.pas Makefile
	fpc demo.pas

clean:
	rm -f testDefs.static testDefs.dynamic
	rm -f test.pas test_dynamic.pas
	rm -f *~ demo
