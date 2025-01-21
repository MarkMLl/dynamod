# This is a test for defToSDUnits, this is NOT a viable asound interface.

all:
	../defToSDUnits testDefs.inc libasound

testDefs.static: testDefs.inc test-macros.inc
	defToSDUnits testDefs.inc libasound

testDefs.dynamic: testDefs.inc test-macros.inc
	defToSDUnits testDefs.inc libasound

clean:
	rm testDefs.static
	rm testDefs.dynamic
	rm *~

