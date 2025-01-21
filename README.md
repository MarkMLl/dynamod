# dynamod
Dynamic-loading class, and program to wrap a .inc for static and dynamic linkage.

This repository comprises two parts:

The first is the Dynamod unit, which is a thin wrapper around a dynamic library (.DLL on Windows, .so on unix) such that after being loaded into memory it is presented as an instantiated class with each entry point being a method.

The second is the defToSDUnits program, which takes a .inc file (typically the result of a run of h2pas or a similar conversion utility) and generates .static and .dynamic Pascal interface units.

These are intended to be renamed manually such that e.g. something.inc generates something.static and something.dynamic, hence the library is wrapped by something.pas and something_dynamic.pas.

At runtime, an entrypoint godothis() in the library may be referred to as something.godothis() irrespective of whether the linkage is static or dynamic: the only thing that needs to be changed is the name of the imported unit (something or something_dynamic). See the example in the demo directory.

The advantage of this approach is that a program relying on a library linked statically may be relatively easily debugged, while a program relying on a library linked dynamically can at the very least "fail intelligently" if the library is unavailable, and can in more sophisticated situations use a magic number to check the interface version or reload the library if it is changed (see https://github.com/MarkMLl/dsocat for an example).
