# dynamod
Dynamic-loading class, and program to wrap a .inc for load-at-startup and load-on-demand linkage.

This repository comprises two parts:

The first is the Dynamod unit, which is a thin wrapper around a dynamic library (.DLL on Windows, .so on unix) such that after being loaded into memory it is presented as an instantiated class with each entry point being a method.

The second is the defToSDUnits program, which takes a .inc file (typically the result of a run of h2pas or a similar conversion utility) and generates .static and .dynamic Pascal interface units for the at-load and on-demand cases respectively.

These are intended to be renamed manually such that e.g. something.inc generates something.static and something.dynamic, hence the library is wrapped by something.pas and something_dynamic.pas.

At runtime, an entrypoint godothis() in the library may be referred to as something.godothis() irrespective of whether the linkage is at-startup or on-demand: the only thing that needs to be changed is the name of the imported unit (something or something_dynamic). See the example in the demo directory.

The advantage of this approach is that a program relying on a library linked at-startup may be relatively easily debugged, while a program relying on a library linked on-demand can at the very least "fail intelligently" if the library is unavailable, and can in more sophisticated situations use a magic number to check the interface version or reload the library if it is changed (see https://github.com/MarkMLl/dsocat for an example).

Because the wrapper is kept thin and structured, if the units implementing the library are written in Pascal (rather than being a preexisting .so written in C etc.) they may be linked at build time making debugging (using the Lazarus IDE) relatively simple.
