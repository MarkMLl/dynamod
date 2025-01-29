program demo;

// This is not a functioning program, but a check that either the test or
// test_dynamic unit can be imported. In either case expect an eventual
// error due to incomplete implementation of required macros.   MarkMLl


{$mode objfpc}{$H+}

uses
  test { _dynamic } ;

begin
end.
