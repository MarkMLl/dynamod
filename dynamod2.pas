(* Lazarus v0.9.24 Win-32 Lazarus v0.9.24 Win-32 Lazarus v0.9.24 Win-32 Lazarus *)
(* Lazarus v0.9.24 Linux-i386 Lazarus v0.9.24 Linux-i386 Lazarus v0.9.34 Linux- *)

unit DynaMod2;

(* This is an interim unit to implement GetLoadErrorStr(). It's inconvenient to *)
(* do this in the DynaMod unit since there are name clashes between the RTL's   *)
(* idea of LoadLibrary and the Windows unit's idea. MarkMLl.                    *)

{$mode objfpc}

interface

uses
  Classes, SysUtils; 

type
  ErrorString2= AnsiString;

function GetLoadErrorStr: ErrorString2;

implementation

{$ifdef UNIX }
uses dl;
{$else       }
uses windows;
{$endif UNIX }


function GetLoadErrorStr: ErrorString2;

var     {%H-}rc: word;

begin
{$ifdef UNIX }
  result := dl.dlerror
{$else       }
  rc := Windows.GetLastError;
  try
    result := Trim(SysErrorMessage(rc));
    if result = '' then
      result := 'Operating system error 0x' + LowerCase(IntToHex(rc, 8)) + ' (no descriptive text)'
  except
    result := 'Operating system error 0x' + LowerCase(IntToHex(rc, 8)) + ' (error getting descriptive text)'
  end
{$endif UNIX }
end { GetLoadErrorStr } ;


end.

