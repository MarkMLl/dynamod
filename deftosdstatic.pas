(* Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FP *)

unit defToSDStatic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

(* Write out part of the boilerplate.
*)
procedure WriteStaticTop;

(* This is defined for both static and dynamic units, but in practice the
  implementation is slightly different. Since this is a one-off, it's not worth
  messing around with a procedure parameter etc.
*)
procedure WriteMacroIncludeStatic(var xTxt: Text; const macroName, macroValue: string);

(* Write out part of the boilerplate.
*)
procedure WriteStaticMacros;

(* Write out part of the boilerplate.
*)
procedure WriteStaticBottom;


implementation

uses
  defToSDCommon, HashLines;

(* Boilerplate text is assembled as single strings and processed via a          *)
(* temporary stringlist.                                                        *)

const
  staticTop=
'// unit THIS LINE NEEDED FOR DECLARATION HINTING TO WORK' +                    QQ +
'(* Return either a static or a dynamic representation of the %%LDESC%% embedding' + QQ +
'  library (%%LNAME%%.a or similar). In this case it is static.' +              QQ +
'*)' +                                                                          QQ +
'unit %%IBASE%%;' +                                                             QQ +
'' +                                                                            QQ +
'(********************************************************************************)' + QQ +
'(*                                                                              *)' + QQ +
'(* WARNING: This is a machine-generated file and manual changes will be lost.   *)' + QQ +
'(*                                                                              *)' + QQ +
'(********************************************************************************)' + QQ +
'' +                                                                            QQ +
'{$mode objfpc}{$H+}' +                                                         QQ +
'{$packrecords C}' +                                                            QQ +
'' +                                                                            QQ +
'(*' +                                                                          QQ +
' * Please refer to %%INAME%% and other accompanying files for' +               QQ +
' * licensing and disclaimer information.' +                                    QQ +
' *)' +                                                                         QQ +
'' +                                                                            QQ +
'interface' +                                                                   QQ +
'' +                                                                            QQ +
'uses' +                                                                        QQ +
'  Classes, SysUtils%%IMPORTS%%;' +                                             QQ +
'' +                                                                            QQ +
'const' +                                                                       QQ +
'  DefaultModuleName= ''%%LNAME%%'';' +                                         QQ +
'  DefaultEarlyLoad= true;' +                                                   QQ +
'  HasLoadVarargsRoutine= true;          (* Presence is implementation-defined   *)' + QQ +
'' +                                                                            QQ +
'const' +                                                                       QQ +
'' +                                                                            QQ +
'  (* The %%LDESC%% libraries are statically linked, always return false.' +    QQ +
'  *)' +                                                                        QQ +
'  IsDynamic= false;' +                                                         QQ +
'' +                                                                            QQ +
'  (* The %%LDESC%% libraries are statically linked, if the program has started' + QQ +
'    successfully then they must be in memory.' +                               QQ +
'  *)' +                                                                        QQ +
'  ModuleInMemory= true;' +                                                     QQ +
'' +                                                                            QQ +
'{$undef DYNAMIC }' +                                                           QQ +
'{$define CONSTS }' +                                                           QQ +
'{$undef TYPES   }' +                                                           QQ +
'{$undef PROCS   }' +                                                           QQ +
'{$undef VPROCS  }' +                                                           QQ +
'{$i %%INAME%% }' +                                                             QQ +
'' +                                                                            QQ +
'{$undef CONSTS  }' +                                                           QQ +
'{$define TYPES  }' +                                                           QQ +
'{$undef PROCS   }' +                                                           QQ +
'{$undef VPROCS  }' +                                                           QQ +
'{$i %%INAME%% }' +                                                             QQ +
'' +                                                                            QQ +
'{$push }' +                                                                    QQ +
'{$macro on }' +                                                                QQ +
'{$undef CONSTS  }' +                                                           QQ +
'{$undef TYPES   }' +                                                           QQ +
'{$define PROCS  }' +                                                           QQ +
'{$define VPROCS }' +                                                           QQ +
'{$define CDECL__:= cdecl; }' +                                                 QQ +
'{$define CDECL_VARARGS__:= cdecl varargs; }' +                                 QQ +
'{$define %%MACRO%%:= external ''%%LNAME%%''; }' +                              QQ +
'{$i %%INAME%% }' +                                                             QQ +
'{$pop }' +                                                                     QQ +
'' +                                                                            QQ +
'(* For a statically-linked program this has no effect.' +                      QQ +
'*)' +                                                                          QQ +
'procedure InitialiseLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'(* For a statically-linked program this has no effect.' +                      QQ +
'*)' +                                                                          QQ +
'procedure InitializeLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'(* Load a named routine expecting a C-style variable number of parameters, or' + QQ +
'  attempt to load all varargs routines if the parameter is * or blank. For a' + QQ +
'  statically-linked program this has no effect.' +                             QQ +
'*)' +                                                                          QQ +
'procedure LoadVarargsRoutine(loadName: string= '''';' +                        QQ +
'                                        keepGoing: boolean= false);' +         QQ +
'' +                                                                            QQ +
'implementation';

  staticBottom=
'' +                                                                            QQ +
'{$push } {$hints off   Suppress hints about parameters being unused }' +       QQ +
'' +                                                                            QQ +
'(* For a statically-linked program this has no effect.' +                      QQ +
'*)' +                                                                          QQ +
'procedure InitialiseLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'begin' +                                                                       QQ +
'end { InitialiseLibrary } ;' +                                                 QQ +
'' +                                                                            QQ +
'{$pop }' +                                                                     QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'(* For a statically-linked program this has no effect.' +                      QQ +
'*)' +                                                                          QQ +
'procedure InitializeLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'begin' +                                                                       QQ +
'  InitialiseLibrary(moduleName, earlyLoad)' +                                  QQ +
'end { InitializeLibrary } ;' +                                                 QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'(* Load a named routine expecting a C-style variable number of parameters, or' + QQ +
'  attempt to load all varargs routines if the parameter is * or blank. For a' + QQ +
'  statically-linked program this has no effect.' +                             QQ +
'*)' +                                                                          QQ +
'procedure LoadVarargsRoutine(loadName: string= '''';' +                        QQ +
'                                        keepGoing: boolean= false);' +         QQ +
'begin' +                                                                       QQ +
'end { LoadVarargsRoutine } ;' +                                                QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'end.';


(* Write out part of the boilerplate.
*)
procedure WriteStaticTop;

var
  scratch: TStringList;
  i: integer;
  buffer: string;

begin
  scratch := TStringList.Create;
  try
    scratch.Text := staticTop;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn1(sTxt, buffer)
    end
  finally
    FreeAndNil(scratch)
  end
end { WriteStaticTop } ;


(* Write out part of the boilerplate.
*)
procedure WriteStaticMacros;

var
  i: integer;
  declaration, routine: string;

begin
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if RightStr(declaration, 1) = ';' then begin
      routine := ExtractRoutineName(declaration);
      HWriteLn1(sTxt);
      HWriteLn1(sTxt);
      HWriteln1(sTxt, declaration);
      HWriteLn1(sTxt);
      HWriteLn1(sTxt, 'begin');
      if Pos('function ', declaration) = 1 then
        HWriteLn1(sTxt, '//  result :=');
      HWriteLn1(sTxt, '{$error Local function/procedure must be implemented manually }');
      HWriteLn1(sTxt, 'end { ' + routine + ' } ;');
      HWriteLn1(sTxt)
    end
  end
end { WriteStaticMacros } ;


(* Write out part of the boilerplate.
*)
procedure WriteStaticBottom;

var
  scratch: TStringList;
  i: integer;
  buffer: string;

begin
  scratch := TStringList.Create;
  try
    scratch.Text := staticBottom;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn1(sTxt, buffer)
    end
  finally
    FreeAndNil(scratch)
  end
end { WriteStaticBottom } ;


(* This is defined for both static and dynamic units, but in practice the
  implementation is slightly different. Since this is a one-off, it's not worth
  messing around with a procedure parameter etc.
*)
procedure WriteMacroIncludeStatic(var xTxt: Text; const macroName, macroValue: string);

begin
  HWriteLn1(xTxt);
  HWriteLn1(xTxt);
  HWriteLn1(xTxt, '(* Procedures and functions which are defined locally rather than being part of *)');
  HWriteLn1(xTxt, '(* the external library, and which in C would often be implemented as macros,   *)');
  HWriteLn1(xTxt, '(* are defined in this file to protect them from being overwritten.             *)');
  HWriteLn1(xTxt);
  HWriteLn1(xTxt, '{$define ' + macroName + ':= ' + macroValue + ' }');
  HWriteLn1(xTxt, '{$i ' + iBase + '-macros.inc' + ' }');
  HWriteLn1(xTxt)
end { WriteMacroIncludeStatic } ;


end.

