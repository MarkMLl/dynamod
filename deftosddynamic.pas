(* Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FP *)

unit defToSDDynamic;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

(* Write out part of the boilerplate.
*)
procedure WriteDynamicTop;

(* Write a procedure type corresponding to every dynamically-imported entry
  point. This expands CDECL__ and CDECL_VARARGS__ macros.
*)
procedure WriteDynamicTypes;

(* Write the procedure variables embedded in the class definition and include
  the original file as method declarations with CDECL__ and CDECL_VARARGS__
  undefined.
*)
procedure WriteDynamicProcVars;

(* As of FPC v3 it is not possible to write a Pascal shim which invokes a C
  function expecting a variable number of parameters. Expose the procedure
  variables as properties, and expect the user to invoke LoadVarargsRoutine()
  manually.
*)
procedure WriteDynamicProperties;

(* This is the first variant of the constructor, which tries to preload all of
  the entry point variables except for those with a variable number of arguments
  which are handled by a separately-generated LoadVarargsRoutine() procedure.
*)
procedure WriteDynamicCreateInit;

(* This is the second variant of the constructor, which nils all of the entry
  point variables including those with a variable number of arguments.
*)
procedure WriteDynamicCreateNil;

(* Generate a function which loads either a single or all of the entry point
  variables. If the first parameter is blank indicating that all should be
  loaded and one of them fails, then if the second parameter is false an
  exception is raised, otherwise the operation is continued.
*)
procedure WriteDynamicLoadVarargsRoutine;

(* Entry points satisfied by the dynamic library (i.e. where the declaration is
  terminated by a macro name) may be generated completely. The Pascal simulation
  of C-type macros (i.e. where the declaration is not terminated by a macro
  name) will need manual attention. This suppresses CDECL__ and CDECL_VARARGS__
  macros.
*)
procedure WriteDynamicSubs;

(* This is defined for both static and dynamic units, but in practice the
  implementation is slightly different. Since this is a one-off, it's not worth
  messing around with a procedure parameter etc.
*)
procedure WriteMacroIncludeDynamic(var xTxt: Text; const macroName, macroValue: string);

(* Entry points satisfied by the dynamic library (i.e. where the declaration is
  terminated by a macro name) may be generated completely. The Pascal simulation
  of C-type macros (i.e. where the declaration is not terminated by a macro
  name) will need manual attention. This supresses CDECL__ and CDECL_VARARGS__
  macros.
*)
procedure WriteDynamicSubsAndMacros;

(* Write out part of the boilerplate.
*)
procedure WriteDynamicBottom;


implementation

uses
  defToSDCommon, hashLines, StrUtils;

(* Boilerplate text is assembled as single strings and processed via a          *)
(* temporary stringlist.                                                        *)

const
  dynamicTop=
'unit %%IBASE%%_dynamic;' +                                                     QQ +
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
'  Classes, SysUtils%%IMPORTS%%, DynamicModule;' +                              QQ +
'' +                                                                            QQ +
'const' +                                                                       QQ +
'  DefaultModuleName= ''%%LNAME%%.so'';' +                                      QQ +
'  DefaultEarlyLoad= true;' +                                                   QQ +
'  HasLoadVarargsRoutine= true;          (* Presence is implementation-defined   *)' + QQ +
'' +                                                                            QQ +
'{$define DYNAMIC }' +                                                          QQ +
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
'(* These are the type descriptions of fields used to hold references to the     *)' + QQ +
'(* methods listed in %%INAME%% (below). It is important that these types  *)' + QQ +
'(* be considered subordinate to the definitions in the include file, since as   *)' + QQ +
'(* well as being used here the include file might potentially be treated by the *)' + QQ +
'(* shared library as the authoritative list of what it is to export.            *)' + QQ +
'' +                                                                            QQ +
'';

  dynamicMid1=
'  public' +                                                                    QQ +
'    (* Case-sensitivity depends on the operating system and filesystem.' +     QQ +
'    *)' +                                                                      QQ +
'    constructor Create(const LoadName: string);' +                                   QQ +
'    procedure LoadVarargsRoutine(loadName: string= ''''; keepGoing: boolean= false);' + QQ +
'    {$push }' +                                                                QQ +
'    {$macro on }' +                                                            QQ +
'    {$undef CONSTS }' +                                                        QQ +
'    {$undef TYPES  }' +                                                        QQ +
'    {$define PROCS }' +                                                        QQ +
'    {$undef VPROCS }' +                                                        QQ +
'    {$define CDECL__:= }' +                                                    QQ +
'    {$define CDECL_VARARGS__:= }' +                                            QQ +
'    {$define %%MACRO%%:= }' +                                                  QQ +
'    {$i %%INAME%% }' +                                                         QQ +
'    {$pop }';

  dynamicMid2=
'  end;' +                                                                      QQ +
'' +                                                                            QQ +
'(* Return either a static or a dynamic representation of the %%LDESC%% embedding' + QQ +
'  library (%%LNAME%%.so or similar). In this case it is dynamic.' +            QQ +
'*)' +                                                                          QQ +
'function %%IBASE%%(): T%%IBASE%%; inline;' +                                   QQ +
'' +                                                                            QQ +
'(* Create the object and optionally load the shared object library.' +         QQ +
'*)' +                                                                          QQ +
'procedure InitialiseLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'(* Create the object and optionally load the shared object library.' +         QQ +
'*)' +                                                                          QQ +
'procedure InitializeLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'implementation' +                                                              QQ +
'' +                                                                            QQ +
'var' +                                                                         QQ +
'  x%%IBASE%%: T%%IBASE%%= nil;' +                                              QQ +
'' +                                                                            QQ +
'';

  dynamicBottom=
'' +                                                                            QQ +
'(*************************************************************************** DB *)' + QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'(* Return either a static or a dynamic representation of the %%LDESC%% embedding' + QQ +
'  library (%%LNAME%%.so or similar). In this case it is dynamic.' +            QQ +
'*)' +                                                                          QQ +
'function %%IBASE%%(): T%%IBASE%%; inline;' +                                   QQ +
'' +                                                                            QQ +
'begin' +                                                                       QQ +
'  if not Assigned(x%%IBASE%%) then' +                                          QQ +
'    InitialiseLibrary;' +                                                      QQ +
'  result := x%%IBASE%%' +                                                      QQ +
'end { %%IBASE%% } ;' +                                                         QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'(* Create the object and optionally load the shared object library.' +         QQ +
'*)' +                                                                          QQ +
'procedure InitialiseLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'begin' +                                                                       QQ +
'  if not Assigned(x%%IBASE%%) then begin' +                                    QQ +
'    x%%IBASE%% := T%%IBASE%%.Create(moduleName);' +                            QQ +
'    if Assigned(x%%IBASE%%) and earlyLoad then' +                              QQ +
'      x%%IBASE%%.LoadModule            (* So that ModuleInMemory is correct     *)' + QQ +
'  end' +                                                                       QQ +
'end { InitialiseLibrary } ;' +                                                 QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'(* Create the object and optionally load the shared object library.' +         QQ +
'*)' +                                                                          QQ +
'procedure InitializeLibrary(const moduleName: string= DefaultModuleName;' +    QQ +
'                                        earlyLoad: boolean= DefaultEarlyLoad);' + QQ +
'' +                                                                            QQ +
'begin' +                                                                       QQ +
'  InitialiseLibrary(moduleName, earlyLoad)' +                                  QQ +
'end { InitializeLibrary } ;' +                                                 QQ +
'' +                                                                            QQ +
'' +                                                                            QQ +
'initialization' +                                                              QQ +
'//  InitialiseLibrary' +                                                       QQ +
'finalization' +                                                                QQ +
'  FreeAndNil(x%%IBASE%%)' +                                                    QQ +
'end.';


(* Write out part of the boilerplate.
*)
procedure WriteDynamicTop;

var
  scratch: TStringList;
  i: integer;
  buffer: string;

begin
  scratch := TStringList.Create;
  try
    scratch.Text := dynamicTop;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn2(dTxt, buffer)
    end
  finally
    FreeAndNil(scratch)
  end
end { WriteDynamicTop } ;


(* Write a procedure type corresponding to every dynamically-imported entry
  point. This expands CDECL__ and CDECL_VARARGS__ macros.
*)
procedure WriteDynamicTypes;

var
  i, p: integer;
  declaration: string;

begin
  HWriteLn2(dTxt, 'type');
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if ContainsStr(declaration, 'CDECL_') then begin
      declaration := ReplaceStr(declaration, 'CDECL__', 'cdecl;');
      declaration := ReplaceStr(declaration, 'CDECL_VARARGS__', 'cdecl varargs;')
    end;
    if RightStr(declaration, 1) = '_' then begin
      declaration := ChopTrailingMacro(declaration);

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; CDECL__ *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* TPyThreadState_SetAsyncExc= function(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* We know that the declaration has been munged to a single line and that       *)
(* parentheses are properly balanced etc., so can use pattern replacement       *)
(* rather than needing a parser.                                                *)

      if Pos('function ', declaration) = 1 then begin
        Delete(declaration, 1, Length('function'));
        declaration := TrimLeft(declaration);
        p := Pos('(', declaration);
        if p = 0 then
          p := Pos(':', declaration);
        Insert('= function', declaration, p)
      end else begin
        Delete(declaration, 1, Length('procedure'));
        declaration := TrimLeft(declaration);
        p := Pos('(', declaration);
        if p = 0 then
          p := Pos(';', declaration);
        Insert('= procedure', declaration, p)
      end;
      HWriteLn2(dTxt, '  T' + declaration)
    end
  end;
  HWriteLn2(dTxt)
end { WriteDynamicTypes } ;


(* Write the procedure variables embedded in the class definition and include
  the original file as method declarations with CDECL__ and CDECL_VARARGS__
  undefined.
*)
procedure WriteDynamicProcVars;

var
  i: integer;
  declaration, buffer: string;
  scratch: TStringList;

begin
  HWriteLn2(dTxt, 'type');
  HWriteLn2(dTxt, '  T' + iBase + '= class(TDynamicModule)');
  HWriteLn2(dTxt, '  strict private');
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if RightStr(declaration, 1) = '_' then begin
      declaration := ChopTrailingMacro(declarations[i]);

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* FPyThreadState_SetAsyncExc: TPyThreadState_SetAsyncExc;                      *)

      buffer := ExtractRoutineName(declaration);
      HWriteLn2(dTxt, '    F' + buffer + ': T' + buffer + ';')
    end
  end;
  scratch := TStringList.Create;
  try
    scratch.Text := dynamicMid1;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn2(dTxt, buffer)
    end;
  finally
    FreeAndNil(scratch)
  end
end { WriteDynamicProcVars } ;


(* As of FPC v3 it is not possible to write a Pascal shim which invokes a C
  function expecting a variable number of parameters. Expose the procedure
  variables as properties, and expect the user to invoke LoadVarargsRoutine()
  manually.
*)
procedure WriteDynamicProperties;

var
  i: integer;
  declaration, buffer: string;
  scratch: TStringList;

begin
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if not HasVarargs(declaration) then
      continue;
    if RightStr(declaration, 1) = '_' then begin
      declaration := ChopTrailingMacro(declarations[i]);

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyArg_ParseTuple(args: PPyObject; format: PAnsiChar; vaoc: array of const): Integer; cdecl; *)
(*                                                                              *)
(* or                                                                           *)
(*                                                                              *)
(* function PyArg_ParseTuple(args: PPyObject; format: PAnsiChar {;...}): Integer; cdecl varargs; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* property PyArg_ParseTuple: TPyArg_ParseTuple read FPyArg_ParseTuple;         *)

      buffer := ExtractRoutineName(declaration);
      HWriteLn2(dTxt, '    property ' + buffer + ': T' + buffer + ' read F' + buffer + ';')
    end
  end;
  scratch := TStringList.Create;
  try
    scratch.Text := dynamicMid2;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn2(dTxt, buffer)
    end;
    HWriteLn2(dTxt)
  finally
    FreeAndNil(scratch)
  end
end { WriteDynamicProperties } ;


(* This is the first variant of the constructor, which tries to preload all of
  the entry point variables except for those with a variable number of arguments
  which are handled by a separately-generated LoadVarargsRoutine() procedure.
*)
procedure WriteDynamicCreateInit;

var
  i: integer;
  declaration, routine: string;

begin
  HWriteLn2(dTxt, 'constructor T' + iBase + '.Create(const LoadName: string);');
  HWriteLn2(dTxt);
  HWriteLn2(dTxt, 'begin');
  HWriteLn2(dTxt, '  inherited Create(loadName);');
  HWriteLn2(dTxt, '{$ifdef PRELOAD_ROUTINES }');
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if HasVarargs(declaration) then
      continue;
    if RightStr(declaration, 1) = '_' then begin
      declaration := ChopTrailingMacro(declarations[i]);
      routine := ExtractRoutineName(declaration);

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* FPyThreadState_SetAsyncExc := TPyThreadState_SetAsyncExc(LoadRoutine('PyThreadState_SetAsyncExc')); *)

      HWriteLn2(dTxt, '  F' + routine + ' := T' + routine + '(' + 'LoadRoutine(''' + routine + '''));')
    end
  end;
  HWriteLn2(dTxt, '  LoadVarargsRoutine;')
end { WriteDynamicCreateInit } ;


(* This is the second variant of the constructor, which nils all of the entry
  point variables including those with a variable number of arguments.
*)
procedure WriteDynamicCreateNil;

var
  i: integer;
  declaration, routine: string;

begin
  HWriteLn2(dTxt, '{$else                   }');
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if RightStr(declaration, 1) = '_' then begin
      declaration := ChopTrailingMacro(declarations[i]);
      routine := ExtractRoutineName(declaration);

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* FPyThreadState_SetAsyncExc := nil;                                           *)

      HWriteLn2(dTxt, '  F' + routine + ' := nil;')
    end
  end;
  HWriteLn2(dTxt, '{$endif PRELOAD_ROUTINES }');
  HWriteLn2(dTxt, 'end { T' + iBase + '.Create } ;');
  HWriteLn2(dTxt);
  HWriteLn2(dTxt);
  HWriteLn2(dTxt, '(************************************************************************** DCN *)')
end { WriteDynamicCreateNil } ;


(* Generate a function which loads either a single or all of the entry point
  variables. If the first parameter is blank indicating that all should be
  loaded and one of them fails, then if the second parameter is false an
  exception is raised, otherwise the operation is continued.
*)
procedure WriteDynamicLoadVarargsRoutine;

var
  declaration: string;
  lastLabel: string= '';
  routines: TStringList;
  i: integer;

begin
  HWriteLn2(dTxt);
  HWriteLn2(dTxt);
  HWriteLn2(dTxt, 'procedure T' + iBase + '.LoadVarargsRoutine(loadName: string= ''''; keepGoing: boolean= false);');
  HWriteLn2(dTxt);
  routines := TStringList.Create;
  try
    for i := 0 to declarations.Count - 1 do begin
      declaration := declarations[i];
      if not HasVarargs(declaration) then
        continue;
      if RightStr(declaration, 1) = '_' then begin
        declaration := ChopTrailingMacro(declarations[i]);
        routines.Append(ExtractRoutineName(declaration))
      end
    end;

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyArg_ParseTuple(args: PPyObject; format: PAnsiChar; vaoc: array of const): Integer; cdecl; *)
(*                                                                              *)
(* or                                                                           *)
(*                                                                              *)
(* function PyArg_ParseTuple(args: PPyObject; format: PAnsiChar {;...}): Integer; cdecl varargs; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* label                                                                        *)
(*   LPyArg_ParseTuple,                                                         *)

    if routines.Count > 0 then begin
      HWriteLn2(dTxt, 'label');
      for i := 0 to routines.Count - 1 do begin
        if i < routines.Count - 1 then
          HWriteLn2(dTxt, '  L' + routines[i] + ',')
        else
          HWriteLn2(dTxt, '  L' + routines[i] + ';')
      end
    end;
    HWriteLn2(dTxt);
    HWriteLn2(dTxt, 'begin');
    HWriteLn2(dTxt, '  loadName := Trim(loadName);');
    HWriteLn2(dTxt, '  if loadName = ''*'' then');
    HWriteLn2(dTxt, '    loadName := '''';');
    if routines.Count > 0 then begin
      HWriteLn2(dTxt, '  case loadName of');

(* Now generate something like                                                  *)
(*                                                                              *)
(* case loadName of                                                             *)
(*   'PyArg_ParseTuple':            begin                                       *)
(*                                    LPyArg_ParseTuple:                        *)
(*                                    try                                       *)
(*                                      FPyArg_ParseTuple := TPyArg_ParseTuple(LoadRoutine('PyArg_ParseTuple')) *)
(*                                    except                                    *)
(*                                      if (loadName <> '') or not keepGoing then *)
(*                                        Raise                                 *)
(*                                    end                                       *)
(*                                  end;                                        *)
(*   'PyArg_ParseTupleAndKeywords': begin                                       *)
(*                                    LPyArg_ParseTupleAndKeywords:             *)
(*                                    try                                       *)
(*                                      FPyArg_ParseTupleAndKeywords := TPyArg_ParseTupleAndKeywords(LoadRoutine('PyArg_ParseTupleAndKeywords')) *)
(*                                    except                                    *)
(*                                      if (loadName <> '') or not keepGoing then *)
(*                                        Raise                                 *)
(*                                    end;                                      *)
(*                                    if loadName = '' then                     *)
(*                                      goto LPyArg_ParseTuple                  *)
(*                                  end                                         *)
(* otherwise                                                                    *)
(*   if loadName = '' then                                                      *)
(*     goto LPyArg_ParseTupleAndKeywords                                        *)
(* end;                                                                         *)
(*                                                                              *)
(* The list is processed in reverse so that if the parameter is blank the entry *)
(* points are loaded in the order given in the original .inc file.              *)

(* I'm not sure whether there's a way of doing this which is both simpler and   *)
(* safe. The addresses of field variables can't be resolved at build time since *)
(* they're components of an object which isn't allocated until runtime, so it's *)
(* not possible to have a simple array of name/pointer tuples. In any event,    *)
(* doing it this way results in something which is easy to step through.        *)

      for i := routines.Count - 1 downto 0 do begin
        HWriteLn2(dTxt, '    ''' + routines[i] + ''':');
        HWriteLn2(dTxt, '       begin');
        HWriteLn2(dTxt, '         L' + routines[i] + ':');
        HWriteLn2(dTxt, '         try');
        HWriteLn2(dTxt, '           F' + routines[i] + ' := T' + routines[i] + '(LoadRoutine(''' + routines[i] + '''))');
        HWriteLn2(dTxt, '         except');
        HWriteLn2(dTxt, '           if (loadName <> '''') or not keepGoing then');
        HWriteLn2(dTxt, '             raise');
        HWriteLn2(dTxt, '         end;');
        if lastLabel <> '' then begin
          HWriteLn2(dTxt, '         if loadName = '''' then');
          HWriteLn2(dTxt, '           goto ' + lastLabel)
        end;
        lastLabel := 'L' + routines[i];
        HWriteLn2(dTxt, '       end;');
      end;
      HWriteLn2(dTxt, '  otherwise');
      if lastLabel <> '' then begin
        HWriteLn2(dTxt, '    if loadName = '''' then');
        HWriteLn2(dTxt, '      goto ' + lastLabel)
      end;
      HWriteLn2(dTxt, '  end')
    end
  finally
    routines.Free
  end;
  HWriteLn2(dTxt, 'end { T' + iBase + '.LoadVarargsRoutine } ;');
  HWriteLn2(dTxt);
  HWriteLn2(dTxt);
  HWriteLn2(dTxt, '(************************************************************************** DLV *)')
end { WriteDynamicLoadVarargsRoutine } ;


(* Entry points satisfied by the dynamic library (i.e. where the declaration is
  terminated by a macro name) may be generated completely. The Pascal simulation
  of C-type macros (i.e. where the declaration is not terminated by a macro
  name) will need manual attention. This supresses CDECL__ and CDECL_VARARGS__
  macros.
*)
procedure WriteDynamicSubsAndMacros;

var
  i, j: integer;
  declaration, routine, buffer: string;
  isSimulatedMacro: boolean;

begin
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if HasVarargs(declaration) then
      continue;
    isSimulatedMacro := RightStr(declaration, 1) <> '_';
    declaration := ChopTrailingMacro(declaration);
    routine := ExtractRoutineName(declaration);
    Insert('T' + iBase + '.', declaration, Pos(routine, declaration));
    HWriteLn2(dTxt);
    HWriteLn2(dTxt);
    HWriteLn2(dTxt, declaration);
    HWriteLn2(dTxt);
    HWriteLn2(dTxt, 'begin');
    if isSimulatedMacro then begin
      if Pos('function ', declaration) = 1 then
        HWriteLn2(dTxt, '//  result :=');
      HWriteLn2(dTxt, '{$error Local function/procedure must be implemented manually }')
    end else begin
      HWriteLn2(dTxt, '  LoadRoutine(pointer(F' + routine + '), ''' + routine + ''');');
      if Pos('function ', declaration) = 1 then begin
        buffer := '  result := ';
        Delete(declaration, 1, Length('function')); (* Remember: needs parentheses *)
        if ContainsStr(DelSpace(declaration), routine + ':') then
          declaration := ReplaceStr(declaration, routine, routine + '()')
      end else begin
        buffer := '  ';
        Delete(declaration, 1, Length('procedure'))
      end;
      declaration := TrimLeft(declaration);
      Delete(declaration, 1, Length(iBase) + 2); (* Discard leading class name  *)

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* FPyThreadState_SetAsyncExc(t_id, exc)                                        *)

      declaration := RemoveComments(declaration); (* e.g. {;...} for varargs    *)
      declaration := ReplaceText(declaration, 'var ', ' ');
      declaration := ReplaceText(declaration, 'out ', ' ');
      declaration := ReplaceText(declaration, 'const ', ' ');
      while (declaration <> '') and (declaration[Length(declaration)] <> ')') do
        Delete(declaration, Length(declaration), 1); (* Discard trailing type   *)
      if declaration = '' then          (* Oops, too far: no parameter list     *)
        declaration := routine;
      declaration := Trim(declaration);
      if Pos(':', declaration) <> 0 then begin  (* Has one or more parameters   *)
        j := 1;
        while j <= Length(declaration) do begin
          if declaration[j] = ')' then
            break;
          while declaration[j] <> ':' do (* Move over variable name             *)
            j += 1;
          while not (declaration[j] in [';', ')']) do   (* Delete type name     *)
            Delete(declaration, j, 1);
          if declaration[j] = ';' then begin
            declaration[j] := ',';
            j += 1
          end
        end
      end;
      declaration := DelSpace1(declaration);
      HWriteLn2(dTxt, buffer + 'F' + declaration)
    end;
    HWriteLn2(dTxt, 'end { T' + iBase + '.' + routine + ' } ;')
  end
end { WriteDynamicSubsAndMacros } ;


(* Entry points satisfied by the dynamic library (i.e. where the declaration is
  terminated by a macro name) may be generated completely. The Pascal simulation
  of C-type macros (i.e. where the declaration is not terminated by a macro
  name) will need manual attention. This suppresses CDECL__ and CDECL_VARARGS__
  macros.
*)
procedure WriteDynamicSubs;

var
  i, j: integer;
  declaration, routine, buffer: string;
  isSimulatedMacro: boolean;

begin
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    if HasVarargs(declaration) then
      continue;
    isSimulatedMacro := RightStr(declaration, 1) <> '_';
    declaration := ChopTrailingMacro(declaration);
    routine := ExtractRoutineName(declaration);
    Insert('T' + iBase + '.', declaration, Pos(routine, declaration));
    if isSimulatedMacro then begin
    end else begin
      HWriteLn2(dTxt);
      HWriteLn2(dTxt);
      HWriteLn2(dTxt, declaration);
      HWriteLn2(dTxt);
      HWriteLn2(dTxt, 'begin');
      HWriteLn2(dTxt, '  LoadRoutine(pointer(F' + routine + '), ''' + routine + ''');');
      if Pos('function ', declaration) = 1 then begin
        buffer := '  result := ';
        Delete(declaration, 1, Length('function')); (* Remember: needs parentheses *)
        if ContainsStr(DelSpace(declaration), routine + ':') then
          declaration := ReplaceStr(declaration, routine, routine + '()')
      end else begin
        buffer := '  ';
        Delete(declaration, 1, Length('procedure'))
      end;
      declaration := TrimLeft(declaration);
      Delete(declaration, 1, Length(iBase) + 2); (* Discard leading class name  *)

(* Change something formatted like                                              *)
(*                                                                              *)
(* function PyThreadState_SetAsyncExc(t_id: LongInt; exc: PPyObject): Integer; cdecl; *)
(*                                                                              *)
(* to                                                                           *)
(*                                                                              *)
(* FPyThreadState_SetAsyncExc(t_id, exc)                                        *)

      declaration := RemoveComments(declaration); (* e.g. {;...} for varargs    *)
      declaration := ReplaceText(declaration, 'var ', ' ');
      declaration := ReplaceText(declaration, 'out ', ' ');
      declaration := ReplaceText(declaration, 'const ', ' ');
      while (declaration <> '') and (declaration[Length(declaration)] <> ')') do
        Delete(declaration, Length(declaration), 1); (* Discard trailing type   *)
      if declaration = '' then          (* Oops, too far: no parameter list     *)
        declaration := routine;
      declaration := Trim(declaration);
      if Pos(':', declaration) <> 0 then begin  (* Has one or more parameters   *)
        j := 1;
        while j <= Length(declaration) do begin
          if declaration[j] = ')' then
            break;
          while declaration[j] <> ':' do (* Move over variable name             *)
            j += 1;
          while not (declaration[j] in [';', ')']) do   (* Delete type name     *)
            Delete(declaration, j, 1);
          if declaration[j] = ';' then begin
            declaration[j] := ',';
            j += 1
          end
        end
      end;
      declaration := DelSpace1(declaration);
      HWriteLn2(dTxt, buffer + 'F' + declaration);
      HWriteLn2(dTxt, 'end { T' + iBase + '.' + routine + ' } ;')
    end
  end
end { WriteDynamicSubs } ;


(* This is defined for both static and dynamic units, but in practice the
  implementation is slightly different. Since this is a one-off, it's not worth
  messing around with a procedure parameter etc.
*)
procedure WriteMacroIncludeDynamic(var xTxt: Text; const macroName, macroValue: string);

begin
  HWriteLn2(xTxt);
  HWriteLn2(xTxt);
  HWriteLn2(xTxt, '(* Procedures and functions which are defined locally rather than being part of *)');
  HWriteLn2(xTxt, '(* the external library, and which in C would often be implemented as macros,   *)');
  HWriteLn2(xTxt, '(* are defined in this file to protect them from being overwritten.             *)');
  HWriteLn2(xTxt);
  HWriteLn2(xTxt, '{$define ' + macroName + ':= ' + macroValue + ' }');
  HWriteLn2(xTxt, '{$i ' + iBase + '-macros.inc' + ' }');
  HWriteLn2(xTxt)
end { WriteMacroIncludeDynamic } ;


(* Write out part of the boilerplate.
*)
procedure WriteDynamicBottom;

var
  scratch: TStringList;
  i: integer;
  buffer: string;

begin
  scratch := TStringList.Create;
  try
    scratch.Text := dynamicBottom;
    for i := 0 to scratch.Count - 1 do begin
      buffer := TokenExpansions(scratch[i]);
      HWriteLn2(dTxt, buffer)
    end
  finally
    FreeAndNil(scratch)
  end
end { WriteDynamicBottom } ;


end.

