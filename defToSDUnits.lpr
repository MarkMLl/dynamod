(* Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FP *)

program defToSDUnits;

(* This program converts the definitions of a library interface in a .inc file  *)
(* to units for static and dynamic linkage, see testDefs.inc for examples.      *)
(*                                                                              *)
(* It is assumed that use of this program complies with both the spirit and the *)
(* letter of whatever license applies- by explicit statement or implication- to *)
(* the library definition being processed, this is particularly important where *)
(* the definition includes both a factual statement of function parameters etc. *)
(* and a plain-language description which is arguably a "literary work" in its  *)
(* own right. Apart from that this may only be used, modified and redistributed *)
(* in compliance with Version 2 of the GPL or any later version.    MarkMLl     *)

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, hashlines, defToSDStatic, defToSDDynamic,
  defToSDCommon, DynamicModule;

(* Above: the DynaMod unit isn't explicitly used in this program, but importing *)
(* it is useful in that it checks that it is available and compiles cleanly on  *)
(* the current platform.                                                        *)


(* Read the input file, saving declarations and import hints and discarding the
  remainder.
*)
procedure readInput;

var
  current: string= '';
  scratch: string;

type
  balanced= (no, yes, error);


  function parensBalanced(const s: string): balanced;

  var
    i: integer;
    left: integer= 0;
    right: integer= 0;

  begin
    for i := 1 to Length(s) do
      case s[i] of
        '(': left += 1;
        ')': right += 1
      otherwise
      end;
    if right > left then
      result := error
    else
      if right = left then
        result := yes
      else
        result := no
  end { parensBalanced } ;


begin
  while not eof(iTxt) do begin          (* Read every line of file              *)
    current := '';
    while not eof(iTxt) do begin        (* Read every line of declaration       *)
      HReadLn0(iTxt, scratch);
      scratch := Trim(scratch);
      if scratch = '' then
        break;

(* Recognise C-style inclusions in the form                                     *)
(*                                                                              *)
(*      {                                                                       *)
(*      #include<UnixType>                                                      *)
(*      }                                                                       *)
(*                                                                              *)
(* where the name should in general correspond to a known FPC unit. There are a *)
(* couple of exceptional cases here, e.g. "stdint.h" is converted to "UnixType" *)
(* and "time.h" automatically imports "UnixType" since there is no equivalent   *)
(* FPC Time unit handling the unix API.                                         *)
(*                                                                              *)
(* The importations string replaces the %%IMPORTS%% token, and will be inserted *)
(* after the usual mandatory imports; hence it requires a leading comma if non- *)
(* blank.                                                                       *)

      if (Pos('#include<', DelSpace(scratch)) = 1) and (Pos('>', scratch) > 1) then begin
        SetLength(scratch, Pos('>', scratch) - 1);
        scratch := DelSpace(scratch);
        Delete(scratch, 1, Length('#include<')); (* Expect single name, not list *)
        case scratch of
          'stdint.h',           (* Defines uint32 etc., while cuint32 etc. are  *)
          'time.h': importations += ', UnixType'        (* Pascal-specific.     *)
        otherwise
          importations += ', ' + scratch
        end
      end;

(* Everything else is either handled as a declaration or discarded.             *)

      scratch := ReplaceText(scratch, 'function ', 'function '); (* Case-insensitive *)
      scratch := ReplaceText(scratch, 'procedure ', 'procedure ');
      if current = '' then
        current := scratch              (* First line of declaration            *)
      else
        current += ' ' + scratch;       (* Single space when concatenating      *)
      if not ((Pos('function ', current) = 1) or (Pos('procedure ', current) = 1)) then
        break;
      case parensBalanced(current) of
        no:   ;
        yes:  case RightStr(current, 1) of
                '_': begin
                       declarations.Append(current);
                       while (current <> '') and ContainsStr(current, ' ') do
                         Delete(current, 1, 1);
                       if current <> '' then
                         macroName := current;
                       break            (* Completed this declaration           *)
                     end;
                ';': begin
                       declarations.Append(current);
                       break            (* Completed this declaration           *)
                     end
              otherwise
                continue                (* Read next line of declaration        *)
              end
      otherwise
        break                           (* Abandon this declaration             *)
      end
    end
  end
end { readInput } ;


(* Look for cases where a declaration doesn't quite end with a macro, and the
  extra text is an entry point name or index. Treat these as error conditions
  except that where the name is identical to the declared procedure or function
  it is deleted silently.

  This is one of very few places where error messages are emitted, since this is
  a semantic/contextual problem rather than one which will be detected by the
  compiler as a syntactic error.
*)
function discardExplicitNames(): boolean;

const
  failOnError= true;

var
  i, p: integer;
  declaration, scratch: string;

begin
  result := true;
  for i := 0 to declarations.Count - 1 do begin
    declaration := declarations[i];
    declaration := LowerCase(Trim(DelSpace1(declaration)));
    if (declaration = '') or (RightStr(declaration, 1) <> ';') then
      continue;

(* If the prepenultimate word does not end in _ or _; assume it's not a macro.  *)

    scratch := ExtractWord(WordCount(declaration, [' ']) - 2, declaration, [' ']);
    if (RightStr(scratch, 1) <> '_') and (RightStr(scratch, 2) <> '_;') then
      continue;

(* If the penultimate word is index we can't handle it.                         *)

    scratch := ExtractWord(WordCount(declaration, [' ']) - 1, declaration, [' ']);
    if scratch = 'index' then begin
      if failOnError then begin
        WriteLn(ErrOutput, 'External entrypoint indexes are not supported, see declaration');
        WriteLn(ErrOutput);
        WriteLn(ErrOutput, declarations[i]);
        WriteLn(ErrOutput);
        exit                            (* Return false...                      *)
      end;
      continue                          (* ...or just ignore it.                *)
    end;

(* If the penultimate word is not name we aren't interested.                    *)

    scratch := ExtractWord(WordCount(declaration, [' ']) - 1, declaration, [' ']);
    if scratch <> 'name' then
      continue;

(* If the ultimate word is not a quoted string followed by a semicolon we can't *)
(* handle it.                                                                   *)

    scratch := ExtractWord(WordCount(declaration, [' ']), declaration, [' ']);
    if (scratch = '') or (scratch[1] <> '''') or (RightStr(scratch, 2) <> ''';') then begin
      if failOnError then begin
        WriteLn(ErrOutput, 'External entrypoint name unquoted, see declaration');
        WriteLn(ErrOutput);
        WriteLn(ErrOutput, declarations[i]);
        WriteLn(ErrOutput);
        exit                            (* Return false...                      *)
      end;
      continue                          (* ...or just ignore it.                *)
    end;
    if RightStr(scratch, 2) = ''';' then
      Delete(scratch, Length(scratch) - 1, 2)
    else
      Delete(scratch, Length(scratch), 1);
    Delete(scratch, 1, 1);

(* If the explicit entrypoint name does not match the function/procedure name   *)
(* we can't handle it.                                                          *)

    if scratch <> ExtractRoutineName(declaration) then begin
      if failOnError then begin
        WriteLn(ErrOutput, 'External entrypoint mismatch, see declaration');
        WriteLn(ErrOutput);
        WriteLn(ErrOutput, declarations[i]);
        WriteLn(ErrOutput);
        exit                            (* Return false...                      *)
      end;
      continue                          (* ...or just ignore it.                *)
    end;

(* We know that we have an explicit entrypoint name and that it matches the     *)
(* function/procedure name. Discard it, plus any spurious trailing semi-colon.  *)

    declaration := declarations[i];
    p := WordPosition(WordCount(declaration, [' ']) - 1, declaration, [' ']);
    SetLength(declaration, p - 1);
    while RightStr(declaration, 1)[1] in [' ', ';'] do
      Delete(declaration, Length(declaration), 1);
    declarations[i] := declaration
  end;
  result := true
end { discardExplicitNames } ;


label
  665, 666;

begin
  case ParamCount() of
    0: begin
         665:
         WriteLn(ErrOutput, 'Expand library definitions to .static and .dynamic units. Command line:');
         WriteLn(ErrOutput);
         WriteLn(ErrOutput, '        defToSDUnits input_file [library]');
         WriteLn(ErrOutput);
         WriteLn(ErrOutput, 'The input file name is mandatory, standard input will not be used if it is');
         WriteLn(ErrOutput, 'omitted; the library name should not have a path or extension. The output file');
         WriteLn(ErrOutput, 'names are generated automatically.');
         WriteLn(ErrOutput);
         Halt(9)
       end;
    1: 666: if ParamStr(1) = '--help' then
              goto 665
            else
              iFull := ParamStr(1)      (* Input file name                      *)
  otherwise
    lName := ParamStr(2);               (* Optional library name                *)
    goto 666
  end;
  if lName = '' then                    (* Fallback to e.g. PythonDefs.inc      *)
    lName := LowerCase(ChangeFileExt(ExtractFileName(iFull), ''));
  nName := lName;                       (* For example libpython2.7             *)
  if Pos('lib', nName) = 1 then
    Delete(nName, 1, 3);                (* Becomes something like python2.7     *)
  lDesc := lName;                       (* For example libpython2.7             *)
  if Pos('lib', lDesc) = 1 then
    Delete(ldesc, 1, 3);
  lDesc[1] := UpCase(lDesc[1]);
  while (lDesc <> '') and (lDesc[Length(lDesc)] in ['.', '0'..'9']) do
    SetLength(lDesc, Length(lDesc) - 1); (* Becomes something like Python       *)
  iName := ExtractFileName(iFull);      (* Lose path                            *)
  iBase := ChangeFileExt(iName, '');    (* Lose extension                       *)
  if RightStr(iBase, 4) = 'Defs' then
    SetLength(iBase, Length(iBase) - 4);

(* iFull  Filename as specified on the command line.                            *)
(* iName  Filename without path.                                                *)
(* iBase  Filename without path or extension, some suffixes stripped.           *)
(* lName  Library name as specified on the command line, or guessed.            *)
(* lDesc  Library name "prettified".                                            *)

  WriteLn('Input file: ', iFull);
  InitLineBreakStyle(iFull);
  AssignFile(iTxt, iFull);
  Reset(iTxt);
  declarations := TStringList.Create;
  importations := '';
  try

(* Traditional unix doctrine has it that a backup should be a rename of the     *)
(* original file, so that it retains the original ownership and access rights.  *)
(* While ~ backups are overwritten every time, any .OLD backups are preserved   *)
(* in case they've been edited manually (but see mention below of the potential *)
(* macro).                                                                      *)

    if not FileExists(ChangeFileExt(iFull, '.static') + '.OLD') then
      if FileExists(ChangeFileExt(iFull, '.static~')) then
        RenameFile(ChangeFileExt(iFull, '.static~'), ChangeFileExt(iFull, '.static.OLD'));
    if not FileExists(ChangeFileExt(iFull, '.dynamic') + '.OLD') then
      if FileExists(ChangeFileExt(iFull, '.dynamic~')) then
        RenameFile(ChangeFileExt(iFull, '.dynamic~'), ChangeFileExt(iFull, '.dynamic.OLD'));
    if FileExists(ChangeFileExt(iFull, '.static')) then
      RenameFile(ChangeFileExt(iFull, '.static'), ChangeFileExt(iFull, '.static~'));
    WriteLn('Static output file: ', ChangeFileExt(iFull, '.static'));
    AssignFile(sTxt, ChangeFileExt(iFull, '.static'));
    Rewrite(sTxt);
    if FileExists(ChangeFileExt(iFull, '.dynamic')) then
      RenameFile(ChangeFileExt(iFull, '.dynamic'), ChangeFileExt(iFull, '.dynamic~'));
    WriteLn('Dynamic output file: ', ChangeFileExt(iFull, '.dynamic'));
    AssignFile(dTxt, ChangeFileExt(iFull, '.dynamic'));
    Rewrite(dTxt);
    readInput;

(* There is no support in this program for explicit external entry point names  *)
(* or indexes. In general it would be pointless to have these since they are    *)
(* only applicable to dynamic (load-time or on-demand) linkage, but tolerate    *)
(* and delete an explicit name which is identical to the declared procedure or  *)
(* function name.                                                               *)

    if not discardExplicitNames() then  (* Generates error message on error     *)
      Halt(8);

(* Each of these represents a pass over the saved declarations. If there is a   *)
(* prexisiting file for local macro routines then include it, otherwise output  *)
(* references to library routines and local macro routines together in the      *)
(* order of declaration.                                                        *)

    WriteStaticTop;
    WriteLn('Optional macros file: ', iBase + '-macros.inc');
    if FileExists(iBase + '-macros.inc') then
      WriteMacroIncludeStatic(sTxt, 'CLS' + UpperCase(lDesc) + '__', '') (* No class prefix for macro routines *)
    else
      WriteStaticMacros;
    WriteStaticBottom;                  (* Must finish line cleanly or hash     *)
    WriteDynamicTop;                    (* line will be broken.                 *)
    WriteDynamicTypes;
    WriteDynamicProcVars;
    WriteDynamicProperties;
    WriteDynamicCreateInit;
    WriteDynamicCreateNil;
    WriteDynamicLoadVarargsRoutine;
    if FileExists(iBase + '-macros.inc') then begin
      WriteDynamicSubs;
      WriteLn('Macro classname tag: ', 'CLS' + UpperCase(lDesc) + '__');
      WriteLn('Macro classname value: ', 'T' + iBase);
      WriteMacroIncludeDynamic(dTxt, 'CLS' + UpperCase(lDesc) + '__', 'T' + iBase + '.') (* Class prefix for macro routines *)
    end else
      WriteDynamicSubsAndMacros;
    WriteDynamicBottom                  (* Must finish line cleanly or hash     *)
  finally                               (* line will be broken.                 *)
    FreeAndNil(declarations);
    HCloseFile0(iTxt);
{$I-}                                   (* Errors are probably from startup     *)
    HCloseFile1(sTxt);
    HCloseFile2(dTxt)
  end
end.

