(* Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FP *)

unit defToSDCommon;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  QQ= LineEnding;

var
  iFull, iName, iBase, lName, lDesc, macroName, importations: string;
  iTxt, sTxt, dTxt: text;
  declarations: TStringList;

(* Expand a predefined token.
*)
function TokenExpansions(const s: string): string;

(* Extract a function or procedure name.
*)
function ExtractRoutineName(const declaration: string): string;

(* Chop any trailing macro name, assumed to end with "_" and to be preceded by
  a semicolon.
*)
function ChopTrailingMacro(const declaration: string): string;

(* Return true if the declaration has a variable number of arguments.
*)
function HasVarargs(const declaration: string): boolean;

(* Remove a comment embedded in a declaration. This will typically be something
  like {;...} indicating somebody's attempt to add a helpful note warning of
  trailing varargs.
*)
function RemoveComments(const declaration: string): string;


implementation

uses
  StrUtils;


(* Expand a predefined token.
*)
function TokenExpansions(const s: string): string;

begin
  result := ReplaceStr(s, '%%INAME%%', iName);
  result := ReplaceStr(result, '%%IBASE%%', iBase);
  result := ReplaceStr(result, '%%LNAME%%', lName);
  result := ReplaceStr(result, '%%LDESC%%', ldesc);
  result := ReplaceStr(result, '%%MACRO%%', macroName);
  result := ReplaceStr(result, '%%IMPORTS%%', importations)
end { TokenExpansions } ;


(* Extract a function or procedure name.
*)
function ExtractRoutineName(const declaration: string): string;

begin
  result := ExtractWord(2, declaration, [' ', '(', ',', ':', ';'])
end { ExtractRoutineName } ;


(* Chop any trailing macro name, assumed to end with "_" and to be preceded by
  a semicolon.
*)
function ChopTrailingMacro(const declaration: string): string;

begin
  result := declaration;
  if RightStr(result, 1) = '_' then
    while RightStr(result, 1) <> ';' do
      SetLength(result, Length(result) - 1)
end { VhopTrailingMacro } ;


(* Return true if the declaration has a variable number of arguments.
*)
function HasVarargs(const declaration: string): boolean;

begin
  result := ContainsStr(declaration, 'CDECL_VARARGS__') or
      ContainsText(ReplaceStr(DelSpace1(declaration), ' )', ')'), 'array of const)')
end { HasVarargs } ;


(* Remove a comment embedded in a declaration. This will typically be something
  like {;...} indicating somebody's attempt to add a helpful note warning of
  trailing varargs.
*)
function RemoveComments(const declaration: string): string;

type
  commentType= (none, brace, digraph, closeB, closeD);

var
  i: integer;
  state: commentType= none;


  function token(): commentType; inline;

  begin
    if declaration[i] = '{' then
      exit(brace);
    if Copy(declaration, i, 2) = '(*' then
      exit(digraph);
    if declaration[i] = '}' then
      exit(closeB);
    if Copy(declaration, i, 2) = '*)' then
      exit(closeD);
    result := none
  end { token } ;


begin
  result := '';
  i := 1;
  while i <= Length(declaration) do
    case state of
      none:   case token() of
                brace:   begin
                           state := brace;
                           i += 1
                         end;
                digraph: begin
                           state := digraph;
                           i += 2
                         end
              otherwise
                result += declaration[i];
                i += 1
              end;
      brace:   case token of
                 closeB: begin
                           state := none;
                           i += 1
                         end
               otherwise
                 i += 1
               end;
      digraph: case token of
                 closeD: begin
                           state := none;
                           i += 2
                         end
               otherwise
                 i += 1
               end
    otherwise                           (* Shouldn't happen, don't output and   *)
      i += 1                            (* don't change state.                  *)
    end
end { RemoveComments } ;


end.

