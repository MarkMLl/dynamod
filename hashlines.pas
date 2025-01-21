(* Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FPC 2.1.0+3.2.3 on Linux Lazarus+FP *)

unit hashLines;

(* This is a support unit for DefToSDUnits. It handles deciding what hash       *)
(* algorithms are supported by FPC's FCL, determining the line-break style of   *)
(* the input file, keeping a running total of the MACs of the input and output  *)
(* files, and outputting a final fingerprint line (outside the scope of MAC     *)
(* generation) which presents the MAC codes of the input and output files in an *)
(* attempt to deter ill-advised manual editing in defiance of the warnings in   *)
(* the generated files.                                         MarkMLl         *)

(* Test this using something like                                               *)
(*                                                                              *)
(* $ md5sum testDefs.inc                                                        *)
(* a3e1191a51d245c1cd89577340e4de9b  testDefs.inc                               *)
(*                                                                              *)
(* $ head --lines=-1 testDefs.static | md5sum                                   *)
(* f6bf9587e172924e21b665c54c16e1f2  -                                          *)
(*                                                                              *)
(* $ tail -1 testDefs.static                                                    *)
(* // MD5 a3e1191a51d245c1cd89577340e4de9b f6bf9587e172924e21b665c54c16e1f2 ANSI LF {} *)
(*                                                                              *)
(* $ head --lines=-1 testDefs.dynamic | md5sum                                  *)
(* 8af1a2123c8819e05a6fef1a272ebc6c  -                                          *)
(*                                                                              *)
(* $ tail -1 testDefs.dynamic                                                   *)
(* // MD5 a3e1191a51d245c1cd89577340e4de9b 8af1a2123c8819e05a6fef1a272ebc6c ANSI LF {} *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
{$if FPC_FULLVERSION >= 030000 }        (* This test requires FPC >= 2.2.4      *)
                                , {%H-}HMAC
{$endif }
  ;

(* Read up to 1024 bytes from the input file, and if there is an acceptable
  line break set the global DefaultTextLineBreakStyle to match it. This will be
  used not only to decide what will be written to the output files, but- more
  importantly- what must be appended to each input and output line on-the-fly
  when they are being hashed for MAC generation.

   If this fails nothing will be changed, i.e. DefaultTextLineBreakStyle will
  depend entirely on the target platform.
*)
procedure InitLineBreakStyle(iFileName: string);

(* Read a line of text, and as a side-effect accumulate its hash.
*)
procedure HReadLn0(var xTxt: text; out line: string);

(* Write a line of text, and as a side-effect accumulate its hash. Assume that
  there's a small number of similar routines, identified by their final digit,
  to avoid the overhead of associative lookup of the first parameter.
*)
procedure HWriteLn1(var xTxt: text; line: string= '');

(* Write a line of text, and as a side-effect accumulate its hash. Assume that
  there's a small number of similar routines, identified by their final digit,
  to avoid the overhead of associative lookup of the first parameter.
*)
procedure HWriteLn2(var xTxt: text; line: string= '');

(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile0(var xTxt: text);

(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile1(var xTxt: text);

(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile2(var xTxt: text);


implementation

(* For the purpose of branding the generated file with the best hash available, *)
(* we need to go directly to the MD5, SHA1 etc. units; however we can use       *)
(* declarations in the HMAC unit (if available) to work out what's present in   *)
(* any specific version of FPC without having to hardcode version numbers or    *)
(* trying to use  $if fileexists()  introduced in FPC 3.4.                      *)

uses
{$if FPC_FULLVERSION < 030000 }         (* This test requires FPC >= 2.2.4      *)
                                MD5
{$else }
  {$if declared(HMACMD5Digest) }        (* Declared in HMAC                     *)
                                MD5
  {$endif }
  {$if declared(HMACSHA1Digest) }       (* Declared in HMAC                     *)
                                , {%H-}SHA1
  {$endif }
{$endif }
  ;

{$if FPC_FULLVERSION < 030000 }         (* This test requires FPC >= 2.2.4      *)
  {$declare HASH_MD5 }
{$else }
  {$if declared(HMACSHA1Digest) }
    {$define HASH_SHA1 }
  {$else }
    {$define HASH_MD5 }
  {$endif }
{$endif }

(********************************************************************************)
(*                                                                              *)
(* FPC's SHA1 implementation is broken on i386 at v3.2.x and possibly other     *)
(* versions. Since we're not aiming for banking-grade non-repudiation etc.      *)
(* tolerate MD5, I consider the risk of ridicule preferable to either having a  *)
(* fingerprint line at variance with unix's sha1sum, using a different          *)
(* fingerprint algorithm depending on the platform or- because the output files *)
(* are supposed to be version-agnostic- on the FPC version.                     *)
(*                                                                              *)
(********************************************************************************)

{$define HASH_MD5 }     (* NOTE COMMENT ABOVE AND DON'T BLAME ME!!!!!  MarkMLl  *)
{$undef HASH_SHA1 }     (* NOTE COMMENT ABOVE AND DON'T BLAME ME!!!!!  MarkMLl  *)

{$macro on }
{$ifdef HASH_MD5 }
  {$define HASH_NAME:= 'MD5' }
  {$define HASH_TCONTEXT:= TMD5Context }
  {$define HASH_TDIGEST:= TMD5Digest }
  {$define HASH_INIT:= MD5Init }
  {$define HASH_UPDATE:= MD5Update }
  {$define HASH_FINAL:= MD5Final }
  {$define HASH_PRINT:= MDPrint }
{$endif HASH_MD5 }
{$ifdef HASH_SHA1 }
  {$define HASH_NAME:= 'SHA-1' }
  {$define HASH_TCONTEXT:= TSHA1Context }
  {$define HASH_TDIGEST:= TSHA1Digest }
  {$define HASH_INIT:= SHA1Init }
  {$define HASH_UPDATE:= SHA1Update }
  {$define HASH_FINAL:= SHA1Final }
  {$define HASH_PRINT:= SHA1Print }
{$endif HASH_SHA1 }

var
  lineBreak: string= '';


(* Read up to 1024 bytes from the input file, and if there is an acceptable
  line break set the global DefaultTextLineBreakStyle to match it. This will be
  used not only to decide what will be written to the output files, but- more
  importantly- what must be appended to each input and output line on-the-fly
  when they are being hashed for MAC generation.

   If this fails nothing will be changed, i.e. DefaultTextLineBreakStyle will
  depend entirely on the target platform.
*)
procedure InitLineBreakStyle(iFileName: string);

type
  TiBlock= array[0..1023] of byte;
  TiFile= file of byte;

var
  iblock: TiBlock;
  iFile: TiFile;
  i: integer;

begin
  AssignFile(iFile, iFileName);
{$push }{$I- }
  Reset(iFile);
  if IOResult <> 0 then
    exit;
  try
    FillByte(iBlock{%H-}, SizeOf(iBlock), 0);
    BlockRead(iFile, iBlock, SizeOf(iBlock));
    if IOResult <> 0 then
      exit;                             (* Via finally block                    *)
    for i := 0 to SizeOf(iBlock) - 2 do
      case iBlock[i] of
        $00: exit;                      (* Via finally block                    *)
        $0a: begin
               DefaultTextLineBreakStyle := tlbsLF;
               lineBreak := #$0a;
               break
             end;
        $0d: case iBlock[i + 1] of
               $00: exit;               (* Via finally block                    *)
               $0a: begin
                      DefaultTextLineBreakStyle := tlbsCRLF;
                      lineBreak := #$0d#$0a;
                      break
                    end
             otherwise
               DefaultTextLineBreakStyle := tlbsCR;
               lineBreak := #$0d;
               break
             end
      otherwise
      end
  finally
    CloseFile(iFile)
  end
{$pop }
end { InitLineBreakStyle } ;


var
  context: array[0..2] of HASH_TCONTEXT;
  digest: array[0..2] of HASH_TDIGEST;


(* Append an output line starting with // as a widely-understood single-line
  comment marker, and containing the name of the hash being used, hashes for
  the input and output (i.e. this) file, the filesystem or system codepage (in
  support of embedded include directives), the type of line break being used,
  and a closing {} (Pascal-specific multi-line comment marker).
*)
procedure writeHashLine(var xTxt: text; digestIndex: integer);

var
  codepageName: string;

begin
  Write(xTxt, '// ');
  Write(xTxt, HASH_NAME, ' ', HASH_PRINT(digest[0]), ' ', HASH_PRINT(digest[digestIndex]), ' ');
{$if declared(DefaultFileSystemCodePage) }
  codepageName := CodePageToCodePageName(DefaultFileSystemCodePage);
{$else }
  codepageName := CodePageToCodePageName(DefaultSystemCodePage);
{$endif }
  if Trim(codepageName) = '' then
    Write(xTxt, 'ANSI ')
  else
    Write(xTxt, codepageName, ' ');
  case DefaultTextLineBreakStyle of
    tlbsLF:   Write(xTxt, 'LF');
    tlbsCRLF: Write(xTxt, 'CRLF');
    tlbsCR:   Write(xTxt, 'CR')
  otherwise
    Write(xTxt, '--')
  end;
  WriteLn(xTxt, ' {}')
end { writeHashLine } ;


(* Read a line of text, and as a side-effect accumulate its hash.
*)
procedure HReadLn0(var xTxt: text; out line: string);

var
  scratch: string;

begin
  ReadLn(xTxt, line);
  scratch := line + lineBreak;
  HASH_UPDATE(context[0], scratch[1], Length(scratch))
end { HReadLn0 } ;


(* Write a line of text, and as a side-effect accumulate its hash. Assume that
  there's a small number of similar routines, identified by their final digit,
  to avoid the overhead of associative lookup of the first parameter.
*)
procedure HWriteLn1(var xTxt: text; line: string= '');


begin
  WriteLn(xTxt, line);
  line := line + lineBreak;
  HASH_UPDATE(context[1], line[1], Length(line))
end { HWriteLn1 } ;


(* Write a line of text, and as a side-effect accumulate its hash. Assume that
  there's a small number of similar routines, identified by their final digit,
  to avoid the overhead of associative lookup of the first parameter.
*)
procedure HWriteLn2(var xTxt: text; line: string= '');

begin
  WriteLn(xTxt, line);
  line := line + lineBreak;
  HASH_UPDATE(context[2], line[1], Length(line))
end { HWriteLn2 } ;


(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile0(var xTxt: text);

begin
  CloseFile(xTxt);
  HASH_FINAL(context[0], digest[0]);
  HASH_FINAL(context[1], digest[1]);
  HASH_FINAL(context[2], digest[2])
end { HCloseFile0 } ;


(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile1(var xTxt: text);

begin
  writeHashLine(xTxt, 1);
  CloseFile(xTxt)
end { HCloseFile1 } ;


(* Close a file, appending the relevant hashes etc. Assume that there's a small
  number of similar routines, identified by their final digit, to avoid the
  overhead of associative lookup of the first parameter.

   Call HCloseFile0() before either HCloseFile1() or HCloseFile2().
*)
procedure HCloseFile2(var xTxt: text);

begin
  writeHashLine(xTxt, 2);
  CloseFile(xTxt)
end { HCloseFile1 } ;


begin
  HASH_INIT(context[0]);
  HASH_INIT(context[1]);
  HASH_INIT(context[2])
end.

