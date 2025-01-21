
function CLSASOUND__ snd_strerror(errnum: cint): PChar; cdecl;

begin
  result := nil
{$error Local function/procedure must be implemented manually }
(* This example is adequate but deceptive. ALSA implements snd_strerror(), but  *)
(* this code fragment originally came from an example where the Pcm and Seq     *)
(* subsystems were separate and this was a shim that both implemented passing   *)
(* control to a lower-level class or unit.                                      *)
end { snd_strerror } ;

