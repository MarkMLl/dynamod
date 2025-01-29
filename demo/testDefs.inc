{
#include <unixtype>
}

{$ifdef CONSTS }
{$endif CONSTS }

{$ifdef TYPES  }

type
  { Signed frames quantity }
  snd_pcm_sframes_t = cint;

  { PCM handle }
  PPsnd_pcm_t = ^Psnd_pcm_t;
  Psnd_pcm_t  = Pointer;

  { PCM stream (direction) }
  snd_pcm_stream_t = cint;

  { PCM sample format }
  snd_pcm_format_t = cint;

  { PCM access type }
  snd_pcm_access_t = cint;

  { Unsigned frames quantity }
  snd_pcm_uframes_t = cuint;

{$endif TYPES  }

{$ifdef PROCS  }

(* Opens a PCM.

Parameters
    pcm         Returned PCM handle
    name        ASCII identifier of the PCM handle
    stream      Wanted stream
    mode        Open mode (see SND_PCM_NONBLOCK, SND_PCM_ASYNC)

Returns
    0 on success otherwise a negative error code
*)
function snd_pcm_open(pcm: PPsnd_pcm_t; Name: PChar; stream: snd_pcm_stream_t; mode: cint): cint; cdecl; LIBASOUND__

(* Set the hardware and software parameters in a simple way.

Parameters
    pcm	PCM handle
    format	required PCM format
    access	required PCM access
    channels	required PCM channels
    rate	required sample rate in Hz
    soft_resample	0 = disallow alsa-lib resample stream, 1 = allow resampling
    latency	required overall latency in us

Returns
    0 on success otherwise a negative error code
*)
function snd_pcm_set_params(pcm: Psnd_pcm_t; format: snd_pcm_format_t;
                                access: snd_pcm_access_t; channels, rate: cuint;
                                soft_resample: cint; latency: cuint): cint; cdecl; LIBASOUND__

{ Explicit entry point names or indexes are not supported, but are tolerated if
  identical to the function/procedure name. This is a reasonable restriction
  since this file might be used to create a static interface which does not have
  any sort of alias capability.
}

(* close PCM handle

Parameters
    pcm PCM handle

Returns
    0 on success otherwise a negative error code

Closes the specified PCM handle and frees all associated resources.
*)
function snd_pcm_close(pcm: Psnd_pcm_t): cint; cdecl; LIBASOUND__

{$endif PROCS  }

{$ifdef VPROCS }

// Special test for a variable number of parameters, which mandates that the
// shim interface uses the Pascal calling convention.

procedure printf(fmt: pchar); CDECL_VARARGS__ LIBASOUND__
procedure debugf(fmt: pchar; vaoc: array of const); CDECL__ LIBASOUND__

{$endif VPROCS  }

{$endif PROCS  }

(********************************************************************************)
(*                                                                              *)
(* Convenience function(s) to satisfy the most common reasons a caller would    *)
(* have to go directly to Asound.                                               *)
(*                                                                              *)
(********************************************************************************)

function snd_strerror(errnum: cint): PChar; cdecl;

{$endif PROCS  }

