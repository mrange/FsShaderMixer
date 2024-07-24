(*
FsShaderMixer - Mixes ShaderToy like shaders
Copyright (C) 2024  Mårten Rånge

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses
*)

namespace Lib.ShaderMixer

open System
open System.Diagnostics

type PlayState =
  | IsPaused                = 0
  | IsPlaying               = 1
  | IsSeekingWhilePaused    = 2
  | IsSeekingWhilePlaying   = 3

type Playback (openALAudioMixer : OpenALAudioMixer option) =

  let clock = Stopwatch.StartNew ()

  let soundEndTime =
    match openALAudioMixer with
    | None        -> infinityf
    | Some  oalm  -> oalm.AudioMixer.EndTime

  let mutable devicePlaying             = false
  let mutable playState                 = PlayState.IsPaused
  let mutable timeWhileDevicePlaying    = nanf
  let mutable offsetWhileDevicePlaying  = nanf
  let mutable timeWhileDevicePaused     = 0.F
  let mutable pitch                     = 1.F

  let globalTime () : float32 = float32 clock.ElapsedMilliseconds/1000.F
    
  let time () : float32 =
    let tm =
      if devicePlaying then
        timeWhileDevicePlaying + (globalTime () + offsetWhileDevicePlaying)*pitch
      else
        timeWhileDevicePaused

    assert not (Single.IsNaN tm)

    Math.Clamp (tm, 0.F, soundEndTime)

  let playDevice () =
    assert not devicePlaying
    timeWhileDevicePlaying    <- timeWhileDevicePaused
    offsetWhileDevicePlaying  <- - globalTime ()
    match openALAudioMixer with
    | None        -> ()
    | Some  oalm  ->
      AudioMixer.setAudioPositionInSec oalm timeWhileDevicePaused
      AudioMixer.playAudio oalm

    timeWhileDevicePaused <- nanf
    devicePlaying         <- true

  let pauseDevice () =
    assert devicePlaying
    timeWhileDevicePaused   <- time ()
    match openALAudioMixer with
    | None        -> ()
    | Some  oalm  ->
      AudioMixer.pauseAudio oalm

    timeWhileDevicePlaying    <- nanf
    offsetWhileDevicePlaying  <- nanf
    devicePlaying             <- false

  let pause () : unit =
    playState <-
      match playState with
      | PlayState.IsPaused               -> PlayState.IsPaused
      | PlayState.IsPlaying              ->
        pauseDevice ()
        PlayState.IsPaused
      | PlayState.IsSeekingWhilePaused   -> PlayState.IsSeekingWhilePaused
      | PlayState.IsSeekingWhilePlaying  -> PlayState.IsSeekingWhilePaused
      | _                                -> failwithf "Unexpected playstate: %A" playState

  let play () : unit =
    playState <-
      match playState with
      | PlayState.IsPaused               -> 
        playDevice ()
        PlayState.IsPlaying
      | PlayState.IsPlaying              -> PlayState.IsPlaying
      | PlayState.IsSeekingWhilePaused   -> PlayState.IsSeekingWhilePlaying
      | PlayState.IsSeekingWhilePlaying  -> PlayState.IsSeekingWhilePlaying
      | _                                -> failwithf "Unexpected playstate: %A" playState

  let setPitch (p : float32) : unit =
    let time = time ()
    if devicePlaying then
      timeWhileDevicePlaying    <- time
      offsetWhileDevicePlaying  <- - globalTime ()
    else
      ()

    pitch <- p

    match openALAudioMixer with
    | None        -> ()
    | Some  oalm  ->
      AudioMixer.setAudioPositionInSec oalm time
      AudioMixer.setAudioPitch oalm p

  let setTime (t : float32) : bool =
    match playState with
    | PlayState.IsPaused               -> false
    | PlayState.IsPlaying              -> false
    | PlayState.IsSeekingWhilePaused   -> timeWhileDevicePaused <- t; true
    | PlayState.IsSeekingWhilePlaying  -> timeWhileDevicePaused <- t; true
    | _                                -> failwithf "Unexpected playstate: %A" playState

  let startSeeking () : unit =
    playState <-
      match playState with
      | PlayState.IsPaused               -> PlayState.IsSeekingWhilePaused
      | PlayState.IsPlaying              -> 
        pauseDevice ()
        PlayState.IsSeekingWhilePlaying
      | PlayState.IsSeekingWhilePaused   -> PlayState.IsSeekingWhilePaused
      | PlayState.IsSeekingWhilePlaying  -> PlayState.IsSeekingWhilePlaying
      | _                                -> failwithf "Unexpected playstate: %A" playState

  let stopSeeking () : unit =
    playState <-
      match playState with
      | PlayState.IsPaused               -> PlayState.IsPaused
      | PlayState.IsPlaying              -> PlayState.IsPlaying
      | PlayState.IsSeekingWhilePaused   -> PlayState.IsPaused
      | PlayState.IsSeekingWhilePlaying  -> 
        playDevice ()
        PlayState.IsPlaying
      | _                                -> failwithf "Unexpected playstate: %A" playState

  member x.Pause () : unit = pause ()
  member x.Play ()  : unit = play ()

  member x.Pitch () : float32 = pitch

  member x.Time () : float32 = time ()

  member x.SetPitch (p : float32)     : unit = setPitch p
  member x.SetTime  (time : float32)  : bool = setTime time

  member x.StartSeeking ()  : unit = startSeeking ()
  member x.StopSeeking  ()  : unit = stopSeeking ()
