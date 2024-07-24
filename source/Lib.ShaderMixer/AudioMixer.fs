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
#nowarn "9"

open FSharp.NativeInterop

open Silk.NET.OpenAL

open OpenGLMath

type AudioChannels =
  | Mono
  | Stereo

type AudioBits =
  | AudioBits8    of byte   array
  | AudioBits16   of int16  array
  | AudioBits16'  of byte   array


type AudioMixer =
  {
    AudioChannels       : AudioChannels
    Frequency           : int
    Looping             : bool
    AudioBits           : AudioBits
  }
  member x.EndTime =
    let channels =
      match x.AudioChannels with
      | Mono  -> 1
      | Stereo-> 2
    let samples = 
      match x.AudioBits with
        | AudioBits8   bits -> bits.Length
        | AudioBits16  bits -> bits.Length
        | AudioBits16' bits -> bits.Length/2
    float32 samples/float32 (channels*x.Frequency)

type OpenALAudioMixer  =
  {
    AudioMixer : AudioMixer
    Device     : nativeptr<Device>
    Context    : nativeptr<Context>
    Buffer     : uint32
    Source     : uint32
    Al         : AL
    Alc        : ALContext
  }


module AudioMixer =
  module internal Internals =
    let checkAL (al : AL) : unit =

      let err = al.GetError ()
      if err <> AudioError.NoError then
        failwithf "OpenAL is in an error state: %A" err

    let assertAL (al : AL) : unit =

#if DEBUG
      let err = al.GetError ()
      assert (err = AudioError.NoError)
#else
      ()
#endif

    let checkALC (alc : ALContext) device : unit =

      let err = alc.GetError device
      if err <> ContextError.NoError then
        failwithf "OpenAL is in an error state: %A" err

    let assertALC (alc : ALContext) device : unit =

#if DEBUG
      let err = alc.GetError device
      assert (err = ContextError.NoError)
#else
      ()
#endif

  open Internals

  let setupOpenALAudioMixer
    (audioMixer : AudioMixer)
    : OpenALAudioMixer =

    let al      = AL.GetApi ()
    let alc     = ALContext.GetApi ()

    checkAL al

    let device  = alc.OpenDevice ""
    checkAL   al
    checkALC  alc device


    let context = alc.CreateContext (device, NativePtr.nullPtr)
    checkAL   al
    checkALC  alc device

    let result  = alc.MakeContextCurrent context
    checkAL   al
    checkALC  alc device

    if not result then
      failwith "Failed to make OpenAL context current"

    let buffer = al.GenBuffer ()
    checkAL   al
    checkALC  alc device

    let source = al.GenSource ()
    checkAL   al
    checkALC  alc device

    let bufferFormat =
      match audioMixer.AudioChannels, audioMixer.AudioBits with
      | Mono    , AudioBits8   _ -> BufferFormat.Mono8
      | Mono    , AudioBits16  _ -> BufferFormat.Mono16
      | Mono    , AudioBits16' _ -> BufferFormat.Mono16
      | Stereo  , AudioBits8   _ -> BufferFormat.Stereo8
      | Stereo  , AudioBits16  _ -> BufferFormat.Stereo16
      | Stereo  , AudioBits16' _ -> BufferFormat.Stereo16


    match audioMixer.AudioBits with
    | AudioBits8  bits
    | AudioBits16' bits ->
      use ptr = fixed bits
      al.BufferData (buffer, bufferFormat, NativePtr.toVoidPtr ptr, bits.Length, audioMixer.Frequency)
      checkAL   al
      checkALC  alc device
    | AudioBits16 bits ->
      use ptr = fixed bits
      al.BufferData (buffer, bufferFormat, NativePtr.toVoidPtr ptr, 2*bits.Length, audioMixer.Frequency)
      checkAL   al
      checkALC  alc device

    al.SetSourceProperty (source, SourceInteger.Buffer, buffer)
    checkAL   al
    checkALC  alc device

    al.SetSourceProperty (source, SourceBoolean.Looping, audioMixer.Looping)
    checkAL   al
    checkALC  alc device

    {
      AudioMixer  = audioMixer
      Device      = device
      Context     = context
      Buffer      = buffer
      Source      = source
      Al          = al
      Alc         = alc
    }

  let tearDownOpenALAudioMixer
    (audioMixer : OpenALAudioMixer)
    : unit =

    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    assertAL   al
    assertALC  alc audioMixer.Device

    al.DeleteSource     audioMixer.Source
    assertAL   al
    assertALC  alc audioMixer.Device

    al.DeleteBuffer     audioMixer.Buffer
    assertAL   al
    assertALC  alc audioMixer.Device

    alc.DestroyContext  audioMixer.Context
    assertAL   al
    assertALC  alc audioMixer.Device

    let result = alc.CloseDevice     audioMixer.Device
    assertAL   al

    assert result

    alc.Dispose ()
    al.Dispose ()

  let playAudio
    (audioMixer : OpenALAudioMixer)
    : unit =
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    al.SourcePlay audioMixer.Source
    checkAL   al
    checkALC  alc audioMixer.Device

  let stopAudio
    (audioMixer : OpenALAudioMixer)
    : unit =
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    al.SourceStop audioMixer.Source
    checkAL   al
    checkALC  alc audioMixer.Device

  let pauseAudio
    (audioMixer : OpenALAudioMixer)
    : unit =
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    al.SourcePause audioMixer.Source
    checkAL   al
    checkALC  alc audioMixer.Device

  let getAudioPositionInSec
    (audioMixer : OpenALAudioMixer)
    : float32 =
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    let mutable pos = 0.F
    audioMixer.Al.GetSourceProperty (audioMixer.Source, SourceFloat.SecOffset, &pos)
    checkAL   al
    checkALC  alc audioMixer.Device
    pos

  let setAudioPositionInSec
    (audioMixer : OpenALAudioMixer)
    (pos        : float32         )
    : unit  =
    let pos = clamp pos 0.F audioMixer.AudioMixer.EndTime
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    audioMixer.Al.SetSourceProperty (audioMixer.Source, SourceFloat.SecOffset, pos)
    checkAL   al
    checkALC  alc audioMixer.Device


  let setAudioPitch
    (audioMixer : OpenALAudioMixer)
    (pitch      : float32         )
    : unit  =
    let pitch = clamp pitch 0.F 4.F
    let al  = audioMixer.Al
    let alc = audioMixer.Alc

    checkAL   al
    checkALC  alc audioMixer.Device

    audioMixer.Al.SetSourceProperty (audioMixer.Source, SourceFloat.Pitch, pitch)
    checkAL   al
    checkALC  alc audioMixer.Device


