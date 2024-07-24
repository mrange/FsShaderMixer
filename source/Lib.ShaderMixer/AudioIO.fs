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

module AudioIO =
  open NAudio.Wave

  let loadFromWavFile (fileName : string) : AudioMixer =
    use waveFileReader  = new WaveFileReader (fileName)

    let audioChannels = 
      match waveFileReader.WaveFormat.Channels with
      | 1 -> Mono
      | 2 -> Stereo
      | n -> failwithf "WaveFormat.Channels expected to be either 1 or 2 but is %d" n

    let bytes = Array.zeroCreate (int waveFileReader.Length)
    ignore <| waveFileReader.Read (bytes, 0, bytes.Length)

    {
      AudioChannels       = audioChannels
      Frequency           = waveFileReader.WaveFormat.SampleRate
      Looping             = false
      AudioBits           = AudioBits16' bytes
    }
