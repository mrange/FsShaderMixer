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

module Scripting =
  open OpenGLMath

  let noBitmapImages  : Map<BitmapImageID , MixerBitmapImage > = Map.empty

  let basicScene fragmentShaderSource : MixerScene =
    {
      Common          = None
      Defines         = [||]
      BufferA         = None
      BufferB         = None
      BufferC         = None
      BufferD         = None
      Image           =
        {
          FragmentSource  = fragmentShaderSource
          Channel0        = None
          Channel1        = None
          Channel2        = None
          Channel3        = None
        }
    }

  let basicPresenter fragmentShaderSource : MixerPresenter =
    {
      FragmentSource  = fragmentShaderSource
      Defines         = [||]
      Channel0        =
        {
          Filter  = Linear
          Wrap    = Clamp
        }
      Channel1        =
        {
          Filter  = Linear
          Wrap    = Clamp
        }
    }

  let basicImageBufferChannel bitmapImage : BufferChannel =
    {
      Filter  = Linear
      Source  = BitmapImage bitmapImage
      Wrap    = Clamp
    }

  let basicImageBufferChannel' bitmapImage : BufferChannel option =
    basicImageBufferChannel bitmapImage |> Some
  
  let blackSceneID      = SceneID "black"
  let blackScene        = basicScene ShaderSources.fragmentShaderBlack

  let redSceneID        = SceneID "red"
  let redScene          = basicScene ShaderSources.fragmentShaderRed

  let faderPresenterID  = PresenterID "fader"
  let faderPresenter    = basicPresenter ShaderSources.fragmentShaderFaderPresenter

  let defaultPresenters : Map<PresenterID, MixerPresenter> =
    [|
      faderPresenterID, faderPresenter
    |] |> Map.ofArray

  let fadeFromTo f t beats : FaderFactory =
    fun beatTime beat ->
      let s = beatTime beat
      let e = beatTime (beat + beats)
      fun time ->
        mix f t (smoothstep s e time)

  let fadeToStage0 beats : FaderFactory =
    fadeFromTo 1.F 0.F beats

  let fadeToStage1 beats : FaderFactory =
    fadeFromTo 0.F 1.F beats

