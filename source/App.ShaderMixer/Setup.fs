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

namespace App.ShaderMixer

open Lib.ShaderMixer

open Scripting

module Setup =
  let createMixer () : Mixer = 
    let gravitySucksID  = SceneID "gravitySucks"

    let gravitySucks    = basicScene ShaderSources.gravitySucks

    {
      NamedBitmapImages = Map.empty
      NamedPresenters   = defaultPresenters
      NamedScenes       =
        [|
          blackSceneID    , blackScene
          redSceneID      , redScene
          gravitySucksID  , gravitySucks
        |] |> Map.ofArray
      BPM           = 142.F
      LengthInBeats = 576

      InitialPresenter  = faderPresenterID
      InitialStage0     = blackSceneID
      InitialStage1     = gravitySucksID

      Script        =
        [|
          0   , ApplyFader  <| fadeToStage1 4.F
        |]
    }
