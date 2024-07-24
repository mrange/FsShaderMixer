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
  let createAudioMixer () : AudioMixer =
    AudioIO.loadFromWavFile @"D:\assets\virgill_-_hyperbased_-_omg_its_a_cube_-_amigaremix_02106.wav"

  let createMixer () : Mixer = 
    let gravitySucksID  = SceneID "gravitySucks"
    let gravitySucks    = basicScene ShaderSources.gravitySucks

    let crewID          = BitmapImageID         "crew"
    let crew            = ImageIO.loadFromFile  R8 @"D:\assets\impulse-members-distance.png"

    let imageID         = SceneID "image"
    let image           = basicScene ShaderSources.image
    let image           =
      { image with
          Image =
            { image.Image with
                Channel0 = basicImageBufferChannel' crewID
            }
      }

    let images  = 
      [|
        crewID, crew
      |] |> Map.ofArray
    
    {
      NamedBitmapImages = images
      NamedPresenters   = defaultPresenters
      NamedScenes       =
        [|
          blackSceneID    , blackScene
          redSceneID      , redScene
          gravitySucksID  , gravitySucks
          imageID         , image
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
