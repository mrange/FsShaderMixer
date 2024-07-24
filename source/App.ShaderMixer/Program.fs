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

open ImGuiNET
open Silk.NET
open Silk.NET.Maths
open Silk.NET.OpenGL
open Silk.NET.Input
open Silk.NET.OpenGL.Extensions.ImGui
open Silk.NET.Windowing

open System
open System.Globalization

open FSharp.Core.Printf

type AppState =
  {
    GL                : GL
    Input             : IInputContext
    ImGui             : ImGuiController
    mutable Position  : float32
    mutable Pitch     : float32
  }

module Program =

  let log (cc : ConsoleColor) (prelude : string) (msg : string) : unit =
    let occ = Console.ForegroundColor

    try
      Console.ForegroundColor <- cc
      Console.WriteLine (prelude + msg)
    finally
      Console.ForegroundColor <- occ

  let bad     msg = log ConsoleColor.Red    "BAD " msg
  let warn    msg = log ConsoleColor.Yellow "WARN" msg
  let info    msg = log ConsoleColor.Gray   "INFO" msg
  let good    msg = log ConsoleColor.Green  "GOOD" msg
  let hili    msg = log ConsoleColor.Cyan   "HILI" msg

  let badf    fmt = kprintf bad  fmt
  let warnf   fmt = kprintf warn fmt
  let infof   fmt = kprintf info fmt
  let goodf   fmt = kprintf good fmt
  let hilif   fmt = kprintf hili fmt

  let dispose nm (d : #IDisposable) : unit =
    if not (isNull d) then
      try
        d.Dispose ()
      with
        | e ->
          badf "Failed to dispose %s because: %s" nm e.Message

  [<EntryPoint>]
  let main argv =
    try
      let culture = CultureInfo.InvariantCulture
      CultureInfo.DefaultThreadCurrentCulture   <- culture
      CultureInfo.DefaultThreadCurrentUICulture <- culture
      CultureInfo.CurrentCulture                <- culture
      CultureInfo.CurrentUICulture              <- culture

      Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

      let mutable options = WindowOptions.Default
      options.Title <- "FsShaderMixer"
      options.Size <- Vector2D<int> (1920, 1080)
      use window = Window.Create options

      let mutable appState = None

      let disposeAppState () =
        match appState with
        | None        -> ()
        | Some state  ->
          dispose "ImGui"     state.ImGui
          dispose "Input"     state.Input
          dispose "GL"        state.GL
          appState <- None

      let onLoad () =
        assert appState.IsNone
        disposeAppState ()

        let gl    = window.CreateOpenGL ()
        let input = window.CreateInput()
        let imgui = new ImGuiController(gl, window, input)

        let io = ImGui.GetIO ()
        io.FontGlobalScale <- 1.5F
        (*
        let fontBuilt = io.Fonts.Build ()
        assert fontBuilt
        *)
        appState <-
          {
            GL        = gl
            Input     = input
            ImGui     = imgui
            Position  = 0.F
            Pitch     = 1.F
          } |> Some


      let onFrameBufferResize (nsz : int Vector2D) =
        match appState with
        | None        -> ()
        | Some state  ->
          state.GL.Viewport nsz

      let onRender (delta : float) =
        match appState with
        | None        -> ()
        | Some state  ->
          state.GL.ClearColor (0.5F, 0.25F, 0.75F, 1.F)
          state.GL.Clear ((uint) ClearBufferMask.ColorBufferBit)

          state.ImGui.Update (float32 delta)

          //ImGuiNET.ImGui.ShowDemoWindow()
          let isVisible = ImGui.Begin "Shader Mixer Controls"
          try
            if isVisible then
              ImGui.LabelText ("BPM"  , "142")
              ImGui.LabelText ("Beat" , "0.00")
              if ImGui.SliderFloat ("Position", &state.Position, 0.F, 120.F) then
                ()
              if ImGui.SliderFloat ("Pitch"   , &state.Pitch   , 0.F, 3.F) then
                ()

              if ImGui.Button "Play" then
                ()
              ImGui.SameLine ()
              if ImGui.Button "Pause" then
                ()
              ImGui.SameLine ()
              if ImGui.Button "Reset Pitch" then
                ()


              ()
          finally
            ImGui.End ()

          state.ImGui.Render ()

      let onClosing () =
        disposeAppState ()

      window.add_Load               onLoad
      window.add_FramebufferResize  onFrameBufferResize
      window.add_Render             onRender
      window.add_Closing            onClosing

      window.Run ()

      0
    with
    | e -> 
      badf "App failed with: %s" e.Message
      99