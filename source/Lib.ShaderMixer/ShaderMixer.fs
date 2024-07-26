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

open System
open System.Diagnostics
open System.Numerics
open System.Runtime.InteropServices
open System.Text

open Silk.NET.OpenGL

open FSharp.NativeInterop
open FSharp.Core.Printf

open MixerLog

type BitmapImageFormat =
  | R8
  | RGBA8

type MixerBitmapImage =
  {
    Width     : uint32
    Height    : uint32
    Format    : BitmapImageFormat
    Bits      : byte[]
  }

  member x.PixelByteSize () : uint32 =
    match x.Format with
    | RGBA8 -> 4u
    | R8    -> 1u

  member x.Validate () : MixerBitmapImage =
    if x.Width*x.Height*x.PixelByteSize () <> uint32 x.Bits.Length then
      failwithf "BitmapImage dimensions don't match the bits"
    else
      x

type BitmapImageID  = BitmapImageID of string
type PresenterID    = PresenterID   of string
type SceneID        = SceneID       of string

type ChannelSource =
  | BufferA
  | BufferB
  | BufferC
  | BufferD
  | Image
  | BitmapImage of  BitmapImageID

type ChannelFilter =
  | Nearest
  | Linear

type ChannelWrap =
  | Clamp
  | Repeat
  | MirroredRepeat

type BufferChannel =
  {
    Filter  : ChannelFilter
    Source  : ChannelSource
    Wrap    : ChannelWrap
  }

type PresenterChannel =
  {
    Filter  : ChannelFilter
    Wrap    : ChannelWrap
  }

type SceneBuffer =
  {
    FragmentSource  : string
    Channel0        : BufferChannel option
    Channel1        : BufferChannel option
    Channel2        : BufferChannel option
    Channel3        : BufferChannel option
  }

type MixerPresenter =
  {
    FragmentSource  : string
    Defines         : string array
    Channel0        : PresenterChannel
    Channel1        : PresenterChannel
  }

type MixerScene =
  {
    Defines       : string array
    Common        : string option
    BufferA       : SceneBuffer option
    BufferB       : SceneBuffer option
    BufferC       : SceneBuffer option
    BufferD       : SceneBuffer option
    Image         : SceneBuffer
  }

type BeatToTime   = float32 -> float32
type Fader        = float32 -> float32
type FaderFactory = BeatToTime -> float32 -> Fader

type ScriptPart =
  | SetPresenter  of PresenterID
  | SetStage0     of SceneID
  | SetStage1     of SceneID
  | ApplyFader    of FaderFactory

type Mixer =
  {
    NamedBitmapImages : Map<BitmapImageID , MixerBitmapImage>
    NamedPresenters   : Map<PresenterID   , MixerPresenter  >
    NamedScenes       : Map<SceneID       , MixerScene      >

    BPM               : float32
    LengthInBeats     : int

    InitialPresenter  : PresenterID
    InitialStage0     : SceneID
    InitialStage1     : SceneID

    Script            : (int*ScriptPart) array
  }

  member x.BeatToTime (beat : float32) : float32 = 
    beat*60.F/x.BPM

  member x.TimeToBeat (time : float32) : float32 = 
    time*x.BPM/60.F

[<Struct>]
[<StructLayout(LayoutKind.Sequential, Pack = 4)>]
type Vertex =
    val mutable Position: Vector3
    val mutable TexCoord: Vector2

[<Struct>]
type OpenGLBuffer         =
  {
    BufferID      : uint32
    Target        : GLEnum
    ElementSize   : uint32
  }

[<Struct>]
type OpenGLFrameBuffer    =
  {
    FrameBufferID : uint32
  }

[<Struct>]
type OpenGLProgram        =
  {
    ProgramID : uint32
  }

[<Struct>]
type OpenGLShader         =
  {
    ShaderID    : uint32
    ShaderType  : ShaderType
  }

[<Struct>]
type OpenGLTexture        =
  {
    TextureID : uint32
  }

[<Struct>]
type OpenGLVertexArray    =
  {
    VertexArrayID : uint32
  }

[<Struct>]
type OpenGLUniformLocation=
  {
    UniformLocationID : int
  }

type OpenGLMixerBitmapImage =
  {
    MixerBitmapImage  : MixerBitmapImage
    Texture           : OpenGLTexture
  }

type OpenGLStageTexture =
  {
    RenderingOrder  : int
    Texture0        : OpenGLTexture
    Texture1        : OpenGLTexture
  }
  member x.ForegroundTexture frameNo =
    if (frameNo &&& 1 = 0) then x.Texture0 else x.Texture1

  member x.BackgroundTexture frameNo =
    if (frameNo &&& 1 = 0) then x.Texture1 else x.Texture0

  member x.Texture currentRenderingOrder frameNo =
    if currentRenderingOrder > x.RenderingOrder then
      x.ForegroundTexture frameNo
    else
      x.BackgroundTexture frameNo
type OpenGLBufferChannel =
  {
    BufferChannel   : BufferChannel
    Location        : OpenGLUniformLocation voption
  }

type OpenGLPresenterChannel =
  {
    PresenterChannel: PresenterChannel
    Location        : OpenGLUniformLocation voption
  }

type OpenGLSceneBuffer =
  {
    RenderingOrder      : int

    SceneBuffer         : SceneBuffer

    Channel0            : OpenGLBufferChannel voption
    Channel1            : OpenGLBufferChannel voption
    Channel2            : OpenGLBufferChannel voption
    Channel3            : OpenGLBufferChannel voption

    VertexShader        : OpenGLShader
    FragmentShader      : OpenGLShader
    Program             : OpenGLProgram

    MixLocation         : OpenGLUniformLocation voption
    ResolutionLocation  : OpenGLUniformLocation voption
    TimeLocation        : OpenGLUniformLocation voption
  }

type OpenGLMixerPresenter =
  {
    MixerPresenter      : MixerPresenter

    Channel0            : OpenGLPresenterChannel
    Channel1            : OpenGLPresenterChannel

    VertexShader        : OpenGLShader
    FragmentShader      : OpenGLShader
    Program             : OpenGLProgram

    MixLocation         : OpenGLUniformLocation voption
    ResolutionLocation  : OpenGLUniformLocation voption
    TimeLocation        : OpenGLUniformLocation voption
  }

type OpenGLMixerScene =
  {
    MixerScene      : MixerScene

    BufferA         : OpenGLSceneBuffer voption
    BufferB         : OpenGLSceneBuffer voption
    BufferC         : OpenGLSceneBuffer voption
    BufferD         : OpenGLSceneBuffer voption
    Image           : OpenGLSceneBuffer
  }

type OpenGLMixerStage =
  {
    BufferA           : OpenGLStageTexture
    BufferB           : OpenGLStageTexture
    BufferC           : OpenGLStageTexture
    BufferD           : OpenGLStageTexture
    Image             : OpenGLStageTexture
  }

type ExpandedScriptPart =
  {
    Presenter : OpenGLMixerPresenter
    Stage0    : OpenGLMixerScene
    Stage1    : OpenGLMixerScene
    Fader     : Fader
  }

type OpenGLMixer =
  {
    Mixer             : Mixer
    Resolution        : Vector2

    NamedBitmapImages : Map<BitmapImageID , OpenGLMixerBitmapImage >
    NamedPresenters   : Map<PresenterID   , OpenGLMixerPresenter   >
    NamedScenes       : Map<SceneID       , OpenGLMixerScene       >

    ExpandedScript    : ExpandedScriptPart array

    Stage0            : OpenGLMixerStage
    Stage1            : OpenGLMixerStage

    FrameBuffer       : OpenGLFrameBuffer
    IndexBuffer       : OpenGLBuffer
    VertexBuffer      : OpenGLBuffer
    VertexArray       : OpenGLVertexArray

    Gl                : GL
  }

module Mixer =
  open OpenGLMath

  module internal Internals =
    let positionLocation  = 0u
    let texCoordLocation  = 1u

    let nullVoidPtr = NativePtr.toVoidPtr (NativePtr.nullPtr : byte nativeptr)

    let vertices : Vertex array =
        let nv x y z u v : Vertex =
          let mutable vv = Vertex ()
          vv.Position <- Vector3 (float32 x, float32 y, float32 z)
          vv.TexCoord <- Vector2 (float32 u, float32 v)
          vv
        [|
          nv -1 -1 0 0 0
          nv  1 -1 0 1 0
          nv -1  1 0 0 1
          nv  1  1 0 1 1
        |]
    let indices : uint16 array =
      [|
        uint16 0
        uint16 1
        uint16 2
        uint16 1
        uint16 3
        uint16 2
      |]

    let rec checkGL (gl : GL) : unit =

      let err = gl.GetError ()
      if err <> GLEnum.NoError then
        badf "OpenGL is in an error state: %A" err
#if DEBUG
        Debugger.Break ()
#endif
        checkGL gl

    let assertGL (gl : GL) : unit =
#if DEBUG
      checkGL gl
#else
      ()
#endif

    type OpenGLDebugMode(gl: GL) =
      class
        let debugProc 
          (source     : GLEnum      )
          (``type``   : GLEnum      )
          (id         : int         )
          (severity   : GLEnum      )
          (length     : int         )
          (message    : nativeint   )
          (userParam  : nativeint   )
          : unit = 
          let messageString = Marshal.PtrToStringAnsi (message, length)
          warnf "OpenGL Debug: %s" messageString

        let debugProcD = DebugProc debugProc
        do
          gl.Enable GLEnum.DebugOutput
          gl.DebugMessageCallback (debugProcD, nullVoidPtr)
        
        interface IDisposable with
          member x.Dispose () : unit =
            gl.DebugMessageCallback (null, nullVoidPtr)
            gl.Disable GLEnum.DebugOutput
      end

    let mapGet nm (m : Map<'K, 'V>) k =
      match m.TryGetValue k with
      | false , _ -> failwithf "Unabled to locate %s using key: %A" nm k
      | true  , v -> v

    let createAndBindBuffer<'T when 'T : unmanaged>
      (gl     : GL          )
      (target : GLEnum      )
      (vs     : 'T array    )
      : OpenGLBuffer =

      let elementSize = uint32 sizeof<'T>
      let buffer =
        {
          BufferID    = gl.GenBuffer ()
          Target      = target
          ElementSize = elementSize
        }

      gl.BindBuffer (buffer.Target, buffer.BufferID)
      checkGL gl

      do
        use ptr = fixed vs

        gl.BufferData (
                buffer.Target
            ,   unativeint (uint32 vs.Length*buffer.ElementSize)
            ,   NativePtr.toVoidPtr ptr
            ,   GLEnum.StaticDraw
            )
        checkGL gl

      buffer

    let createShader
      (gl         : GL            )
      (parentID   : string        )
      (shaderType : ShaderType    )
      (source     : string        )
      : OpenGLShader =

      let shader =
        {
          ShaderID  = gl.CreateShader shaderType
          ShaderType= shaderType
        }
      checkGL gl

      gl.ShaderSource (shader.ShaderID, source)
      checkGL gl

      gl.CompileShader shader.ShaderID
      checkGL gl

      shader

    let getUniformLocation
      (gl       : GL            )
      (program  : OpenGLProgram )
      (name     : string        )
      : OpenGLUniformLocation voption =

      let loc =
        {
          UniformLocationID = gl.GetUniformLocation (program.ProgramID, name)
        }
      checkGL gl

      // -1 indicates no matching uniform exists
      if loc.UniformLocationID <> -1 then
        ValueSome loc
      else
        ValueNone

    let createTexture
      (gl         : GL            )
      (resolution : Vector2       )
      : OpenGLTexture =

      let texture =
        {
          TextureID = gl.GenTexture ()
        }
      checkGL gl

      gl.BindTexture (GLEnum.Texture2D, texture.TextureID)
      checkGL gl

      gl.TexImage2D (GLEnum.Texture2D, 0, int GLEnum.Rgba32f, uint32 resolution.X, uint32 resolution.Y, 0, GLEnum.Rgba, GLEnum.Float, nullVoidPtr)
      checkGL gl

      gl.BindTexture (GLEnum.Texture2D, 0u)
      checkGL gl

      texture

    let createTextureFromBitmapImage
      (gl               : GL              )
      (mixerBitmapImage : MixerBitmapImage)
      : OpenGLTexture =

      mixerBitmapImage.Validate () |> ignore

      let texture =
        {
          TextureID = gl.GenTexture ()
        }
      checkGL gl

      gl.BindTexture (GLEnum.Texture2D, texture.TextureID)
      checkGL gl

      do
        use ptr = fixed mixerBitmapImage.Bits
        let internalFormat, format =
          match mixerBitmapImage.Format with
          | RGBA8 -> GLEnum.Rgba8   , GLEnum.Rgba
          | R8    -> GLEnum.R8      , GLEnum.Red
        gl.TexImage2D (GLEnum.Texture2D, 0, int internalFormat, mixerBitmapImage.Width, mixerBitmapImage.Height, 0, format, GLEnum.UnsignedByte, NativePtr.toVoidPtr ptr)
        checkGL gl

      gl.BindTexture (GLEnum.Texture2D, 0u)
      checkGL gl

      texture

    let createOpenGLStageTexture
      (gl               : GL            )
      (resolution       : Vector2       )
      (renderingOrder   : int           )
      : OpenGLStageTexture =

      {
        RenderingOrder  = renderingOrder
        Texture0        = createTexture gl resolution
        Texture1        = createTexture gl resolution
      }

    let createOpenGLBufferChannel
      (gl             : GL            )
      (program        : OpenGLProgram )
      (name           : string        )
      (bufferChannel  : BufferChannel )
      : OpenGLBufferChannel =

      {
        BufferChannel   = bufferChannel
        Location        = getUniformLocation gl program name
      }

    let createOpenGLBufferChannel'
      (gl             : GL                  )
      (program        : OpenGLProgram       )
      (name           : string              )
      (bufferChannel  : BufferChannel option)
      : OpenGLBufferChannel voption =

      match bufferChannel with
      | None    -> ValueNone
      | Some bc -> createOpenGLBufferChannel gl program name bc |> ValueSome

    let createOpenGLPresenterChannel
      (gl               : GL              )
      (program          : OpenGLProgram   )
      (name             : string          )
      (presenterChannel : PresenterChannel)
      : OpenGLPresenterChannel =

      {
        PresenterChannel= presenterChannel
        Location        = getUniformLocation gl program name
      }

    let createFragmentSource
      (common         : string option )
      (defines        : string array  )
      (fragmentSource : string        )
      : string =

      let prelude = "#define "
      let common  = match common with | None -> "" | Some s -> s
      let capacity =
          ShaderSources.fragmentShaderSourcePrelude.Length
        + common.Length
        + (Array.sumBy (fun (define : string) -> define.Length + prelude.Length) defines)
        + fragmentSource.Length
        + 2*(3 + defines.Length)        // Line endings
        + 16                            // Some extra bytes to be sure we don't reallocate
      let sb = StringBuilder capacity
      ignore <| sb.AppendLine ShaderSources.fragmentShaderSourcePrelude
      ignore <| sb.AppendLine common
      for define in defines do
        ignore <| sb.Append prelude
        ignore <| sb.AppendLine define
      ignore <| sb.AppendLine fragmentSource

      sb.ToString ()

    let createProgram
      (gl             : GL            )
      (parentID       : string        )
      (common         : string option )
      (defines        : string array  )
      (fragmentSource : string        ) =

      let vertexShader    = createShader gl parentID ShaderType.VertexShader    ShaderSources.vertexShader
      let fragmentShader  = createShader gl parentID ShaderType.FragmentShader  <| createFragmentSource common defines fragmentSource

      let program         =
        {
          ProgramID = gl.CreateProgram()
        }
      checkGL gl

      gl.AttachShader (program.ProgramID, vertexShader.ShaderID)
      checkGL gl

      gl.AttachShader (program.ProgramID, fragmentShader.ShaderID)
      checkGL gl

      gl.BindAttribLocation (program.ProgramID, positionLocation, "a_position")
      checkGL gl

      gl.BindAttribLocation (program.ProgramID, texCoordLocation, "a_texcoord")
      checkGL gl

      gl.LinkProgram program.ProgramID
      checkGL gl

      let mixLocation               = getUniformLocation gl program "iMix"
      let resolutionUniformLocation = getUniformLocation gl program "iResolution"
      let timeUniformLocation       = getUniformLocation gl program "iTime"

      struct (vertexShader, fragmentShader, program, mixLocation, resolutionUniformLocation, timeUniformLocation)

    let createOpenGLSceneBuffer
      (gl             : GL            )
      (SceneID        sceneID         )
      (mixerScene     : MixerScene    )
      (renderingOrder : int           )
      (sceneBuffer    : SceneBuffer   )
      : OpenGLSceneBuffer =

      let struct (vertexShader, fragmentShader, program, mixLocation, resolutionUniformLocation, timeUniformLocation) =
        createProgram gl sceneID mixerScene.Common mixerScene.Defines sceneBuffer.FragmentSource

      {
        RenderingOrder      = renderingOrder
        SceneBuffer         = sceneBuffer
        Channel0            = createOpenGLBufferChannel' gl program "iChannel0" sceneBuffer.Channel0
        Channel1            = createOpenGLBufferChannel' gl program "iChannel1" sceneBuffer.Channel1
        Channel2            = createOpenGLBufferChannel' gl program "iChannel2" sceneBuffer.Channel2
        Channel3            = createOpenGLBufferChannel' gl program "iChannel3" sceneBuffer.Channel3

        VertexShader        = vertexShader
        FragmentShader      = fragmentShader
        Program             = program

        MixLocation         = mixLocation
        ResolutionLocation  = resolutionUniformLocation
        TimeLocation        = timeUniformLocation
      }

    let createOpenGLSceneBuffer'
      (gl             : GL                )
      (sceneID        : SceneID           )
      (mixerScene     : MixerScene        )
      (renderingOrder : int               )
      (sceneBuffer    : SceneBuffer option)
      : OpenGLSceneBuffer voption =

      match sceneBuffer with
      | None    -> ValueNone
      | Some sb -> createOpenGLSceneBuffer gl sceneID mixerScene renderingOrder sb |> ValueSome

    let createOpenGLMixerPresenter
      (gl             : GL            )
      (PresenterID      presenterID   )
      (mixerPresenter : MixerPresenter)
      : OpenGLMixerPresenter =

      let struct (vertexShader, fragmentShader, program, mixLocation, resolutionUniformLocation, timeUniformLocation) =
        createProgram gl presenterID None mixerPresenter.Defines mixerPresenter.FragmentSource

      {
        MixerPresenter      = mixerPresenter
        Channel0            = createOpenGLPresenterChannel gl program "iChannel0" mixerPresenter.Channel0
        Channel1            = createOpenGLPresenterChannel gl program "iChannel1" mixerPresenter.Channel1

        VertexShader        = vertexShader
        FragmentShader      = fragmentShader
        Program             = program

        MixLocation         = mixLocation
        ResolutionLocation  = resolutionUniformLocation
        TimeLocation        = timeUniformLocation
      }

    let createOpenGLMixerScene
      (gl         : GL            )
      (sceneID    : SceneID       )
      (mixerScene : MixerScene    )
      : OpenGLMixerScene =

      {
        MixerScene = mixerScene
        BufferA    = createOpenGLSceneBuffer'  gl sceneID mixerScene 0 mixerScene.BufferA
        BufferB    = createOpenGLSceneBuffer'  gl sceneID mixerScene 1 mixerScene.BufferB
        BufferC    = createOpenGLSceneBuffer'  gl sceneID mixerScene 2 mixerScene.BufferC
        BufferD    = createOpenGLSceneBuffer'  gl sceneID mixerScene 3 mixerScene.BufferD
        Image      = createOpenGLSceneBuffer   gl sceneID mixerScene 4 mixerScene.Image
      }

    let createOpenGLMixerBitmapImage
      (gl               : GL              )
      (mixerBitmapImage : MixerBitmapImage)
      : OpenGLMixerBitmapImage =

      {
        MixerBitmapImage  = mixerBitmapImage
        Texture           = createTextureFromBitmapImage gl mixerBitmapImage
      }

    let createOpenGLMixerStage
      (gl         : GL            )
      (resolution : Vector2       )
      : OpenGLMixerStage =

      {
        BufferA           = createOpenGLStageTexture gl resolution 0
        BufferB           = createOpenGLStageTexture gl resolution 1
        BufferC           = createOpenGLStageTexture gl resolution 2
        BufferD           = createOpenGLStageTexture gl resolution 3
        Image             = createOpenGLStageTexture gl resolution 4
      }

    let tearDownOpenGLBufferChannel
      (mixer          : OpenGLMixer         )
      (bufferChannel  : OpenGLBufferChannel )
      : unit =

      ()

    let tearDownOpenGLBufferChannel'
      (mixer          : OpenGLMixer                 )
      (bufferChannel  : OpenGLBufferChannel voption )
      : unit =

      match bufferChannel with
      | ValueNone     -> ()
      | ValueSome bc  -> tearDownOpenGLBufferChannel mixer bc

    let tearDownOpenGLPresenterChannel
      (mixer              : OpenGLMixer           )
      (presenterChannel   : OpenGLPresenterChannel)
      : unit =

      ()

    let tearDownOpenGLStageTexture
      (mixer        : OpenGLMixer       )
      (stageTexture : OpenGLStageTexture)
      : unit =

      let gl    = mixer.Gl

      gl.DeleteTexture stageTexture.Texture1.TextureID
      assertGL gl

      gl.DeleteTexture stageTexture.Texture0.TextureID
      assertGL gl

    let tearDownOpenGLMixerBitmapImage
      (mixer            : OpenGLMixer           )
      (mixerBitmapImage : OpenGLMixerBitmapImage)
      : unit =

      let gl    = mixer.Gl

      gl.DeleteTexture mixerBitmapImage.Texture.TextureID
      assertGL gl

    let tearDownOpenGLSceneBuffer
      (mixer        : OpenGLMixer       )
      (sceneBuffer  : OpenGLSceneBuffer )
      : unit =

      let gl    = mixer.Gl

      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel3
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel2
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel1
      tearDownOpenGLBufferChannel' mixer sceneBuffer.Channel0

      gl.DeleteProgram                sceneBuffer.Program.ProgramID
      assertGL gl

      gl.DeleteShader                 sceneBuffer.FragmentShader.ShaderID
      assertGL gl

      gl.DeleteShader                 sceneBuffer.VertexShader.ShaderID
      assertGL gl

    let tearDownOpenGLSceneBuffer'
      (mixer        : OpenGLMixer               )
      (sceneBuffer  : OpenGLSceneBuffer voption )
      : unit =

      match sceneBuffer with
      | ValueNone     -> ()
      | ValueSome sb  -> tearDownOpenGLSceneBuffer mixer sb

    let tearDownOpenGLMixerScene
      (mixer      : OpenGLMixer     )
      (mixerScene : OpenGLMixerScene)
      : unit =

      tearDownOpenGLSceneBuffer' mixer mixerScene.BufferD
      tearDownOpenGLSceneBuffer' mixer mixerScene.BufferC
      tearDownOpenGLSceneBuffer' mixer mixerScene.BufferB
      tearDownOpenGLSceneBuffer' mixer mixerScene.BufferA
      tearDownOpenGLSceneBuffer  mixer mixerScene.Image

    let tearDownOpenGLMixerPresenter
      (mixer          : OpenGLMixer         )
      (mixerPresenter : OpenGLMixerPresenter)
      : unit =

      let gl    = mixer.Gl

      tearDownOpenGLPresenterChannel mixer mixerPresenter.Channel1
      tearDownOpenGLPresenterChannel mixer mixerPresenter.Channel0

      gl.DeleteProgram mixerPresenter.Program.ProgramID
      assertGL gl

      gl.DeleteShader  mixerPresenter.FragmentShader.ShaderID
      assertGL gl

      gl.DeleteShader  mixerPresenter.VertexShader.ShaderID
      assertGL gl

    let tearDownOpenGLMixerStage
      (mixer      : OpenGLMixer     )
      (mixerStage : OpenGLMixerStage)
      : unit =

      tearDownOpenGLStageTexture mixer mixerStage.Image
      tearDownOpenGLStageTexture mixer mixerStage.BufferD
      tearDownOpenGLStageTexture mixer mixerStage.BufferC
      tearDownOpenGLStageTexture mixer mixerStage.BufferB
      tearDownOpenGLStageTexture mixer mixerStage.BufferA

    let resizeOpenGLStageTexture
      (mixer        : OpenGLMixer       )
      (resolution   : Vector2           )
      (stageTexture : OpenGLStageTexture)
      : OpenGLStageTexture =

      let gl    = mixer.Gl

      tearDownOpenGLStageTexture mixer stageTexture
      createOpenGLStageTexture   gl resolution stageTexture.RenderingOrder

    let resizeOpenGLMixerStage
      (mixer      : OpenGLMixer     )
      (resolution : Vector2         )
      (mixerStage : OpenGLMixerStage)
      : OpenGLMixerStage =

      let gl = mixer.Gl

      {
        BufferA     = resizeOpenGLStageTexture mixer resolution mixerStage.BufferA
        BufferB     = resizeOpenGLStageTexture mixer resolution mixerStage.BufferB
        BufferC     = resizeOpenGLStageTexture mixer resolution mixerStage.BufferC
        BufferD     = resizeOpenGLStageTexture mixer resolution mixerStage.BufferD
        Image       = resizeOpenGLStageTexture mixer resolution mixerStage.Image
      }

    let renderOpenGLTexture
      (mixer          : OpenGLMixer           )
      (textureUnitNo  : int                   )
      (sourceTexture  : OpenGLTexture         )
      (loc            : OpenGLUniformLocation )
      (filter         : ChannelFilter         )
      (wrap           : ChannelWrap           )
      : unit =

      let gl    = mixer.Gl

      let textureUnit = enum<TextureUnit> (int TextureUnit.Texture0 + textureUnitNo)

      gl.ActiveTexture textureUnit
      checkGL gl

      gl.BindTexture (GLEnum.Texture2D, sourceTexture.TextureID)
      checkGL gl

      match filter with
      | Nearest ->
        gl.TexParameter (GLEnum.Texture2D, GLEnum.TextureMinFilter, int GLEnum.Nearest)
        gl.TexParameter (GLEnum.Texture2D, GLEnum.TextureMagFilter, int GLEnum.Nearest)
      | Linear  ->
        gl.TexParameter (GLEnum.Texture2D, GLEnum.TextureMinFilter, int GLEnum.Linear)
        gl.TexParameter (GLEnum.Texture2D, GLEnum.TextureMagFilter, int GLEnum.Linear)
      checkGL gl

      match wrap with
      | Clamp   ->
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapS, int GLEnum.ClampToEdge)
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapT, int GLEnum.ClampToEdge)
      | Repeat  ->
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapS, int GLEnum.Repeat)
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapT, int GLEnum.Repeat)
      | MirroredRepeat ->
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapS, int GLEnum.MirroredRepeat)
        gl.TexParameter (GLEnum.Texture2D  , GLEnum.TextureWrapT, int GLEnum.MirroredRepeat)
      checkGL gl

      gl.Uniform1 (loc.UniformLocationID, textureUnitNo)
      checkGL gl

    let renderOpenGLBufferChannel
      (mixer          : OpenGLMixer         )
      (mixerStage     : OpenGLMixerStage    )
      (frameNo        : int                 )
      (textureUnitNo  : int                 )
      (renderingOrder : int                 )
      (bufferChannel  : OpenGLBufferChannel )
      : unit =

      match bufferChannel.Location with
      | ValueNone     ->  ()
      | ValueSome loc ->
        let sourceTexture =
          match bufferChannel.BufferChannel.Source with
          | BufferA       -> mixerStage.BufferA.Texture renderingOrder frameNo
          | BufferB       -> mixerStage.BufferB.Texture renderingOrder frameNo
          | BufferC       -> mixerStage.BufferC.Texture renderingOrder frameNo
          | BufferD       -> mixerStage.BufferD.Texture renderingOrder frameNo
          | Image         -> mixerStage.Image.Texture   renderingOrder frameNo
          | BitmapImage ii->
            let image = mapGet "the bitmap image" mixer.NamedBitmapImages ii
            image.Texture
        renderOpenGLTexture mixer textureUnitNo sourceTexture loc bufferChannel.BufferChannel.Filter bufferChannel.BufferChannel.Wrap

    let renderOpenGLPresenterChannel
      (mixer            : OpenGLMixer           )
      (frameNo          : int                   )
      (mixerStage       : OpenGLMixerStage      )
      (textureUnitNo    : int                   )
      (presenterChannel : OpenGLPresenterChannel)
      : unit =

      match presenterChannel.Location with
      | ValueNone     ->  ()
      | ValueSome loc ->
        let sourceTexture = mixerStage.Image.ForegroundTexture frameNo

        renderOpenGLTexture mixer textureUnitNo sourceTexture loc presenterChannel.PresenterChannel.Filter presenterChannel.PresenterChannel.Wrap

      ()

    let renderOpenGLBufferChannel'
      (mixer          : OpenGLMixer                 )
      (mixerStage     : OpenGLMixerStage            )
      (frameNo        : int                         )
      (textureUnitNo  : int                         )
      (renderingOrder : int                         )
      (bufferChannel  : OpenGLBufferChannel voption )
      : unit =

      match bufferChannel with
      | ValueNone     -> ()
      | ValueSome bc  -> renderOpenGLBufferChannel mixer mixerStage frameNo textureUnitNo renderingOrder bc

    let renderProgram
      (mixer              : OpenGLMixer                   )
      (mix                : float32                       )
      (time               : float32                       )
      (mixLocation        : OpenGLUniformLocation voption )
      (resolutionLocation : OpenGLUniformLocation voption )
      (timeLocation       : OpenGLUniformLocation voption ) =

      let gl    = mixer.Gl

      match mixLocation with
      | ValueNone   -> ()
      | ValueSome ml->
        gl.Uniform1 (ml.UniformLocationID, mix)
        checkGL gl

      match resolutionLocation with
      | ValueNone   -> ()
      | ValueSome rl->
        gl.Uniform2 (rl.UniformLocationID, mixer.Resolution.X, float32 mixer.Resolution.Y)
        checkGL gl

      match timeLocation with
      | ValueNone   -> ()
      | ValueSome tl->
        gl.Uniform1 (tl.UniformLocationID, time)
        checkGL gl

      gl.DrawElements (GLEnum.Triangles, uint32 indices.Length, GLEnum.UnsignedShort, nullVoidPtr)
      checkGL gl

    let renderOpenGLSceneBuffer
      (mixer          : OpenGLMixer       )
      (mixerStage     : OpenGLMixerStage  )
      (time           : float32           )
      (frameNo        : int               )
      (stageTexture   : OpenGLStageTexture)
      (sceneBuffer    : OpenGLSceneBuffer )
      : unit =

      let gl    = mixer.Gl

      let targetTexture = stageTexture.ForegroundTexture frameNo

      gl.FramebufferTexture2D (GLEnum.Framebuffer, GLEnum.ColorAttachment0, GLEnum.Texture2D, targetTexture.TextureID, 0)
      checkGL gl


      let fboStatus = gl.CheckFramebufferStatus (GLEnum.Framebuffer)

      if fboStatus <> GLEnum.FramebufferComplete then
        badf "FBO Status: %A" fboStatus

      gl.UseProgram sceneBuffer.Program.ProgramID
      checkGL gl

      renderOpenGLBufferChannel' mixer mixerStage frameNo 0 sceneBuffer.RenderingOrder sceneBuffer.Channel0
      renderOpenGLBufferChannel' mixer mixerStage frameNo 1 sceneBuffer.RenderingOrder sceneBuffer.Channel1
      renderOpenGLBufferChannel' mixer mixerStage frameNo 2 sceneBuffer.RenderingOrder sceneBuffer.Channel2
      renderOpenGLBufferChannel' mixer mixerStage frameNo 3 sceneBuffer.RenderingOrder sceneBuffer.Channel3

      renderProgram mixer 0.F time sceneBuffer.MixLocation sceneBuffer.ResolutionLocation sceneBuffer.TimeLocation

    let renderOpenGLSceneBuffer'
      (mixer        : OpenGLMixer               )
      (mixerStage   : OpenGLMixerStage          )
      (time         : float32                   )
      (frameNo      : int                       )
      (stageTexture : OpenGLStageTexture        )
      (sceneBuffer  : OpenGLSceneBuffer voption )
      : unit =

      match sceneBuffer with
      | ValueNone     -> ()
      | ValueSome sb  -> renderOpenGLSceneBuffer mixer mixerStage time frameNo stageTexture sb

    let renderOpenGLMixerStage
      (mixer      : OpenGLMixer     )
      (time       : float32         )
      (frameNo    : int             )
      (mixerStage : OpenGLMixerStage)
      (scene      : OpenGLMixerScene)
      : unit =

      renderOpenGLSceneBuffer' mixer mixerStage time frameNo mixerStage.BufferA scene.BufferA
      renderOpenGLSceneBuffer' mixer mixerStage time frameNo mixerStage.BufferB scene.BufferB
      renderOpenGLSceneBuffer' mixer mixerStage time frameNo mixerStage.BufferC scene.BufferC
      renderOpenGLSceneBuffer' mixer mixerStage time frameNo mixerStage.BufferD scene.BufferD
      renderOpenGLSceneBuffer  mixer mixerStage time frameNo mixerStage.Image   scene.Image

    let renderOpenGLMixerPresenter
      (mixer          : OpenGLMixer         )
      (mix            : float32             )
      (time           : float32             )
      (frameNo        : int                 )
      (stage0         : OpenGLMixerStage    )
      (stage1         : OpenGLMixerStage    )
      (mixerPresenter : OpenGLMixerPresenter)
      : unit =

      let gl    = mixer.Gl

      gl.UseProgram mixerPresenter.Program.ProgramID
      checkGL gl

      renderOpenGLPresenterChannel mixer frameNo stage0 0 mixerPresenter.Channel0
      renderOpenGLPresenterChannel mixer frameNo stage1 1 mixerPresenter.Channel1

      renderProgram mixer mix time mixerPresenter.MixLocation mixerPresenter.ResolutionLocation mixerPresenter.TimeLocation

    let expandScript
      (mixer              : Mixer                                  )
      (namedPresenters    : Map<PresenterID , OpenGLMixerPresenter>)
      (namedScenes        : Map<SceneID     , OpenGLMixerScene    >)
      : ExpandedScriptPart array =

      let groupedScriptParts =
        mixer.Script
        |> Array.groupBy fst
        |> Map.ofArray


      let faderStage0 : Fader = fun time -> 0.F
      let mutable presenter   = mapGet "the intitial presenter" namedPresenters mixer.InitialPresenter
      let mutable stage0      = mapGet "the intitial stage 0"   namedScenes     mixer.InitialStage0
      let mutable stage1      = mapGet "the intitial stage 1"   namedScenes     mixer.InitialStage1
      let mutable fader       = faderStage0

      let expandedScript : ExpandedScriptPart array = Array.zeroCreate mixer.LengthInBeats

      for beat = 0 to mixer.LengthInBeats - 1 do
        match groupedScriptParts.TryGetValue beat with
        | false , _           -> ()
        | true  , scriptParts ->
          for _, scriptPart in scriptParts do
            match scriptPart with
            | SetPresenter  pid   -> presenter  <- mapGet "the presenter" namedPresenters pid
            | SetStage0     sid   -> stage0     <- mapGet "stage0" namedScenes sid
            | SetStage1     sid   -> stage1     <- mapGet "stage1" namedScenes sid
            | ApplyFader    faderf-> fader      <- faderf mixer.BeatToTime (float32 beat)
        expandedScript.[beat] <-
          {
            Presenter = presenter
            Stage0    = stage0
            Stage1    = stage1
            Fader     = fader
          }

      expandedScript

    module Loops =
      let rec toOpacityMask (f : byte array) (t : byte array) fi ti =
        if fi < f.Length then
          t.[ti] <- f.[fi]
          toOpacityMask f t (fi + 4) (ti + 1)

  open Internals

  let toOpacityMask (bitMapImage : MixerBitmapImage) : MixerBitmapImage =
    bitMapImage.Validate () |> ignore
    match bitMapImage.Format with
    | RGBA8 ->
      let bits = Array.create (int (bitMapImage.Width*bitMapImage.Height)) 0uy

      Loops.toOpacityMask bitMapImage.Bits bits 3 0

      { bitMapImage with
          Format    = R8
          Bits      = bits
      }.Validate ()
    | R8    -> bitMapImage

  let setupOpenGLMixer
    (gl         : GL          )
    (resolution : Vector2     )
    (mixer      : Mixer       )
    : OpenGLMixer =
#if DEBUG
    info "setupOpenGLMixer called"
#endif

    checkGL gl

    if mixer.LengthInBeats < 1 then failwithf "LengthInBeats expected to be at least 1"

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGLDebugMode (gl)
#endif
#endif

    let frameBuffer =
      {
        FrameBufferID = gl.GenFramebuffer ()
      }
    checkGL gl

    let vertexBuffer  = createAndBindBuffer gl GLEnum.ArrayBuffer         vertices
    let indexBuffer   = createAndBindBuffer gl GLEnum.ElementArrayBuffer  indices

    let vertexArray   =
      {
        VertexArrayID = gl.GenVertexArray ()
      }
    checkGL gl

    gl.BindVertexArray vertexArray.VertexArrayID
    checkGL gl

    gl.VertexAttribPointer (
            positionLocation
        ,   3
        ,   GLEnum.Float
        ,   false
        ,   vertexBuffer.ElementSize
        ,   nullVoidPtr
        )
    checkGL gl

    gl.VertexAttribPointer(
            texCoordLocation
        ,   2
        ,   GLEnum.Float
        ,   false
        ,   vertexBuffer.ElementSize
        ,   12
        )
    checkGL gl

    gl.EnableVertexAttribArray positionLocation
    checkGL gl

    gl.EnableVertexAttribArray texCoordLocation
    checkGL gl

    let namedBitmapImages =
      mixer.NamedBitmapImages
      |> Map.map (fun k v -> createOpenGLMixerBitmapImage gl v)

    let namedPresenters =
      mixer.NamedPresenters
      |> Map.map (fun k v -> createOpenGLMixerPresenter gl k v)

    let namedScenes =
      mixer.NamedScenes
      |> Map.map (fun k v -> createOpenGLMixerScene gl k v)

    let expandedScript = expandScript mixer namedPresenters namedScenes

    let res =
      {
        Mixer             = mixer
        Resolution        = resolution
        NamedBitmapImages = namedBitmapImages
        NamedPresenters   = namedPresenters
        NamedScenes       = namedScenes
        ExpandedScript    = expandedScript
        Stage0            = createOpenGLMixerStage gl resolution
        Stage1            = createOpenGLMixerStage gl resolution
        FrameBuffer       = frameBuffer
        IndexBuffer       = indexBuffer
        VertexBuffer      = vertexBuffer
        VertexArray       = vertexArray
        Gl                = gl
      }

    gl.BindTexture (GLEnum.Texture2D, 0u)
    checkGL gl

    res

  let tearDownOpenGLMixer
    (mixer  : OpenGLMixer )
    : unit =

#if DEBUG
    info "tearDownOpenGLMixer called"
#endif

    let gl    = mixer.Gl
    assertGL gl

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGLDebugMode (gl)
#endif
#endif

    tearDownOpenGLMixerStage mixer mixer.Stage1
    tearDownOpenGLMixerStage mixer mixer.Stage0

    for kv in mixer.NamedScenes do
      let scene = kv.Value
      tearDownOpenGLMixerScene mixer scene

    for kv in mixer.NamedPresenters do
      let presenter = kv.Value
      tearDownOpenGLMixerPresenter mixer presenter

    for kv in mixer.NamedBitmapImages do
      let bitmapImage = kv.Value
      tearDownOpenGLMixerBitmapImage mixer bitmapImage

    gl.DeleteVertexArray mixer.VertexArray.VertexArrayID
    assertGL gl

    gl.DeleteBuffer       mixer.VertexBuffer.BufferID
    assertGL gl

    gl.DeleteBuffer       mixer.IndexBuffer.BufferID
    assertGL gl

    gl.DeleteFramebuffer  mixer.FrameBuffer.FrameBufferID
    assertGL gl

    assertGL gl

  let resizeOpenGLMixer
    (resolution : Vector2     )
    (mixer      : OpenGLMixer ) : OpenGLMixer =

#if DEBUG
    info "resizeOpenGLMixer called"
#endif

    let gl    = mixer.Gl
    checkGL gl

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGLDebugMode (gl)
#endif
#endif

    let res =
      { mixer with
          Resolution  = resolution
          Stage0      = resizeOpenGLMixerStage mixer resolution mixer.Stage0
          Stage1      = resizeOpenGLMixerStage mixer resolution mixer.Stage1
      }

    gl.BindTexture (GLEnum.Texture2D, 0u)
    checkGL gl

    res
  let renderOpenGLMixer
    (time         : float32     )
    (frameNo      : int         )
    (mixer        : OpenGLMixer )
    : unit =

    let gl    = mixer.Gl
    checkGL gl

#if DEBUG
#if CAPTURE_OPENGL_LOGS
    use _ = new OpenGLDebugMode (gl)
#endif
#endif

    // This should have been check in the setup
    assert (mixer.ExpandedScript.Length > 0)

    let beat      = clamp (int ((mixer.Mixer.BPM*time)/60.F)) 0 (mixer.ExpandedScript.Length - 1)
    let scriptPart= mixer.ExpandedScript.[beat]

    let presenter = scriptPart.Presenter
    let scene0    = scriptPart.Stage0
    let scene1    = scriptPart.Stage1
    let mix       = scriptPart.Fader time

    let stage0    = mixer.Stage0
    let stage1    = mixer.Stage1

    let oldFbo = gl.GetInteger GLEnum.FramebufferBinding

    (*
    gl.Viewport (view.X, view.Y, view.Width, view.Height)
    checkGL gl
    *)

    gl.BindFramebuffer (GLEnum.Framebuffer, mixer.FrameBuffer.FrameBufferID)
    checkGL gl

    gl.BindBuffer (mixer.VertexBuffer.Target, mixer.VertexBuffer.BufferID)
    checkGL gl

    gl.BindBuffer (mixer.IndexBuffer.Target, mixer.IndexBuffer.BufferID)
    checkGL gl

    gl.BindVertexArray mixer.VertexArray.VertexArrayID
    checkGL gl

    if mix < 1.F then
      renderOpenGLMixerStage mixer time frameNo stage0 scene0

    if mix > 0.F then
      renderOpenGLMixerStage mixer time frameNo stage1 scene1

    gl.FramebufferTexture2D (GLEnum.Framebuffer, GLEnum.ColorAttachment0, GLEnum.Texture2D, 0u, 0)
    checkGL gl

    gl.BindFramebuffer (GLEnum.Framebuffer, uint32 oldFbo)
    checkGL gl

    renderOpenGLMixerPresenter mixer mix time frameNo stage0 stage1 presenter

    gl.BindTexture (GLEnum.Texture2D, 0u)
    checkGL gl

