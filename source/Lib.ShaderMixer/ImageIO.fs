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
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats

type SixLaborsImage =
  | ImageL8      of Image<L8>
  | ImageRgba32  of Image<Rgba32>

  interface IDisposable with
    member x.Dispose () : unit =
      match x with
      | ImageL8      image -> image.Dispose ()
      | ImageRgba32  image -> image.Dispose ()

  member x.GetImage () : Image =
    match x with
    | ImageL8      image -> image :> Image
    | ImageRgba32  image -> image :> Image

module ImageIO =
  open SixLabors.Fonts
  open SixLabors.ImageSharp.Drawing.Processing
  open SixLabors.ImageSharp.Processing

  let createSixLaborsImage 
    (width        : uint32            )
    (height       : uint32            )
    (format       : BitmapImageFormat )
    : SixLaborsImage =
    match format with 
    | R8      ->  ImageL8     <| new Image<L8>     (int width, int height)
    | RGBA8   ->  ImageRgba32 <| new Image<Rgba32> (int width, int height)

  let toSixLaborsImage
    (bitmapImage  :  MixerBitmapImage )
    : SixLaborsImage =
      bitmapImage.Validate () |> ignore

      match bitmapImage.Format with
      | BitmapImageFormat.R8    ->
        ImageL8     <| Image<L8>.LoadPixelData (bitmapImage.Bits, int bitmapImage.Width, int bitmapImage.Height)
      | BitmapImageFormat.RGBA8 ->
        ImageRgba32 <| Image<Rgba32>.LoadPixelData (bitmapImage.Bits, int bitmapImage.Width, int bitmapImage.Height)

  let toMixerBitmapImage
    (sixLaborsImage :  SixLaborsImage)
    : MixerBitmapImage =
      let bits, format =
        match sixLaborsImage with
        | ImageL8 image ->
          let bits : byte array = Array.zeroCreate (image.Width*image.Height)
          image.CopyPixelDataTo bits
          bits, R8
        | ImageRgba32 image ->
          let bits : byte array = Array.zeroCreate (4*image.Width*image.Height)
          image.CopyPixelDataTo bits
          bits, RGBA8

      let image = sixLaborsImage.GetImage()
      {
        Width     = uint32 image.Width
        Height    = uint32 image.Height
        Format    = format
        Bits      = bits
      }.Validate ()

  let loadSixLaborsImageFromFile
    (format       : BitmapImageFormat )
    (fileName     : string            )
    : SixLaborsImage =
    match format with
    | R8    -> 
      ImageL8 <| Image.Load<L8> fileName
    | RGBA8 ->
      ImageRgba32 <| Image.Load<Rgba32> fileName

  let loadMixerBitmapImageFromFile
    (format       : BitmapImageFormat )
    (fileName     : string            )
    : MixerBitmapImage =
    use image =  loadSixLaborsImageFromFile format fileName
    toMixerBitmapImage image

  let saveSixLaborsImageAsPng 
    (sixLaborsImage : SixLaborsImage  )
    (fileName       : string          )
    : unit =
    sixLaborsImage.GetImage().SaveAsPng fileName

  let saveMixerBitmapImageAsPng 
    (bitmapImage  : MixerBitmapImage)
    (fileName     : string          )
    : unit =
    use sixLaborsImage = toSixLaborsImage bitmapImage
    saveSixLaborsImageAsPng sixLaborsImage fileName

  let createFontCollection 
    (fontPaths : string array) 
    : Map<string, FontFamily> = 
    let fc = FontCollection ()
    let ra = ResizeArray fontPaths.Length
    for fontPath in fontPaths do
      let ff = fc.Add fontPath
      ra.Add (ff.Name, ff)
    ra |> Map.ofSeq

  let renderCenteredText 
    (sixLaborsImage : SixLaborsImage) 
    (font           : Font          ) 
    (n              : int           ) 
    (ny             : int           )
    (text           : string        ) 
    : unit =
    let image   = sixLaborsImage.GetImage ()
    let options = TextOptions font
    let size    = TextMeasurer.MeasureSize (text, options)
    let x       = (float32 image.Width - size.Width)*0.5F
    let h       = (float32 image.Height/float32 ny)
    let y       = h*float32 n
    let y       = y + (h - size.Height)*0.5F
    let mutator (ctx : IImageProcessingContext) = 
      ignore <| ctx.DrawText (text, font, Brushes.Solid Color.White, PointF (x, y))
    image.Mutate mutator
