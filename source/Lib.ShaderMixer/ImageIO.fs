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

module ImageIO =
  open SixLabors.ImageSharp
  open SixLabors.ImageSharp.PixelFormats
  open SixLabors.ImageSharp.Formats.Png

  let loadFromFile
    (format       : BitmapImageFormat )
    (fileName     : string            )
    : MixerBitmapImage =

    let image = 
      match format with
      | RGBA8 ->
        use image = Image.Load<Rgba32> fileName
        let bits : byte array = Array.zeroCreate (4*image.Width*image.Height)
        image.CopyPixelDataTo bits
        {
          Width     = uint32 image.Width
          Height    = uint32 image.Height
          Format    = format
          Bits      = bits
        }
      | R8    -> 
        use image = Image.Load<L8> fileName
        let bits : byte array = Array.zeroCreate (image.Width*image.Height)
        image.CopyPixelDataTo bits
        {
          Width     = uint32 image.Width
          Height    = uint32 image.Height
          Format    = format
          Bits      = bits
        }
    image.Validate ()
    image
  let saveAsPng 
    (bitmapImage  : MixerBitmapImage)
    (fileName     : string          )
    : unit =

    bitmapImage.Validate ()

    match bitmapImage.Format with
    | RGBA8 ->
      use image = Image.LoadPixelData<Rgba32> (bitmapImage.Bits, int bitmapImage.Width, int bitmapImage.Height)
      image.SaveAsPng fileName
    | R8    -> 
      use image = Image.LoadPixelData<L8> (bitmapImage.Bits, int bitmapImage.Width, int bitmapImage.Height)
      image.SaveAsPng fileName
  

