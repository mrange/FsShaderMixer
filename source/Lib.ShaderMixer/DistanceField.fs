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

module DistanceField =
  open OpenGLMath

  let createDistanceField 
    (radius     : int               )
    (cutoff     : float             )
    (buffer     : int               )
    (bitmapImage: MixerBitmapImage  )
    : MixerBitmapImage =

    let radius  = float radius

    let inf     = 1E20

    let pwidth  = int bitmapImage.Width
    let pheight = int bitmapImage.Height
    let bits    = bitmapImage.Bits

    let width   = pwidth  + 2*buffer
    let height  = pheight + 2*buffer
    let size    = max width height
    let psize   = int (bitmapImage.PixelByteSize ())
    let poff    = 
      match bitmapImage.Format with
      | RGBA8 -> 3
      | R8    -> 0

    let gridOuter : float   array = Array.create (width * height) inf
    let gridInner : float   array = Array.zeroCreate (width * height)
    let f         : float   array = Array.zeroCreate (size)
    let z         : float   array = Array.zeroCreate (size + 1)
    let v         : int     array = Array.zeroCreate (size)


    for y = 0 to pheight - 1 do
      for x = 0 to pwidth - 1 do
        let a = bits.[(x+y*pwidth)*psize+poff]

        let off = x + buffer + (y + buffer)*width

        if a = 0uy then
          ()
        elif a = 255uy then
          gridOuter.[off] <- 0.
          gridInner.[off] <- inf
        else
          let a   = (float a)/255.
          let d   = 0.5 - a
          let d2  = d*d
          gridOuter.[off] <- if d > 0. then d2 else 0.
          gridInner.[off] <- if d < 0. then d2 else 0.

    // See: https://cs.brown.edu/people/pfelzens/papers/dt-final.pdf
  #if DEBUG
    let edt1d (grid : float array) offset stride length =
  #else
    let inline edt1d (grid : float array) offset stride length =
  #endif
      let mutable k = 0

      v.[0] <- 0
      z.[0] <- -inf
      z.[1] <- inf
      f.[0] <- grid.[offset]

      for q = 1 to length - 1 do
        let fq = grid[offset + q * stride]
        f.[q] <- fq
        let q2 = float (q * q);
        let mutable cont = true
        let mutable s = 0.

        while cont do
          let vk = v.[k]
          s <- 0.5*((fq  - f.[vk]) + q2 - float (vk*vk))/(float (q - vk))
          cont <- s <= z.[k]
          if cont then
            k <- k - 1
            cont <- k > -1

        k <- k + 1

        v.[k] <- q
        z.[k] <- s
        z.[k + 1] <- inf

      k <- 0
      for q = 0 to length - 1 do
        while z.[k + 1] < q do
          k <- k + 1
        let vk = v.[k]
        let qr = float (q - vk)
        grid.[offset + q * stride] <- f[vk] + qr*qr 

  #if DEBUG
    let edt grid x0 y0 w h =
  #else
    let inline edt grid x0 y0 w h =
  #endif
      for x = x0 to x0 + w - 1 do
        edt1d grid (y0 * width + x) width h
      for y = y0 to height - 1 do
        edt1d grid (y * width + x0) 1 w

    edt gridOuter 0      0      width            height
    edt gridInner buffer buffer (width - buffer) (height - buffer)

    let data      : byte array = Array.zeroCreate (width * height)

    for i = 0 to data.Length - 1 do
      let d = sqrt gridOuter.[i] - sqrt gridInner.[i]
      let d = round (255. - 255.*(d/radius+cutoff))
      let d = clamp d 0. 255.0
      data.[i] <- byte d

    {
      Width   = uint32 width
      Height  = uint32 height
      Format  = R8
      Bits    = data
    }

