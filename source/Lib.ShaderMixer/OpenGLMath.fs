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

module OpenGLMath =
  let inline clamp x minVal maxVal = min (max x minVal) maxVal

  let mix (x : float32) (y : float32) (a : float32) = x*(1.F-a)+y*a

  let smoothstep (edge0 : float32) (edge1 : float32) (x : float32) : float32 =
    let t = clamp ((x - edge0) / (edge1 - edge0)) 0.F 1.F
    t * t * (3.F - 2.F * t)


