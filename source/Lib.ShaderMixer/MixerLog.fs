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

open FSharp.Core.Printf

module MixerLog =
  let log (cc : ConsoleColor) (prelude : string) (msg : string) : unit =
    let occ = Console.ForegroundColor

    try
      Console.ForegroundColor <- cc
      Console.WriteLine (prelude + msg)
    finally
      Console.ForegroundColor <- occ

  let bad     msg = log ConsoleColor.Red    "BAD - " msg
  let warn    msg = log ConsoleColor.Yellow "WARN- " msg
  let info    msg = log ConsoleColor.Gray   "INFO- " msg
  let good    msg = log ConsoleColor.Green  "GOOD- " msg
  let hili    msg = log ConsoleColor.Cyan   "HILI- " msg

  let badf    fmt = kprintf bad  fmt
  let warnf   fmt = kprintf warn fmt
  let infof   fmt = kprintf info fmt
  let goodf   fmt = kprintf good fmt
  let hilif   fmt = kprintf hili fmt

  