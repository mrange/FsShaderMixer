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

module internal ShaderSources =
  let vertexShader =
    """#version 300 es

precision highp float;

in vec4 a_position;
in vec2 a_texcoord;

out vec2 v_texcoord;

void main() {
  gl_Position = a_position;
  v_texcoord = a_texcoord;
}
"""

  let fragmentShaderSourcePrelude =
    """#version 300 es

precision highp float;

uniform float iMix;
uniform float iTime;
uniform vec2 iResolution;
uniform sampler2D iChannel0;
uniform sampler2D iChannel1;
uniform sampler2D iChannel2;
uniform sampler2D iChannel3;

in vec2 v_texcoord;

out vec4 f_fragColor;

void mainImage(out vec4 fragColor, in vec2 fragCoord);

void main() {
  mainImage(f_fragColor, gl_FragCoord.xy);
}

"""

  let fragmentShaderBlack = """
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  fragColor = vec4(0.,0.,0.,1.);
}
"""

  let fragmentShaderRed = """
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  fragColor = vec4(1.,0.,0.,1.);
}
"""

  let fragmentShaderFaderPresenter = """
void mainImage(out vec4 fragColor, in vec2 fragCoord) {
  vec2 q = fragCoord/iResolution.xy;
  //q.y = 1. - q.y;
  vec3 col0 = texture(iChannel0, q).xyz;
  vec3 col1 = texture(iChannel1, q).xyz;
  fragColor = vec4(mix(col0, col1, iMix), 1.);
}
"""