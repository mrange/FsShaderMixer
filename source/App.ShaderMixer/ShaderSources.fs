﻿(*
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

module ShaderSources =
  // While 'FsShaderMixer' is GPL v3 this license is not intended
  //  to apply to the shader sources found here. Many might be prior art
  //  found on ShaderToy that have their own individual license which I have no right
  //  or want to relicense to GPL v3

  let gravitySucks = """
  // CC0: Gravity sucks
  //  Tinkering away....

  #define LAYERS      5.
  #define SCALE       1.

  #define TIME        iTime
  #define RESOLUTION  iResolution
  #define PI          3.141592654
  #define TAU         (2.0*PI)


  // License: Unknown, author: Unknown, found: don't remember
  float hash(float co) {
    return fract(sin(co*12.9898) * 13758.5453);
  }

  // License: MIT OR CC-BY-NC-4.0, author: mercury, found: https://mercury.sexy/hg_sdf/
  float mod1(inout float p, float size) {
    float halfsize = size*.5;
    float c = floor((p + halfsize)/size);
    p = mod(p + halfsize, size) - halfsize;
    return c;
  }

  // License: Unknown, author: Unknown, found: don't remember
  float bounce(float t, float dy, float dropOff) {
    const float g = 5.;
    float p0 = 2.*dy/g;

    t += p0/2.;

    float ldo = log(dropOff);

    float yy = 1. - (1. - dropOff) * t / p0;

    if (yy > 1e-4)  {
      float n  = floor(log(yy) / ldo);
      float dn = pow(dropOff, n);

      float yyy = dy * dn;
      t -= p0 * (1. - dn) / (1. - dropOff);

      return -.5*g*t*t + yyy*t;

    } else {
        return 0.;
    }
  }

  vec3 ball(vec3 col, vec2 pp, vec2 p, float r, float pal) {
    const vec3 ro = vec3(0.,0., 10.);
    const vec3 difDir = normalize(vec3(1., 1.5, 2.));
    const vec3 speDir = normalize(vec3(1., 2., 1.));
    vec3 p3 = vec3(pp, 0.);
    vec3 rd = normalize(p3-ro);

    vec3 bcol = .5+.5*sin(0.5*vec3(0., 1., 2.)+TAU*pal);
    float aa = sqrt(8.)/RESOLUTION.y;
    float z2 = (r*r-dot(p, p));
    if (z2 > 0.) {
      float z = sqrt(z2);
      vec3 cp = vec3(p, z);
      vec3 cn = normalize(cp);
      vec3 cr = reflect(rd, cn);
      float cd= max(dot(difDir, cn), 0.0);
      float cs= 1.008-dot(cr, speDir);

      vec3 ccol = mix(.1, 1.,cd*cd)*bcol+sqrt(bcol)*(1E-2/cs);
      float d = length(p)-r;
      col = mix(col, ccol, smoothstep(0., -aa, d));
    }

    return col;
  }


  vec3 effect(vec2 p) {
    p.y += .5;
    float sy = sign(p.y);
    p.y = abs(p.y);
    if (sy < 0.) {
      p.y*=  1.5;
    }

    vec3 col = vec3(0.);
    float aa = sqrt(4.)/RESOLUTION.y;
    for (float i = 0.; i < LAYERS; ++i) {
      float h0 = hash(i+123.4);
      float h1 = fract(8667.0*h0);
      float h2 = fract(8707.0*h0);
      float h3 = fract(8887.0*h0);
      float tf = mix(.5, 1.5, h3);
      float it = tf*TIME;
      float cw = mix(0.25, 0.75, h0*h0)*SCALE;
      float per = mix(0.75, 1.5, h1*h1)*cw;
      vec2 p0 = p;
      float nt = floor(it/per);
      p0.x -= cw*(it-nt*per)/per;
      float n0 = mod1(p0.x, cw)-nt;
      if (n0 > -7.-i*3.) continue;
      float ct = it+n0*per;

      float ch0 = hash(h0+n0);
      float ch1 = fract(8667.0*ch0);
      float ch2 = fract(8707.0*ch0);
      float ch3 = fract(8887.0*ch0);
      float ch4 = fract(9011.0*ch0);

      float radii = cw*mix(.25, .5, ch0*ch0);
      float dy = mix(3., 2., ch3);
      float bf = mix(.6, .9, ch2);
      float b = bounce(ct/tf+ch4, dy, bf);
      p0.y -= b+radii;
      col = ball(col, p, p0, radii, ch1);
    }

    if (sy < 0.) {
      col *= mix(sqrt(vec3(.05, .1, .2)), vec3(.05, .1, .2), p.y);
      col += .1*vec3(0., 0., 1.)*max(p.y*p.y, 0.);
    }

    col = sqrt(col);
    return col;
  }

  void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    vec2 p = (-RESOLUTION.xy+2.*fragCoord)/RESOLUTION.yy;
    vec3 col = effect(p);

    fragColor = vec4(col,1.);
  }
  """



