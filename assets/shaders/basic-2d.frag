#version 150 core
in vec3 Color;
in vec2 Texcoord;
out vec4 outColor;

varying vec3 norm_interp;
varying vec3 vert_pos;

uniform sampler2D tex;
void main() 
{
  vec3 norm = normalize(norm_interp);
  // vec3 light_dir = normalize(vec3(0.0, 1.0, 0.0) - vert_pos);
  vec3 light_dir = normalize(vec3(0.0, 0.0, -1.0));

  float dif = max(dot(light_dir,norm), 0.0);
  float amb = 0.2;

  float spec = 0.0;
  if(dif > 0.0)
  {
    vec3 view_dir = normalize(-vert_pos);
    vec3 half_dir = normalize(light_dir + view_dir);
    float spec_angle = max(dot(half_dir, norm), 0.0);
    spec = pow(spec_angle, 16.0);
    // vec3 reflect_dir = reflect(-light_dir, norm);
    // float spec_angle = max(dot(reflect_dir, view_dir), 0.0);
    // spec = pow(spec_angle, 16.0);
  }
  float lum = amb+0.3*dif+0.7*spec;

  vec4 tex = texture(tex, Texcoord);
  outColor = vec4(vec3(tex)*lum, tex.w);
  //outColor = vec4(tex.r, tex.g, lum, 1.0);
}
