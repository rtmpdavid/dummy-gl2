#version 150 core

uniform mat4 model;
uniform mat4 proj;
uniform vec3 color;

//object data
in vec3 vertex_position;
in vec3 vertex_normal;
in vec2 texture_coord;

out vec3 Color;
out vec2 Texcoord;
varying vec3 norm_interp;
varying vec3 vert_pos;


void main() 
{
  vec4 vert_pos4 = model * vec4(vertex_position, 1.0);
  mat4 normal_matrix = transpose(inverse(model));

  vert_pos = vec3(vert_pos4) / vert_pos4.w;
  norm_interp = vec3(normal_matrix*vec4(vertex_normal, 0.0));
  Color = color;
  Texcoord = texture_coord;
  gl_Position = proj*model*vec4(vertex_position, 1.0);
}
