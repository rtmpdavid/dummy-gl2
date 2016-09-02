#version 150 core

uniform vec3 color;

//object data
in vec3 vertex_position;
in vec3 vertex_normal;

out vec3 Color;

void main() 
{
  Color = color;
  gl_Position = vec4(vertex_position, 1.0);
}
