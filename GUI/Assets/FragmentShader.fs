#version 330
uniform sampler2D albedo;
out vec4 color;
void main(){
  color=texture(albedo,gl_PointCoord);
}