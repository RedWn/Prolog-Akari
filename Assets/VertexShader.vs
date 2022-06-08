#version 330
uniform int x,y;
uniform float size;
void main(){
  gl_PointSize=size;
  gl_Position=vec4(x*size-size/2.0-1,y*size-size/2.0-1,1.0,1.0);
}