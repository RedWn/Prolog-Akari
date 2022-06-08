#version 330
uniform int x,y;
uniform float size;
uniform float sizeFactor;
void main(){
  gl_PointSize=sizeFactor*size;
  gl_Position=vec4(x*size-size/2.0-1,y*size-size/2.0-1,0.0,1.0);
}