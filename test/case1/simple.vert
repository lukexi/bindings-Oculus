//
// Copyright : (c) T.Mishima 2014
// License : Apache-2.0
//
#version 150
in vec3 VertexPosition;
in vec4 VertexColor;

out vec4 Color;

uniform vec3 TransMat;
uniform mat4 ViewMat;

void main() {
  Color = VertexColor;  
  gl_Position = ViewMat * vec4(TransMat + VertexPosition,1.0);
//   // gl_Position.w = 1.0;
}

// uniform Transformation {
//         mat4 projection_matrix;
//         mat4 modelview_matrix;
// };

// in vec3 vertex;

// void main(void) {
//         gl_Position = projection_matrix * modelview_matrix * vec4(vertex, 1.0);
// }
