#version 330

layout (location = 0) in vec3 vPosition;
layout (location = 1) in vec4 vColor;
layout (location = 2) in vec3 vNormal;
out vec4 color;

void main() {
	gl_Position = vec4(vPosition, 1.0);
	color = vColor;
}
