#version 330

in vec3 position;
in mat4 transform;

out vec4 color;

uniform mat4 u_transform;

void main() {
	gl_Position = u_transform * transform * vec4(position, 1.0);
	color = vec4(1);
}
