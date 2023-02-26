#version 330

in vec3 v_position;
in vec2 v_texcoords;

out vec2 texcoords;

uniform mat4 u_transform;

void main() {
	gl_Position = u_transform * vec4(v_position, 1.0);
	texcoords = v_texcoords;
}
