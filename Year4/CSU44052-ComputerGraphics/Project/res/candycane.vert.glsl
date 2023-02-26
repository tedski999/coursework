#version 330

in vec3 v_position;
in vec3 v_normal;
in vec2 v_texcoords;

out vec3 position;
out vec2 texcoords;
out vec3 normal;

uniform mat4 u_model;
uniform mat4 u_transform;

void main() {
	gl_Position = u_transform * vec4(v_position, 1.0);
    position = vec3(u_model * vec4(v_position, 1.0));
    normal = mat3(transpose(inverse(u_model))) * v_normal;
	texcoords = v_texcoords;
}
