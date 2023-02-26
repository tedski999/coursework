#version 330

in vec4 color;
in vec2 texcoords;

out vec4 f_color;

uniform sampler2D u_texture;

void main() {
	f_color = color * texture(u_texture, texcoords);
}
