#version 330

in vec4 color;
in vec2 texcoords;

out vec4 f_color;

uniform sampler2D u_texture;
uniform vec4 u_white_color;
uniform vec4 u_black_color;

void main() {
	f_color = color * mix(u_black_color, u_white_color, texture(u_texture, texcoords));
}
