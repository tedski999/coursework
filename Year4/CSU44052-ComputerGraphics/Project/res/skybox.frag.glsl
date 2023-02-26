#version 330

in vec2 texcoords;
in vec2 color;

out vec4 f_color;

uniform sampler2D u_texture;

void main() {
	f_color = texture(u_texture, texcoords);
}
