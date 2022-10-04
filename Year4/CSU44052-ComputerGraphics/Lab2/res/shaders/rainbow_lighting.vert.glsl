#version 330

layout (location = 0) in vec3 vPosition;
layout (location = 1) in vec4 vColor;
layout (location = 2) in vec3 vNormal;

out vec4 color;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

vec4 light_pos = vec4(50.0, 10.0, 4.0, 1.0);
vec3 Kd = vec3(1.0, 1.0, 1.0);
vec3 Ld = vec3(1.0, 1.0, 1.0);

void main() {
	vec4 eye = model * vec4(vPosition, 1.0);
	vec3 tnorm = normalize(mat3(model) * vNormal);
	vec3 s = normalize(vec3(light_pos - eye));
	vec3 l = Ld * Kd * max(dot(s, tnorm), 0.0);
	vec3 rainbow = vec3(sin(vPosition.x) * 0.5 + 1.0, cos(vPosition.y) * 0.5 + 1.0, sin(vPosition.z) * 0.5 + 1.0);
	color = vColor * vec4(l * rainbow, 1.0);
	gl_Position = projection * view * eye;
}
