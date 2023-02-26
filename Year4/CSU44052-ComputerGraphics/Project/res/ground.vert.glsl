#version 330

in vec3 v_position;
in vec3 v_normal;
in float v_height;

out vec4 color;

uniform mat4 u_model;
uniform mat4 u_transform;

vec3 sky = vec3(0.15f, 0.33f, .61f);
vec4 light_pos = vec4(-200.0, 200.0, 200.0, 1.0);
vec3 Kd = vec3(1.0, 1.0, 1.0);
vec3 Ld = vec3(1.0, 1.0, 1.0);

vec4 bottom_snow = vec4(0.5,0.5,0.7,1);
vec4 top_snow = vec4(0.9,0.9,1,1);

void main() {
	gl_Position = u_transform * vec4(v_position, 1.0);

	vec4 eye = u_model * vec4(v_position, 1.0);
	vec3 tnorm = normalize(mat3(u_model) * v_normal);
	vec3 s = normalize(vec3(light_pos - eye));
	vec3 l = Ld * Kd * max(dot(s, tnorm), 0.0);

	float f = (v_height - eye.y) / 2;
	color = vec4(l + sky, 1.0) * mix(top_snow, bottom_snow, f);
}
