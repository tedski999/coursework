#version 330

in vec3 v_position;
in vec3 v_normal;
in vec4 v_bone_weights;
in ivec4 v_bone_indices;

out vec4 color;

uniform mat4 u_model;
uniform mat4 u_transform;
uniform mat4 u_bones[128];

vec3 sky = vec3(0.15f, 0.33f, .61f);
vec4 light_pos = vec4(-200.0, 200.0, 200.0, 1.0);
vec3 Kd = vec3(1.0, 1.0, 1.0);
vec3 Ld = vec3(1.0, 1.0, 1.0);

void main() {
	mat4 bone_transform = mat4(0);
	for (int i = 0; i < 4; i++)
		bone_transform += u_bones[v_bone_indices[i]] * v_bone_weights[i];
	gl_Position = u_transform * bone_transform * vec4(v_position, 1.0);

	vec4 eye = u_model * vec4(v_position, 1.0);
	vec3 tnorm = normalize(mat3(u_model) * v_normal);
	vec3 s = normalize(vec3(light_pos - eye));
	vec3 l = Ld * Kd * max(dot(s, tnorm), 0.0);
	color = vec4(l + sky, 1.0);
}
