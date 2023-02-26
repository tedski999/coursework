#version 330

in vec3 position;
in vec2 texcoords;
in vec3 normal;

out vec4 f_color;

uniform vec3 u_light_position;
uniform vec3 u_view_position;
uniform vec3 u_light_color;

uniform sampler2D u_texture;

void main() {
    // ambient
    float ambientStrength = 0.1;
    vec3 ambient = ambientStrength * u_light_color;

    // diffuse
    vec3 norm = normalize(normal);
    vec3 light_direction = normalize(u_light_position - position);
    float diff = max(dot(norm, light_direction), 0.0);
    vec3 diffuse = diff * u_light_color;

    // specular
    float specular_intensity = 0.5;
    vec3 viewDir = normalize(u_view_position - position);
    vec3 reflectDir = reflect(-light_direction, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec3 specular = specular_intensity * spec * u_light_color;

    f_color = vec4(ambient + diffuse + specular, 1.0) * texture(u_texture, texcoords);
}
