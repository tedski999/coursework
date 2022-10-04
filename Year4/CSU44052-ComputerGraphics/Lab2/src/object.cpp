#include "object.hpp"
#include "shader.hpp"
#include "model.hpp"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

yar::object::object(yar::model *model, yar::shader *shader, glm::vec3 position, glm::vec3 rotation, glm::vec3 scale)
	: model(model), shader(shader), position(position), rotation(rotation), scale(scale) {}

void yar::object::render() {
	glm::mat4 transform(1.0f);
	transform = glm::translate(transform, position);
	transform = glm::rotate(transform, glm::radians(rotation.x), {1.0f, 0.0f, 0.0f});
	transform = glm::rotate(transform, glm::radians(rotation.y), {0.0f, 1.0f, 0.0f});
	transform = glm::rotate(transform, glm::radians(rotation.z), {0.0f, 0.0f, 1.0f});
	transform = glm::scale(transform, scale);
	shader->set_uniform("model", transform);
	shader->use();
	model->draw();
}
