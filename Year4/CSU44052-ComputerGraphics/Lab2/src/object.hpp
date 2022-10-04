#ifndef YAR_OBJECT_HPP
#define YAR_OBJECT_HPP

#include "model.hpp"
#include "shader.hpp"
#include <glm/glm.hpp>

namespace yar {
	class object {

		public:
			object(yar::model *model, yar::shader *shader, glm::vec3 position, glm::vec3 rotation, glm::vec3 scale);
			void render();

		public:
			glm::vec3 position, rotation, scale;

		private:
			yar::model *model;
			yar::shader *shader;
	};
}

#endif
