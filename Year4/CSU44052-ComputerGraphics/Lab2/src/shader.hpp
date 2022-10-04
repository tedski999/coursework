#ifndef YAR_SHADER_HPP
#define YAR_SHADER_HPP

#include <string>
#include <GL/glew.h>
#include <glm/glm.hpp>

namespace yar {
	class shader {

		public:
			shader(std::string vert_file, std::string frag_file);
			~shader();
			void set_uniform(std::string name, glm::mat4 value);
			void use();

		private:
			GLuint program;
	};
}

#endif
