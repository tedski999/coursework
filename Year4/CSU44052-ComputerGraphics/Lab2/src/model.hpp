#ifndef YAR_MODEL_HPP
#define YAR_MODEL_HPP

#include <string>
#include <vector>
#include <GL/glew.h>
#include <assimp/scene.h>

namespace yar {

	struct mesh {
		int len;
		GLuint vao, vbo, ebo;
	};

	class model {

		public:
			model(std::string file);
			~model();
			void draw();

		private:
			std::vector<mesh> meshes;
	};
}

#endif
