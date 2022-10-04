#ifndef YAR_YAR_HPP
#define YAR_YAR_HPP

#include "model.hpp"
#include "shader.hpp"
#include "object.hpp"
#define GLFW_INCLUDE_NONE
#include <GLFW/glfw3.h>

namespace yar {
	class yar {

	public:
		yar();
		~yar();
		void run();

	private:
		void update();
		void render();

	private:
		bool is_running;
		GLFWwindow *window;
		model *teapot_model, *teacup_model;
		shader *basic_shader, *simple_lighting_shader, *rainbow_lighting_shader;
		object *teapot, *teacup1, *teacup2, *teacup3;
	};
}

#endif
