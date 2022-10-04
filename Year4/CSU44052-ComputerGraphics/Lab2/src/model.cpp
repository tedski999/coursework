#include "model.hpp"
#include <string>
#include <iostream>
#include <cassert>
#include <GL/glew.h>
#include <assimp/Importer.hpp>
#include <assimp/postprocess.h>
#include <assimp/scene.h>

yar::model::model(std::string file) {
	Assimp::Importer importer;
	const aiScene *scene = importer.ReadFile(file, aiProcess_Triangulate | aiProcess_PreTransformVertices | aiProcess_JoinIdenticalVertices | aiProcess_GenNormals);
	if (!scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode) {
		std::cerr << "Error: "<< importer.GetErrorString() << std::endl;
		exit(1);
	}

	for (int i = 0; i < scene->mNumMeshes; i++) {
		const aiMesh *ai_mesh = scene->mMeshes[i];

		assert(ai_mesh->HasPositions());
		assert(ai_mesh->HasFaces());
		assert(ai_mesh->HasNormals());

		std::vector<GLuint> indices;
		GLfloat vertices[ai_mesh->mNumVertices * 3];
		GLfloat colors[ai_mesh->mNumVertices * 4];
		GLfloat normals[ai_mesh->mNumVertices * 3];

		for (int j = 0; j < ai_mesh->mNumFaces; j++) {
			aiFace face = ai_mesh->mFaces[j];
			indices.insert(indices.end(), face.mIndices, face.mIndices + face.mNumIndices);
		}

		for (int j = 0; j < ai_mesh->mNumVertices; j++) {
			vertices[j*3+0] = ai_mesh->mVertices[j].x;
			vertices[j*3+1] = ai_mesh->mVertices[j].y;
			vertices[j*3+2] = ai_mesh->mVertices[j].z;
			colors[j*4+0] = 0.8f;
			colors[j*4+1] = 1.0f;
			colors[j*4+2] = 1.0f;
			colors[j*4+3] = 1.0f;
			normals[j*3+0] = ai_mesh->mNormals[j].x;
			normals[j*3+1] = ai_mesh->mNormals[j].y;
			normals[j*3+2] = ai_mesh->mNormals[j].z;
		}

		struct mesh mesh = { .len = (int) indices.size() };

		glGenVertexArrays(1, &mesh.vao);
		glBindVertexArray(mesh.vao);

		glGenBuffers(1, &mesh.vbo);
		glBindBuffer(GL_ARRAY_BUFFER, mesh.vbo);
		glBufferData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 10 * sizeof (GLfloat), NULL, GL_STATIC_DRAW);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 0 * sizeof (GLfloat), ai_mesh->mNumVertices * 3 * sizeof (GLfloat), vertices);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 3 * sizeof (GLfloat), ai_mesh->mNumVertices * 4 * sizeof (GLfloat), colors);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 7 * sizeof (GLfloat), ai_mesh->mNumVertices * 3 * sizeof (GLfloat), normals);

		glGenBuffers(1, &mesh.ebo);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh.ebo);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, mesh.len * sizeof (GLuint), indices.data(), GL_STATIC_DRAW);

		enum { POSITION, COLOR, NORMAL };
		glEnableVertexAttribArray(POSITION);
		glVertexAttribPointer(POSITION, 3, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 0 * sizeof (GLfloat)));
		glEnableVertexAttribArray(COLOR);
		glVertexAttribPointer(COLOR,    4, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 3 * sizeof (GLfloat)));
		glEnableVertexAttribArray(NORMAL);
		glVertexAttribPointer(NORMAL,   3, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 7 * sizeof (GLfloat)));

		meshes.push_back(mesh);
	}
}

yar::model::~model() {
	for (struct mesh mesh : meshes) {
		glDeleteBuffers(1, &mesh.vbo);
		glDeleteBuffers(1, &mesh.ebo);
		glDeleteVertexArrays(1, &mesh.vao);
	}
}

void yar::model::draw() {
	for (struct mesh mesh : meshes) {
		glBindVertexArray(mesh.vao);
        glDrawElements(GL_TRIANGLES, mesh.len, GL_UNSIGNED_INT, 0);
	}
}
