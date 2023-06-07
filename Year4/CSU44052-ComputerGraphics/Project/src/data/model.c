#include "model.h"
#include "../platform/alloc.h"
#include "../debug/assert.h"
#include "../debug/prof.h"
#include "wad.h"
#include <glad/gl.h>
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

struct cb_model *cb_model_load(struct zip *wad, char *path) {
	cb_prof(CB_PROF_START, "Model %s", path);
	struct cb_model *model = cb_calloc(1, sizeof *model);

	int data_len;
	char *data = cb_wad_load(wad, path, &data_len);

	cb_prof(CB_PROF_START, "aiImport");
	int flags = aiProcess_Triangulate | aiProcess_PreTransformVertices | aiProcess_JoinIdenticalVertices | aiProcess_FlipUVs;
	const struct aiScene *scene = aiImportFileFromMemory(data, data_len, flags, "obj");
	cb_assert(scene && scene->mRootNode && !(scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE), "Unable to parse model data from %s: %s\n", path, aiGetErrorString());
	cb_prof(CB_PROF_END, "aiImport");

	cb_prof(CB_PROF_START, "Generate");
	for (int i = 0; i < scene->mNumMeshes; i++) {
		const struct aiMesh *ai_mesh = scene->mMeshes[i];

		int indices_len = 0;
		GLuint *indices = NULL;
		GLfloat vertices[ai_mesh->mNumVertices * 3];
		GLfloat tex_coords[ai_mesh->mNumVertices * 2];
		GLfloat colors[ai_mesh->mNumVertices * 4];
		GLfloat normals[ai_mesh->mNumVertices * 3];

		for (int j = 0; j < ai_mesh->mNumFaces; j++) {
			struct aiFace face = ai_mesh->mFaces[j];
			int old_len = indices_len;
			indices_len += face.mNumIndices;
			indices = cb_realloc(indices, sizeof *indices * indices_len);
			for (int i = 0; i < face.mNumIndices; i++) {
				indices[old_len + i] = face.mIndices[i];
			}
		}

		for (int j = 0; j < ai_mesh->mNumVertices; j++) {
			vertices[j*3+0] = ai_mesh->mVertices[j].x;
			vertices[j*3+1] = ai_mesh->mVertices[j].y;
			vertices[j*3+2] = ai_mesh->mVertices[j].z;
			tex_coords[j*2+0] = ai_mesh->mTextureCoords[0][j].x;
			tex_coords[j*2+1] = ai_mesh->mTextureCoords[0][j].y;
			colors[j*4+0] = 1.0f;
			colors[j*4+1] = 1.0f;
			colors[j*4+2] = 1.0f;
			colors[j*4+3] = 1.0f;
			normals[j*3+0] = ai_mesh->mNormals[j].x;
			normals[j*3+1] = ai_mesh->mNormals[j].y;
			normals[j*3+2] = ai_mesh->mNormals[j].z;
		}

		model->meshes_len += 1;
		model->meshes = cb_realloc(model->meshes, sizeof *model->meshes * model->meshes_len);
		struct cb_mesh *mesh = &model->meshes[model->meshes_len-1];
		*mesh = (struct cb_mesh) { .len = indices_len };

		// TODO(shader types): this should be done by the gfx object (entities vs instances have different shader structures)

		glGenVertexArrays(1, &mesh->vao);
		glBindVertexArray(mesh->vao);

		glGenBuffers(1, &mesh->vbo);
		glBindBuffer(GL_ARRAY_BUFFER, mesh->vbo);
		glBufferData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 12 * sizeof (GLfloat), NULL, GL_STATIC_DRAW);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 0 * sizeof (GLfloat), ai_mesh->mNumVertices * 3 * sizeof (GLfloat), vertices);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 3 * sizeof (GLfloat), ai_mesh->mNumVertices * 2 * sizeof (GLfloat), tex_coords);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 5 * sizeof (GLfloat), ai_mesh->mNumVertices * 4 * sizeof (GLfloat), colors);
		glBufferSubData(GL_ARRAY_BUFFER, ai_mesh->mNumVertices * 9 * sizeof (GLfloat), ai_mesh->mNumVertices * 3 * sizeof (GLfloat), normals);

		glGenBuffers(1, &mesh->ebo);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->ebo);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, mesh->len * sizeof (GLuint), indices, GL_STATIC_DRAW);

		enum { POSITION, TEXCOORD, COLOR, NORMAL };
		glEnableVertexAttribArray(POSITION);
		glEnableVertexAttribArray(TEXCOORD);
		glEnableVertexAttribArray(COLOR);
		glEnableVertexAttribArray(NORMAL);
		glVertexAttribPointer(POSITION, 3, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 0 * sizeof (GLfloat)));
		glVertexAttribPointer(TEXCOORD, 2, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 3 * sizeof (GLfloat)));
		glVertexAttribPointer(COLOR,    4, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 5 * sizeof (GLfloat)));
		glVertexAttribPointer(NORMAL,   3, GL_FLOAT, GL_FALSE, 0, (void *) (ai_mesh->mNumVertices * 9 * sizeof (GLfloat)));
	}
	cb_prof(CB_PROF_END, "Generate");

	cb_free(data);

	cb_prof(CB_PROF_END, "Model %s", path);
	return model;

}

void cb_model_unload(struct cb_model *model) {
	for (int i = 0; i < model->meshes_len; i++) {
		glDeleteBuffers(1, &model->meshes[i].ebo);
		glDeleteBuffers(1, &model->meshes[i].vbo);
		glDeleteVertexArrays(1, &model->meshes[i].vao);
	}
	cb_free(model->meshes);
	cb_free(model);
}
