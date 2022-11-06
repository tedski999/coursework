#include "model.h"
#include "../core/wad.h"
#include "../util/alloc.h"
#include "../util/assert.h"
#include "../util/prof.h"
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

// TODO: error detection and generalising

struct cb_model *cbi_model(struct cbi_wad *wad, char *name) {
	cbi_prof(CBI_PROF_START, "Model %s", name);
	struct cb_model *model = cbi_calloc(1, sizeof *model);

	int data_len;
	char *data = cbi_wad_load(wad, name, &data_len);

	cbi_prof(CBI_PROF_START, "aiImport");
	int flags = aiProcess_Triangulate | aiProcess_PreTransformVertices | aiProcess_JoinIdenticalVertices | aiProcess_FlipUVs;
	const struct aiScene *scene = aiImportFileFromMemory(data, data_len, flags, NULL);
	cbi_assert(scene && scene->mRootNode && !(scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE), "Unable to parse model data from %s:\n", name, aiGetErrorString());
	cbi_prof(CBI_PROF_END, "aiImport");

	cbi_prof(CBI_PROF_START, "Generate");
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
			indices = cbi_realloc(indices, sizeof *indices * indices_len);
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
		model->meshes = cbi_realloc(model->meshes, sizeof *model->meshes * model->meshes_len);
		struct cbi_mesh *mesh = &model->meshes[model->meshes_len-1];
		*mesh = (struct cbi_mesh) { .len = indices_len };

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
	cbi_prof(CBI_PROF_END, "Generate");

	cbi_free(data);

	cbi_prof(CBI_PROF_END, "Model %s", name);
	return model;
}
