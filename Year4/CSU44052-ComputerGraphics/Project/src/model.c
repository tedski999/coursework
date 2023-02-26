#include "model.h"
#include "alloc.h"
#include "assert.h"
#include "log.h"
#include "wad.h"
#include <glad/gl.h>
#include <cglm/cglm.h>
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

static void process_nodes(struct ww_model_node *node, struct aiNode *ai_node) {
	node->name = strdup(ai_node->mName.data);
	memcpy(&node->transform, &ai_node->mTransformation, sizeof (mat4));
	glm_mat4_transpose(node->transform);
	node->children_len = ai_node->mNumChildren;
	node->children = ww_malloc(node->children_len * sizeof *node->children);
	for (int i = 0; i < ai_node->mNumChildren; i++)
		process_nodes(&node->children[i], ai_node->mChildren[i]);
}

static void free_nodes(struct ww_model_node *node) {
	for (int i = 0; i < node->children_len; i++)
		free_nodes(&node->children[i]);
	ww_free(node->children);
	ww_free(node->name);
}

struct ww_model *ww_model_load(char *path, char *type) {
	struct ww_model *model = ww_malloc(sizeof *model);

	int data_len;
	char *data = ww_wad(path, &data_len);
	int flags = aiProcess_Triangulate | aiProcess_JoinIdenticalVertices | aiProcess_FlipUVs | aiProcess_LimitBoneWeights;
	const struct aiScene *scene = aiImportFileFromMemory(data, data_len, flags, type);
	ww_assert(scene, "Unable to parse model data from %s: %s", path, aiGetErrorString());
	ww_free(data);

	model->bones_len = 0;
	model->bones = NULL;
	model->meshes_len = scene->mNumMeshes;
	model->meshes = ww_malloc(model->meshes_len * sizeof *model->meshes);
	for (int m = 0; m < scene->mNumMeshes; m++) {
		struct aiMesh *ai_mesh = scene->mMeshes[m];
		struct ww_model_mesh *mesh = &model->meshes[m];

		mesh->indices_len = ai_mesh->mNumFaces * 3;
		mesh->indices = ww_malloc(mesh->indices_len * sizeof *mesh->indices);
		for (int j = 0; j < ai_mesh->mNumFaces; j++) {
			mesh->indices[j*3+0] = ai_mesh->mFaces[j].mIndices[0];
			mesh->indices[j*3+1] = ai_mesh->mFaces[j].mIndices[1];
			mesh->indices[j*3+2] = ai_mesh->mFaces[j].mIndices[2];
		}

		int bone_counts[ai_mesh->mNumVertices];
		mesh->vertices_len = ai_mesh->mNumVertices;
		mesh->vertices = ww_malloc(mesh->vertices_len * sizeof *mesh->vertices);
		for (int v = 0; v < ai_mesh->mNumVertices; v++) {
			bone_counts[v] = 0;
			mesh->vertices[v] = (struct ww_model_mesh_vertex) {
				{ ai_mesh->mVertices[v].x, ai_mesh->mVertices[v].y, ai_mesh->mVertices[v].z },
				{ ai_mesh->mNormals[v].x, ai_mesh->mNormals[v].y, ai_mesh->mNormals[v].z },
				{ ai_mesh->mTextureCoords[0][v].x, ai_mesh->mTextureCoords[0][v].y }
			};
		}

		for (int b = 0; b < ai_mesh->mNumBones; b++) {
			struct aiBone *ai_bone = ai_mesh->mBones[b];

			int bone_index = 0;
			for (; bone_index < model->bones_len; bone_index++)
				if (!strcmp(ai_bone->mName.data, model->bones[bone_index].name))
					break;
			if (bone_index == model->bones_len) {
				model->bones_len += 1;
				model->bones = ww_realloc(model->bones, model->bones_len * sizeof *model->bones);
				model->bones[model->bones_len - 1] = (struct ww_model_bone) { strdup(ai_bone->mName.data), GLM_MAT4_ZERO_INIT };
				memcpy(&model->bones[model->bones_len - 1].offset, &ai_bone->mOffsetMatrix, sizeof (mat4));
				glm_mat4_transpose(model->bones[model->bones_len - 1].offset);
			}

			for (int w = 0; w < ai_bone->mNumWeights; w++) {
				struct aiVertexWeight *vw = &ai_bone->mWeights[w];
				int i = bone_counts[vw->mVertexId]++;
				mesh->vertices[vw->mVertexId].bone_weights[i] = vw->mWeight;
				mesh->vertices[vw->mVertexId].bone_indices[i] = bone_index;
			}
		}

		glGenVertexArrays(1, &mesh->vao);
		glBindVertexArray(mesh->vao);
		glGenBuffers(1, &mesh->ebo);
		glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->ebo);
		glBufferData(GL_ELEMENT_ARRAY_BUFFER, mesh->indices_len * sizeof *mesh->indices, mesh->indices, GL_STATIC_DRAW);
		glGenBuffers(1, &mesh->vbo);
		glBindVertexArray(0);
	}

	model->animations_len = scene->mNumAnimations;
	model->animations = ww_malloc(model->animations_len * sizeof *model->animations);
	for (int a = 0; a < scene->mNumAnimations; a++) {
		struct aiAnimation *ai_animation = scene->mAnimations[a];
		struct ww_model_animation *animation = &model->animations[a];
		animation->name = strdup(ai_animation->mName.data);
		animation->duration = ai_animation->mDuration;
		animation->tps = ai_animation->mTicksPerSecond;
		animation->channels_len = ai_animation->mNumChannels;
		animation->channels = ww_malloc(animation->channels_len * sizeof *animation->channels);
		for (int c = 0; c < animation->channels_len; c++) {
			struct aiNodeAnim *ai_channel = ai_animation->mChannels[c];
			struct ww_model_animation_channel *channel = &animation->channels[c];
			channel->name = strdup(ai_channel->mNodeName.data);
			ww_assert(
				ai_channel->mNumPositionKeys == ai_channel->mNumRotationKeys && ai_channel->mNumRotationKeys == ai_channel->mNumScalingKeys,
				"'%s' channel '%s' key counts are different: %d %d %d", animation->name, channel->name,
				ai_channel->mNumPositionKeys, ai_channel->mNumRotationKeys, ai_channel->mNumScalingKeys);
			channel->keys_len = ai_channel->mNumPositionKeys;
			channel->keys = ww_malloc(channel->keys_len * sizeof *channel->keys);
			for (int k = 0; k < channel->keys_len; k++) {
				struct ww_model_animation_channel_key *key = &channel->keys[k];
				struct aiVector3D *position = &ai_channel->mPositionKeys[k].mValue;
				struct aiQuaternion *rotation = &ai_channel->mRotationKeys[k].mValue;
				struct aiVector3D *scaling = &ai_channel->mScalingKeys[k].mValue;
				glm_translate_make(key->transform, (vec3) { position->x, position->y, position->z });
				glm_quat_rotate(key->transform, (vec4) { rotation->x, rotation->y, rotation->z, rotation->w }, key->transform);
				glm_scale(key->transform, (vec3) { scaling->x, scaling->y, scaling->z });
				key->time = ai_channel->mPositionKeys[k].mTime;
			}
		}
	}

	struct aiMatrix4x4 identity;
	aiIdentityMatrix4(&identity);
	process_nodes(&model->root_node, scene->mRootNode);
	glm_mat4_inv_precise(model->root_node.transform, model->inverse_transform);

	aiReleaseImport(scene);
	return model;
}

void ww_model_render(struct ww_model *model) {
	for (int i = 0; i < model->meshes_len; i++) {
		struct ww_model_mesh *mesh = &model->meshes[i];
		glBindVertexArray(mesh->vao);
		glDrawElements(GL_TRIANGLES, mesh->indices_len, GL_UNSIGNED_INT, 0);
	}
	glBindVertexArray(0);
}

void ww_model_unload(struct ww_model *model) {
	for (int m = 0; m < model->meshes_len; m++) {
		struct ww_model_mesh *mesh = &model->meshes[m];
		ww_free(mesh->indices);
		ww_free(mesh->vertices);
		glDeleteVertexArrays(1, &mesh->vao);
		glDeleteBuffers(1, &mesh->vbo);
		glDeleteBuffers(1, &mesh->ebo);
	}
	for (int b = 0; b < model->bones_len; b++) {
		ww_free(model->bones[b].name);
	}
	for (int a = 0; a < model->animations_len; a++) {
		struct ww_model_animation *animation = &model->animations[a];
		for (int c = 0; c < animation->channels_len; c++) {
			struct ww_model_animation_channel *channel = &animation->channels[c];
			ww_free(channel->name);
			ww_free(channel->keys);
		}
		ww_free(animation->channels);
		ww_free(animation->name);
	}
	free_nodes(&model->root_node);
	ww_free(model->meshes);
	ww_free(model->bones);
	ww_free(model);
}
