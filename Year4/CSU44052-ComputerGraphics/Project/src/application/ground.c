#include "ground.h"
#include "../alloc.h"
#include "../entity.h"

struct vertex {
	vec3 position;
	vec3 normal;
	float height;
};

struct data {
	GLuint shader;
	GLuint u_transform;
	GLuint u_model;
	GLuint vao, vbo, ebo;
	int indices_len;
	struct vertex *vertices;
	int vertices_len;
	int seed;
	int detail;
	double frequency;
	double scale;
	double size;
};

static const unsigned char HASH[] = {
    208,34,231,213,32,248,233,56,161,78,24,140,71,48,140,254,245,255,247,247,40,
    185,248,251,245,28,124,204,204,76,36,1,107,28,234,163,202,224,245,128,167,204,
    9,92,217,54,239,174,173,102,193,189,190,121,100,108,167,44,43,77,180,204,8,81,
    70,223,11,38,24,254,210,210,177,32,81,195,243,125,8,169,112,32,97,53,195,13,
    203,9,47,104,125,117,114,124,165,203,181,235,193,206,70,180,174,0,167,181,41,
    164,30,116,127,198,245,146,87,224,149,206,57,4,192,210,65,210,129,240,178,105,
    228,108,245,148,140,40,35,195,38,58,65,207,215,253,65,85,208,76,62,3,237,55,89,
    232,50,217,64,244,157,199,121,252,90,17,212,203,149,152,140,187,234,177,73,174,
    193,100,192,143,97,53,145,135,19,103,13,90,135,151,199,91,239,247,33,39,145,
    101,120,99,3,186,86,99,41,237,203,111,79,220,135,158,42,30,154,120,67,87,167,
    135,176,183,191,253,115,184,21,233,58,129,233,142,39,128,211,118,137,139,255,
    114,20,218,113,154,27,127,246,250,1,8,198,250,209,92,222,173,21,88,102,219
};

static int noise2(int x, int y) {
    return HASH[(HASH[y % 256] + x) % 256];
}

static double smooth_interp(double x, double y, double s) {
    return x + s * s * (3-2*s) * (y-x);
}

static double noise2d(int seed, double x, double y) {
    int x_int = floor(x), y_int = floor(y);
    double lo = smooth_interp(noise2(x_int, y_int+0+seed), noise2(x_int+1, y_int+0+seed), x - x_int);
    double hi = smooth_interp(noise2(x_int, y_int+1+seed), noise2(x_int+1, y_int+1+seed), x - x_int);
    return smooth_interp(lo, hi, y - y_int);
}

static double perlin2d(int seed, double x, double y, double freq) {
    return noise2d(seed, x*freq, y*freq) / 256;
}

static void compute_normal(vec3 *p0, vec3 *p1, vec3 *p2, vec3 *n) {
		vec3 a, b;
		glm_vec3_sub(*p1, *p0, a);
		glm_vec3_sub(*p2, *p0, b);
		(*n)[vX] = a[vZ] * b[vX] - a[vX] * b[vZ];
		(*n)[vY] = a[vX] * b[vY] - a[vY] * b[vX];
		(*n)[vZ] = a[vY] * b[vZ] - a[vZ] * b[vY];
}

static void on_render(struct ww_entity *ground, struct ww_transforms *transforms, double interp) {
	struct data *data = ground->data;

	glBindBuffer(GL_ARRAY_BUFFER, data->vbo);
	glBufferData(GL_ARRAY_BUFFER, data->vertices_len * sizeof *data->vertices, data->vertices, GL_DYNAMIC_DRAW);

	glUseProgram(data->shader);
	glUniformMatrix4fv(data->u_transform, 1, GL_FALSE, *transforms->mvp);
	glUniformMatrix4fv(data->u_model, 1, GL_FALSE, *transforms->m);
	glBindVertexArray(data->vao);
	glDrawElements(GL_TRIANGLES, data->indices_len, GL_UNSIGNED_INT, 0);
}

static void on_destroy(struct ww_entity *ground) {
	struct data *data = ground->data;
	ww_free(data->vertices);
	glDeleteBuffers(1, &data->ebo);
	glDeleteBuffers(1, &data->vbo);
	glDeleteVertexArrays(1, &data->vao);
	ww_free(data);
}

struct ww_entity *ww_ground(int seed, int detail, double frequency, double scale, double size, GLuint shader) {
	struct ww_entity *ground = ww_entity_create();
	struct data *data = ww_malloc(sizeof *data);

	ground->render_fn = on_render;
	ground->cleanup_fn = on_destroy;

	ground->data = data;
	data->shader = shader;
	data->u_model = glGetUniformLocation(data->shader, "u_model");
	data->u_transform = glGetUniformLocation(shader, "u_transform");
	data->seed = seed;
	data->frequency = frequency;
	data->scale = scale;
	data->size = size;
	data->detail = detail;

	int faces_len = (detail-1) * (detail-1);
	data->indices_len = faces_len * 6;
	data->vertices_len = detail * detail;
	data->vertices = malloc(data->vertices_len * sizeof *data->vertices);
	GLuint indices[data->indices_len];

	for (int i = 0; i < data->vertices_len; i++) {
		data->vertices[i].position[vX] = ((int)(i % detail)) / (detail-1.0) * size - (size / 2);
		data->vertices[i].position[vZ] = ((int)(i / detail)) / (detail-1.0) * size - (size / 2);
		data->vertices[i].height = perlin2d(seed, data->vertices[i].position[vX], data->vertices[i].position[vZ], frequency) * scale;
		data->vertices[i].position[vY] = data->vertices[i].height;
	}

	for (int i = 0; i < data->vertices_len; i++) {
		compute_normal(
			&data->vertices[i].position,
			&data->vertices[i+1].position,
			&data->vertices[i+detail].position,
			&data->vertices[i].normal);
	}

	for (int i = 0; i < faces_len; i++) {
		int offset = i / (detail-1);
		indices[i*6+0] = offset + (i+1);
		indices[i*6+1] = offset + (i);
		indices[i*6+2] = offset + (i+detail);
		indices[i*6+3] = offset + (i+1);
		indices[i*6+4] = offset + (i+detail);
		indices[i*6+5] = offset + (i+detail+1);
	}

	glGenVertexArrays(1, &data->vao);
	glBindVertexArray(data->vao);

	glGenBuffers(1, &data->ebo);
	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, data->ebo);
	glBufferData(GL_ELEMENT_ARRAY_BUFFER, data->indices_len * sizeof *indices, indices, GL_STATIC_DRAW);

	glGenBuffers(1, &data->vbo);
	glBindBuffer(GL_ARRAY_BUFFER, data->vbo);

	enum { POSITION, NORMAL, HEIGHT };
	glEnableVertexAttribArray(POSITION);
	glEnableVertexAttribArray(NORMAL);
	glEnableVertexAttribArray(HEIGHT);
	glVertexAttribPointer(POSITION,  3, GL_FLOAT, GL_FALSE, sizeof *data->vertices, (void *) (offsetof(struct vertex, position)));
	glVertexAttribPointer(NORMAL,    3, GL_FLOAT, GL_FALSE, sizeof *data->vertices, (void *) (offsetof(struct vertex, normal)));
	glVertexAttribPointer(HEIGHT,    1, GL_FLOAT, GL_FALSE, sizeof *data->vertices, (void *) (offsetof(struct vertex, height)));

	glBindVertexArray(0);

	return ground;
}

void ww_ground_deform(struct ww_entity *ground, double x, double z, double s) {
	struct data *data = ground->data;

	double min_x = data->vertices[0].position[vX];
	double min_z = data->vertices[0].position[vZ];
	double max_x = data->vertices[data->vertices_len-1].position[vX];
	double max_z = data->vertices[data->vertices_len-1].position[vZ];
	if (x < min_x) x = min_x; else if (x > max_x) x = max_x;
	if (z < min_z) z = min_z; else if (z > max_z) z = max_z;
	int xx = round((x - min_x) / (max_x - min_x) * data->detail);
	int zz = round((z - min_z) / (max_z - min_z) * data->detail);

	for (int ix = xx-3; ix < xx+3; ix++) {
		if (ix < 0 && ix >= data->detail)
			continue;
		for (int iz = zz-3; iz < zz+3; iz++) {
			if (iz < 0 && iz >= data->detail)
				continue;
			struct vertex *v = &data->vertices[ix * data->detail + iz];
			double d = (2-glm_vec2_distance((vec2){z,x},(vec2){v->position[vX],v->position[vZ]}));
			double nh = v->height - s*d/2;
			if (nh < v->position[vY])
				v->position[vY] = nh;
		}
	}
}

double ww_ground_get_height(struct ww_entity *ground, double x, double z) {
	struct data *data = ground->data;
	return perlin2d(data->seed, z, x, data->frequency) * data->scale;
}
