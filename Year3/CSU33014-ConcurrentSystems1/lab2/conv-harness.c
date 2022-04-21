/* CSU33014 Concurrent Systems I - Lab 2
 * Team members:
 * - Ted Johnson <edjohnso@tcd.ie> <TCD 19335618>
*/

/* Test and timing harness program for developing a multichannel
 * multikernel convolution (as used in deep learning networks)
 *
 * Note there are some simplifications around this implementation,
 * in particular with respect to computing the convolution at edge
 * pixels of the image.
 *
 * Author: David Gregg, Ted Johnson
 * Date:   April 2022
 *
 * Version 1.8 : Implemented student_conv
 *
 * Version 1.7 : Adjusted types for mixed-type computation
 *
 * Version 1.6 : Modified the code so that the input tensor is float
 *
 * Version 1.5 : Modified the code so that the input and kernel
 *               are tensors of 16-bit integer values
 *
 * Version 1.4 : Modified the random generator to reduce the range
 *               of generated values;
 *
 * Version 1.3 : Fixed which loop variables were being incremented
 *               in write_out();
 *               Fixed dimensions of output and control_output 
 *               matrices in main function
 *
 * Version 1.2 : Changed distribution of test data to (hopefully) 
 *               eliminate random walk of floating point error;
 *               Also introduced checks to restrict kernel-order to
 *               a small set of values
 *
 * Version 1.1 : Fixed bug in code to create 4d matrix
*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <assert.h>
#include <omp.h>
#include <math.h>
#include <stdint.h>
#include <string.h>
#include <immintrin.h>

/* the following two definitions of DEBUGGING control whether or not
 * debugging information is written out. To put the program into
 * debugging mode, uncomment the following line: */
/*#define DEBUGGING(_x) _x */
/* to stop the printing of debugging information, use the following line: */
#define DEBUGGING(_x)

/* write 3d matrix to stdout */
void write_out(int16_t ***a, int dim0, int dim1, int dim2) {
	int i, j, k;

	for (i = 0; i < dim0; i++) {
		printf("Outer dimension number %d\n", i);
		for (j = 0; j < dim1; j++) {
			for (k = 0; k < dim2 - 1; k++) {
				printf("%d, ", a[i][j][k]);
			}
			// print end of line
			printf("%f\n", (double) a[i][j][dim2-1]);
		}
	}
}

/* create new empty 4d float matrix */
float ****new_empty_4d_matrix_float(int dim0, int dim1, int dim2, int dim3) {
	float ****result = malloc(dim0 * sizeof(float***));
	float ***mat1 = malloc(dim0 * dim1 * sizeof(float**));
	float **mat2 = malloc(dim0 * dim1 * dim2 * sizeof(float*));
	float *mat3 = malloc(dim0 * dim1 * dim2 *dim3 * sizeof(float));
	int i, j, k;

	for (i = 0; i < dim0; i++) {
		result[i] = &(mat1[i*dim1]);
		for (j = 0; j < dim1; j++) {
			result[i][j] = &(mat2[i*dim1*dim2 + j*dim2]);
			for (k = 0; k < dim2; k++) {
				result[i][j][k] = &(mat3[i*dim1*dim2*dim3+j*dim2*dim3+k*dim3]);
			}
		}
	}

	return result;
}

/* create new empty 3d matrix */
float ***new_empty_3d_matrix_float(int dim0, int dim1, int dim2) {
	float ****mat4d;
	float ***mat3d;

	// create a 4d matrix with single first dimension
	mat4d = new_empty_4d_matrix_float(1, dim0, dim1, dim2);
	// now throw away out first dimension
	mat3d = mat4d[0];
	free(mat4d);
	return mat3d;
}

/* create new empty 4d int16_t matrix */
int16_t ****new_empty_4d_matrix_int16(int dim0, int dim1, int dim2, int dim3) {
	int16_t ****result = malloc(dim0 * sizeof(int16_t***));
	int16_t ***mat1 = malloc(dim0 * dim1 * sizeof(int16_t**));
	int16_t **mat2 = malloc(dim0 * dim1 * dim2 * sizeof(int16_t*));
	int16_t *mat3 = malloc(dim0 * dim1 * dim2 *dim3 * sizeof(int16_t));
	int i, j, k;

	for (i = 0; i < dim0; i++) {
		result[i] = &(mat1[i*dim1]);
		for (j = 0; j < dim1; j++) {
			result[i][j] = &(mat2[i*dim1*dim2 + j*dim2]);
			for (k = 0; k < dim2; k++) {
				result[i][j][k] = &(mat3[i*dim1*dim2*dim3+j*dim2*dim3+k*dim3]);
			}
		}
	}

	return result;
}

/* create new empty 3d matrix */
int16_t ***new_empty_3d_matrix_int16(int dim0, int dim1, int dim2) {
	int16_t ****mat4d;
	int16_t ***mat3d;

	// create a 4d matrix with single first dimension
	mat4d = new_empty_4d_matrix_int16(1, dim0, dim1, dim2);
	// now throw away out first dimension
	mat3d = mat4d[0];

	free(mat4d);
	return mat3d;
}

/* take a copy of the matrix and return in a newly allocated matrix */
int16_t ****copy_4d_matrix(int16_t ****source_matrix, int dim0, int dim1, int dim2, int dim3) {
	int i, j, k, l;
	int16_t ****result = new_empty_4d_matrix_int16(dim0, dim1, dim2, dim3);

	for (i = 0; i < dim0; i++) {
		for (j = 0; j < dim1; j++) {
			for (k = 0; k < dim2; k++) {
				for (l = 0; l < dim3; l++) {
					result[i][j][k][l] = source_matrix[i][j][k][l];
				}
			}
		}
	}

	return result;
}

/* create a matrix and fill it with random numbers */
int16_t ****gen_random_4d_matrix_int16(int dim0, int dim1, int dim2, int dim3) {
	int16_t ****result;
	int i, j, k, l;
	struct timeval seedtime;
	int seed;

	result = new_empty_4d_matrix_int16(dim0, dim1, dim2, dim3);

	/* use the microsecond part of the current time as a pseudorandom seed */
	gettimeofday(&seedtime, NULL);
	seed = seedtime.tv_usec;
	srandom(seed);

	/* fill the matrix with random numbers */
	const int range = 1 << 10; // 2^10
	//const int bias = 1 << 16; // 2^16
	for (i = 0; i < dim0; i++) {
		for (j = 0; j < dim1; j++) {
			for (k = 0; k < dim2; k++) {
				for (l = 0; l < dim3; l++) {
					// generate uniform random integer with mean of zero
					long long rand = random();
					// now cut down the range and bias the mean to reduce
					// the likelihood of large floating point round-off errors
					int reduced_range = (rand % range);
					result[i][j][k][l] = reduced_range;
				}
			}
		}
	}

	return result;
}


/* create a matrix and fill it with random numbers */
float ****gen_random_4d_matrix_float(int dim0, int dim1, int dim2, int dim3) {
	float ****result;
	int i, j, k, l;
	struct timeval seedtime;
	int seed;

	result = new_empty_4d_matrix_float(dim0, dim1, dim2, dim3);

	/* use the microsecond part of the current time as a pseudorandom seed */
	gettimeofday(&seedtime, NULL);
	seed = seedtime.tv_usec;
	srandom(seed);

	/* fill the matrix with random numbers */
	const int range = 1 << 12; // 2^12
	const int bias = 1 << 10; // 2^16
	for (i = 0; i < dim0; i++) {
		for (j = 0; j < dim1; j++) {
			for (k = 0; k < dim2; k++) {
				for (l = 0; l < dim3; l++) {
					// generate uniform random integer with mean of zero
					long long rand = random();
					// now cut down the range and bias the mean to reduce
					// the likelihood of large floating point round-off errors
					int reduced_range = (rand % range);
					result[i][j][k][l] = reduced_range + bias;
				}
			}
		}
	}

	return result;
}


/* create a matrix and fill it with random numbers */
float ***gen_random_3d_matrix_float(int dim0, int dim1, int dim2) {
	float ****mat4d;
	float ***mat3d;

	// create a 4d matrix with single first dimension
	mat4d = gen_random_4d_matrix_float(1, dim0, dim1, dim2);
	// now throw away out first dimension
	mat3d = mat4d[0];

	free(mat4d);
	return mat3d;
}

/* create a matrix and fill it with random numbers */
int16_t ***gen_random_3d_matrix_int16(int dim0, int dim1, int dim2) {
	int16_t ****mat4d;
	int16_t ***mat3d;

	// create a 4d matrix with single first dimension
	mat4d = gen_random_4d_matrix_int16(1, dim0, dim1, dim2);
	// now throw away out first dimension
	mat3d = mat4d[0];

	free(mat4d);
	return mat3d;
}

/* check the sum of absolute differences is within reasonable epsilon */
void check_result(float ***result, float ***control, int dim0, int dim1, int dim2) {
	int i, j, k;
	double sum_abs_diff = 0.0;
	const double EPSILON = 0.0625;

	//printf("SAD\n");

	for ( i = 0; i < dim0; i++ ) {
		for ( j = 0; j < dim1; j++ ) {
			for ( k = 0; k < dim2; k++ ) {
				double diff = fabs(control[i][j][k] - result[i][j][k]);
				assert( diff >= 0.0 );
				sum_abs_diff = sum_abs_diff + diff;
			}
		}
	}

	if (sum_abs_diff > EPSILON) {
		fprintf(stderr, "WARNING: sum of absolute differences (%f) > EPSILON (%f)\n", sum_abs_diff, EPSILON);
	} else {
		printf("COMMENT: sum of absolute differences (%f)  within acceptable range (%f)\n", sum_abs_diff, EPSILON);
	}
}

/* the slow but correct version of matmul written by David */
void multichannel_conv(float ***image, int16_t ****kernels, float ***output, int width, int height, int nchannels, int nkernels, int kernel_order) {
	int h, w, x, y, c, m;
	for (m = 0; m < nkernels; m++) {
		for (w = 0; w < width; w++) {
			for (h = 0; h < height; h++) {
				double sum = 0.0;
				for (c = 0; c < nchannels; c++) {
					for (x = 0; x < kernel_order; x++) {
						for (y = 0; y < kernel_order; y++) {
							sum += image[w+x][h+y][c] * kernels[m][c][x][y];
						}
					}
					output[m][w][h] = (float) sum;
				}
			}
		}
	}
}

/* the fast version of matmul written by the student */
void student_conv(float ***image, int16_t ****kernels, float ***output, int width, int height, int nchannels, int nkernels, int kernel_order) {

	// Our target machine offers 64 threads, so let's capitalise on this!
	// Each thread processes the input image using its own dedicated kernel and output buffer.
	#pragma omp parallel for
	for (int m = 0; m < nkernels; m++) {

		// Transform the input int16_t kernel stored as kernel[channel][x][y] to a float kernel stored as kernel[x][y][channel].
		// By storing the kernel 'channel-first', memory is accessed contiguously when operating on the input image.
		// We also convert to float and align to 16-bits for SSE processing later.
		__attribute__ ((aligned(16))) float kernel[kernel_order][kernel_order][nchannels];
		for (int x = 0; x < kernel_order; x++) {
			for (int y = 0; y < kernel_order; y++) {
				// As nchannels is divisible by 32, we can unroll this loop 32 times
				#pragma GCC unroll 32
				for (int c = 0; c < nchannels; c++) {
					kernel[x][y][c] = kernels[m][c][x][y];
				}
			}
		}

		// For every pixel in the output image...
		for (int w = 0; w < width; w++) {
			for (int h = 0; h < height; h++) {

				// We're going to sum up 4 doubles independently using SSE
				__m128d sums_lo = _mm_setzero_pd();
				__m128d sums_hi = _mm_setzero_pd();

				// For every pixel in the input kernel...
				for (int x = 0; x < kernel_order; x++) {
					for (int y = 0; y < kernel_order; y++) {

						// As nchannels is divisible by 32, we can optimise this loop to operate on 32 channels together
						for (int c = 0; c < nchannels; c += 32) {

							// We will process 4 channels at once using SSE. As such, we need to perform 32/4=8 iterations.
							// Again, we tell GCC to unroll this fixed-sized loop.
							#pragma GCC unroll 8
							for (int s = 0; s < 8; s++) {

								// We process 4 channels at once using SSE intrinsics. We can load both the image and kernel
								// data directly as both are stored contiguously in memory thanks to our previous transformation.
								__m128 img4 = _mm_loadu_ps(&image[w+x][h+y][c + s * 4]);
								__m128 krn4 = _mm_load_ps(&kernel[x][y][c + s * 4]);
								__m128 mul4 = _mm_mul_ps(img4, krn4);

								// Add the 4 float results to the 4 double sums
								sums_lo = _mm_add_pd(sums_lo, _mm_cvtps_pd(mul4));
								sums_hi = _mm_add_pd(sums_hi, _mm_cvtps_pd(_mm_movehl_ps(mul4, mul4)));
							}
						}
					}
				}

				// Sum the 4 double sums and write as a float to output
				__m128d sums = _mm_add_pd(sums_lo, sums_hi);
				__m128d sum = _mm_hadd_pd(sums, sums);
				_mm_store_ss(&output[m][w][h], _mm_cvtpd_ps(sum));
			}
		}
	}
}

int main(int argc, char **argv) {
	float ***image;                     //float image[W][H][C];
	int16_t ****kernels;                //float kernels[M][C][K][K];
	float ***control_output, ***output; //float output[M][W][H];
	long long david_mul_time, student_mul_time;
	int width, height, kernel_order, nchannels, nkernels;
	struct timeval start_time;
	struct timeval stop_time;

	if (argc != 6) {
		fprintf(stderr, "Usage: conv-harness <image_width> <image_height> <kernel_order> <number of channels> <number of kernels>\n");
		exit(1);
	} else {
		width = atoi(argv[1]);
		height = atoi(argv[2]);
		kernel_order = atoi(argv[3]);
		nchannels = atoi(argv[4]);
		nkernels = atoi(argv[5]);
	}
	switch (kernel_order) {
		case 1: case 3: case 5: case 7:
			break;
		default:
			fprintf(stderr, "FATAL: kernel_order must be 1, 3, 5 or 7, not %d\n", kernel_order);
			exit(1);
	}

	/* allocate the matrices */
	image = gen_random_3d_matrix_float(width + kernel_order, height + kernel_order, nchannels);
	kernels = gen_random_4d_matrix_int16(nkernels, nchannels, kernel_order, kernel_order);
	output = new_empty_3d_matrix_float(nkernels, width, height);
	control_output = new_empty_3d_matrix_float(nkernels, width, height);
	//DEBUGGING(write_out(A, a_dim1, a_dim2));

	printf("Running...\n");

	/* use a simple multichannel convolution routine to produce control result */
	gettimeofday(&start_time, NULL);
	multichannel_conv(image, kernels, control_output, width, height, nchannels, nkernels, kernel_order);
	gettimeofday(&stop_time, NULL);
	david_mul_time = (stop_time.tv_sec - start_time.tv_sec) * 1000000L + (stop_time.tv_usec - start_time.tv_usec);
	printf("David conv time:   %lld microseconds\n", david_mul_time);
	DEBUGGING(write_out(control_output, nkernels, width, height));

	/* perform student's multichannel convolution */
	gettimeofday(&start_time, NULL);
	student_conv(image, kernels, output, width, height, nchannels, nkernels, kernel_order);
	gettimeofday(&stop_time, NULL);
	student_mul_time = (stop_time.tv_sec - start_time.tv_sec) * 1000000L + (stop_time.tv_usec - start_time.tv_usec);
	printf("Student conv time: %lld microseconds\n", student_mul_time);
	DEBUGGING(write_out(output, nkernels, width, height));

	printf("Performance difference: %f\n", david_mul_time / (double) student_mul_time);

	/* now check that the student's multichannel convolution routine gives the same answer as the known working version */
	check_result(output, control_output, nkernels, width, height);

	return 0;
}
