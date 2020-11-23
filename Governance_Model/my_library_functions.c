/**
 * \file  my_library_functions.c
 * \brief Holds user created library functions.
 */
#include <gsl/gsl_rng.h>
#include <gsl/gsl_multifit.h>
#include "header.h"
#include "my_library_header.h"

#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>

extern gsl_rng * FLAME_GSL_RNG;  /* global GSL random generator */

int FLAME_GSL_GEN_NO;

/** \fn int random_int(int min,int max)
 * \brief Returns an integer between and including min and max
 * \param min The minimum integer.
 * \param max The maximum integer.
 * \return The random integer.
 */
int random_int(int min,int max)
{
FLAME_GSL_GEN_NO++;
	double random_number = gsl_rng_uniform(FLAME_GSL_RNG)*(max - min + 1) + min;
	return (int)random_number;
    //return min + rand() % (max - min + 1);
}

double max(double a, double b)
{
    return (a >= b) ? a : b; 
}

double min(double a, double b)
{
    return (a <= b) ? a : b; 
}


/** \fn double random_unif()
 * \brief Uniformly distributed random numbers, chosen from
 *   a uniform distribution on the closed interval (0.0,1.0).
 */
double random_unif()
{
	FLAME_GSL_GEN_NO++;

    return gsl_rng_uniform(FLAME_GSL_RNG);
    //return ((double)rand()/(double)RAND_MAX);
}

/** \fn double random_unif_interval(double a, double b)
 * \brief Uniformly distributed random numbers, chosen from
 *   a uniform distribution on the closed interval (a,b).
 */
double random_unif_interval(double a, double b)
{
	return (a + (b-a)*random_unif());
}
/** \fn round_double_to_int(double x)
 * \brief Rounds a double to the closest integer.
 */
int round_double_to_int(double x)
{
  if(x>0) return (int)(x + 0.5);

  return (int)(x - 0.5);
}

double abs_double(double x)
{
  if(x<0) return ((-1)*x);

  return x;
}

/** \fn normal_distributed_double()
 * \brief returns a standard normal distributed random number (Polar method)
 */
double normal_distributed_double()
{
	double x_1,x_2;
	double q=1.1;	
	while(q>1-1e-6)
	{
	x_1 = random_unif_interval(0, 2) - 1;

	x_2 = random_unif_interval(0, 2) - 1;

	q = pow(x_1,2) + pow(x_2,2);
	}

	double p = pow(-2*log(q)/q,0.5)	;

	x_1 = p * x_1; 

	return x_1;

}

int compare (const void * a, const void * b){
	if (*(double*)a > *(double*)b) return 1;
	  else return -1;

}



void return_quantile(double_array data, double_array * quantiles, double_array quantiles_pop){
	int i,j,size,index;

	double *data1, sum;
	int *quantile_index;


	size = data.size;

	/*define temp (static) arrays of required size*/

	/*For the sorted data*/
	data1 = malloc(size*sizeof(double));
	/*For the quantiles*/
	quantile_index = malloc(((* quantiles ).size)*sizeof(int));

	for(i=0; i< data.size;i++){

		data1[i] = data.array[i];


	}



/*Sort ascending*/
  qsort (data1, data.size, sizeof(double), compare);

  double temp;

  /*Determine the positions of the quantiles -> array of indexes*/

  for(i=0; i < quantiles_pop.size;i++ ){

	  temp=quantiles_pop.array[i]*(data.size-1);

	  quantile_index[i]= floor(temp);


  }

  /*Determine data values for each quantile and return them*/

  sum = 0.0;

  index=0;



	  for(j=0; j< size;j++){

		  sum += data1[j];

		  if(quantile_index[index]==j){

			  (* quantiles ).array[index] =  sum;
			  index++;
		  }

	  }




 /* for(i=0; i < (* quantiles ).size;i++ ){
    (* quantiles ).array[i] =  (* quantiles ).array[i] / sum;

    }
*/


  for(i=0; i < (* quantiles ).size;i++ ){
  (* quantiles ).array[i] =  data1[quantile_index[i]];

  }


  free(data1);
  free(quantile_index);

}


double dicky_fuller_teststatistic_augmented(double_array data){

	double test_statistic = 0.0;

	double  chisq;

	gsl_multifit_linear_workspace * linear_model;


	gsl_vector * y_vector;

	gsl_vector * coeff_vector;

	gsl_matrix * x_matrix;

	gsl_matrix * cov_matrix;

	y_vector =   gsl_vector_alloc(data.size - 1);

	coeff_vector =  gsl_vector_alloc(9);


	x_matrix = gsl_matrix_alloc( data.size - 1, 9);

	cov_matrix = gsl_matrix_alloc(9, 9);

	//gsl_vector x_vector;
	//gsl_vector_alloc(&x_vector);


	for(int i = 7; i < data.size-1; i++){

	double diff = data.array[i+1] - data.array[ i];
	double diff_lag1 = data.array[i] - data.array[ i-1];
	double diff_lag2 = data.array[i-1] - data.array[ i-2];
	double diff_lag3 = data.array[i-2] - data.array[ i-3];
	double diff_lag4 = data.array[i-3] - data.array[ i-4];
	double diff_lag5 = data.array[i-4] - data.array[ i-5];
	double diff_lag6 = data.array[i-5] - data.array[ i-6];

		gsl_vector_set(y_vector, i, diff);

		gsl_matrix_set(x_matrix, i,0, 1);
		gsl_matrix_set(x_matrix, i,1, 0); // set time effect to zero. Old: gsl_matrix_set(x_matrix, i,1, 0);
		gsl_matrix_set(x_matrix, i,2, data.array[ i]);
		gsl_matrix_set(x_matrix, i,3, diff_lag1);
		gsl_matrix_set(x_matrix, i,4, diff_lag2);
		gsl_matrix_set(x_matrix, i,5, diff_lag3);
		gsl_matrix_set(x_matrix, i,6, diff_lag4);
		gsl_matrix_set(x_matrix, i,7, diff_lag5);
		gsl_matrix_set(x_matrix, i,8, diff_lag6);

	}



	linear_model =  gsl_multifit_linear_alloc(data.size-1, 9);

	//gsl_multifit_linear_svd(x_matrix , linear_model);


	int status = gsl_multifit_linear(x_matrix, y_vector, coeff_vector, cov_matrix, &chisq, linear_model);

	printf("error: %s \n", gsl_strerror(status));



	double coeff2 = gsl_vector_get(coeff_vector, (2));

	double sd2 = sqrt(gsl_matrix_get(cov_matrix,2,2));


	printf("beta %f    sd %f  \n",coeff2,sd2);


	if(sd2!=sd2){


	for(int i=0; i< data.size;i++)
		printf("%f   \n ", data.array[i]);


		printf("\n");

	}


	test_statistic = coeff2 / sd2;


	gsl_vector_free(y_vector);
	gsl_vector_free(coeff_vector);
	gsl_matrix_free(x_matrix);
	gsl_matrix_free(cov_matrix);
	gsl_multifit_linear_free(linear_model);


	return test_statistic;
}








double dicky_fuller_teststatistic(double_array data){

	double test_statistic = 0.0;

	double  chisq;

	gsl_multifit_linear_workspace * linear_model;


	gsl_vector * y_vector;

	gsl_vector * coeff_vector;

	gsl_matrix * x_matrix;

	gsl_matrix * cov_matrix;

	y_vector =   gsl_vector_alloc(data.size - 1);

	coeff_vector =  gsl_vector_alloc(2);


	x_matrix = gsl_matrix_alloc( data.size - 1, 2);

	cov_matrix = gsl_matrix_alloc(2, 2);

	//gsl_vector x_vector;
	//gsl_vector_alloc(&x_vector);


	for(int i = 0; i < data.size-1; i++){

	double diff = data.array[i+1] - data.array[ i];


		gsl_vector_set(y_vector, i, diff);

		gsl_matrix_set(x_matrix, i,0, 1);
		gsl_matrix_set(x_matrix, i,1, data.array[ i]);

	}



	linear_model =  gsl_multifit_linear_alloc(data.size-1, 2);

	//gsl_multifit_linear_svd(x_matrix , linear_model);


	int status = gsl_multifit_linear(x_matrix, y_vector, coeff_vector, cov_matrix, &chisq, linear_model);

	printf("error: %s \n", gsl_strerror(status));



	double coeff2 = gsl_vector_get(coeff_vector, (1));

	double sd2 = sqrt(gsl_matrix_get(cov_matrix,1,1));

	test_statistic = coeff2 / sd2;


	gsl_vector_free(y_vector);
	gsl_vector_free(coeff_vector);
	gsl_matrix_free(x_matrix);
	gsl_matrix_free(cov_matrix);
	gsl_multifit_linear_free(linear_model);


	return test_statistic;
}

