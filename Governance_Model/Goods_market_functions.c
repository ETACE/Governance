#include "header.h"
#include "Goods_Market_agent_header.h"
#include "my_library_header.h"
#include <time.h>

/* Init functio
*/
int Goods_market_init(){

	//Here we set the pessimists' expected price effect of buybacks to 2/3 of that of optimists
	FLAME_environment_variable_const_impact_buybacks_shareprice_pessimists = (-1)*0.66666667*FLAME_environment_variable_const_impact_buybacks_shareprice_optimists;
	


return 0;
}


/* Dertermine certain variables
*/
int Goods_market_update(){

	//New industry wide wage level: linked to productivity
	WAGE_LEVEL = AVERAGE_PRODUCTIVITY*CONST_WAGE_ADJUSTMENT;

	AV_PRODUCTIVITY_GROWTH=0.0;

	// Change in market size according to an AR1 process: not used 
	EPSILON_MARKET_SIZE = CONST_AUTOCORR_MARKET_SIZE* EPSILON_MARKET_SIZE + normal_distributed_double()* CONST_STD_MARKET_SIZE;


	MARKET_SIZE = CONST_MARKET_SIZE + EPSILON_MARKET_SIZE;


	if(AVERAGE_PRODUCTIVITIES.size>1){

		AV_PRODUCTIVITY_GROWTH = (AVERAGE_PRODUCTIVITIES.array[AVERAGE_PRODUCTIVITIES.size-1] - AVERAGE_PRODUCTIVITIES.array[0])/ AVERAGE_PRODUCTIVITIES.array[0];

		AV_PRODUCTIVITY_GROWTH = AV_PRODUCTIVITY_GROWTH/(AVERAGE_PRODUCTIVITIES.size-1);
	}
	//send info to firms
	add_market_info_message(MARKET_SIZE, WAGE_LEVEL, AV_PRODUCTIVITY_GROWTH);

	return 0;
}

/*Comnpute a range of market and firm statistics. This done here to save disk space and to speed up post-simulation data processing*/
int Goods_market_compute_statistic(){

	int counter = 0;

	AVERAGE_PRODUCTIVITY = 0.0;
	AVERAGE_PRODUCTIVITY_WEIGHTED = 0.0;
	SUM_DIVIDENDS = 0.0;
	TOTAL_OUTPUT = 0.0;
	HERFINDAHL_INDEX = 0.0;
	SUM_REAL_INVESTMENTS = 0.0;
	SUM_BUYBACKS = 0.0;
	AV_REAL_INVESTMENTS = 0.0;
	AV_BUYBACKS = 0.0;
	AV_SHARE_REAL_INVESTMENT= 0.0;
	AV_SHARE_PRICE= 0.0;
	NO_SHARES_OUTSTANDING= 0.0;
	FRONTIER = 0;
	MEAN_SQUARED_RANKS = 0.0;

	// Read data from firms
	START_OUTPUT_INFO_MESSAGE_LOOP

	counter++;

	TOTAL_OUTPUT+= output_info_message->output;
	AVERAGE_PRODUCTIVITY_WEIGHTED += (output_info_message->output*output_info_message->productivity);
	AVERAGE_PRODUCTIVITY += output_info_message->productivity;
	SUM_REAL_INVESTMENTS += output_info_message->real_investment;
	SUM_BUYBACKS += output_info_message->buyback;
	AV_REAL_INVESTMENTS += output_info_message->real_investment;
	AV_BUYBACKS += output_info_message->buyback;
	SUM_DIVIDENDS += output_info_message->dividends;
	AV_SHARE_PRICE += output_info_message->share_price;
	NO_SHARES_OUTSTANDING += output_info_message->no_shares_outstanding;

	if(output_info_message->real_investment + output_info_message->buyback>0)
    		AV_SHARE_REAL_INVESTMENT += output_info_message->real_investment/(output_info_message->real_investment + output_info_message->buyback);

	if(output_info_message->productivity> FRONTIER)
    		FRONTIER = output_info_message->productivity;


	FINISH_OUTPUT_INFO_MESSAGE_LOOP

	START_OUTPUT_INFO_MESSAGE_LOOP

	//Compute Herfindahl
	HERFINDAHL_INDEX += pow(output_info_message->output/TOTAL_OUTPUT ,2);


	FINISH_OUTPUT_INFO_MESSAGE_LOOP

	//Store the last 200 values of the HHI
	if(ARR_HERFINDAHL_INDEX.size == 200){

		remove_double(&ARR_HERFINDAHL_INDEX,0);

	}

	add_double(&ARR_HERFINDAHL_INDEX,HERFINDAHL_INDEX);

	AV_HERFINDAHL_INDEX = 0.0;

	for(int i=0; i < min(ARR_HERFINDAHL_INDEX.size-1,200); i++){

		AV_HERFINDAHL_INDEX += ARR_HERFINDAHL_INDEX.array[ARR_HERFINDAHL_INDEX.size-1-i];

	}

	if( min(ARR_HERFINDAHL_INDEX.size-1,200)>0)
		AV_HERFINDAHL_INDEX = AV_HERFINDAHL_INDEX / min(ARR_HERFINDAHL_INDEX.size-1,200);


	int cnt = 0;

	START_RANK_INFO_MESSAGE_LOOP

	MEAN_SQUARED_RANKS += rank_info_message->mean_squared_dev;
	cnt++;

	FINISH_RANK_INFO_MESSAGE_LOOP

	if(cnt>0)
		MEAN_SQUARED_RANKS = MEAN_SQUARED_RANKS/(1.0*cnt);

	AVERAGE_PRODUCTIVITY_WEIGHTED = AVERAGE_PRODUCTIVITY_WEIGHTED/ TOTAL_OUTPUT;
	AVERAGE_PRODUCTIVITY = AVERAGE_PRODUCTIVITY/ counter;

	if(ARR_AV_PRODUCTIVITY.size==500)
		remove_double(&ARR_AV_PRODUCTIVITY, 0);

	if(ARR_WEIGHTED_PRODUCTIVITY.size==500)
		remove_double(&ARR_WEIGHTED_PRODUCTIVITY, 0);


	  add_double(&ARR_AV_PRODUCTIVITY, AVERAGE_PRODUCTIVITY);

	  add_double(&ARR_WEIGHTED_PRODUCTIVITY, AVERAGE_PRODUCTIVITY_WEIGHTED);



	  AV_ANNUAL_PRODUCTIVITY_GROWTH_500 = pow((ARR_AV_PRODUCTIVITY.array[ARR_AV_PRODUCTIVITY.size-1]/ARR_AV_PRODUCTIVITY.array[0]),(4.0/((ARR_AV_PRODUCTIVITY.size))))-1;
	  WEIGHTED_ANNUAL_PRODUCTIVITY_GROWTH_500 = pow((ARR_WEIGHTED_PRODUCTIVITY.array[ARR_WEIGHTED_PRODUCTIVITY.size-1]/ARR_WEIGHTED_PRODUCTIVITY.array[0]),(4.0/((ARR_AV_PRODUCTIVITY.size))))-1;

	 //Compute averages
	  AV_REAL_INVESTMENTS = AV_REAL_INVESTMENTS/counter;
	  AV_BUYBACKS = AV_BUYBACKS/counter;
	  AV_SHARE_REAL_INVESTMENT= AV_SHARE_REAL_INVESTMENT/counter;
	  AV_SHARE_PRICE= AV_SHARE_PRICE/counter;
	  NO_SHARES_OUTSTANDING= NO_SHARES_OUTSTANDING/counter;
	  AV_PRODUCTIVITY_GAP =  AVERAGE_PRODUCTIVITY /FRONTIER;
	  RATIO_BUYBACKS_DIVIDENDS = SUM_BUYBACKS / SUM_DIVIDENDS;


	  if(DAY==CONST_TRANSITION_PHASE){

		  AV_PRODUCTIVITY_START = AVERAGE_PRODUCTIVITY_WEIGHTED;
		  AV_PRODUCTIVITY_WEIGHTED_START = AVERAGE_PRODUCTIVITY_WEIGHTED;
		  FRONTIER_START = FRONTIER;

	  }

	  if(DAY>CONST_TRANSITION_PHASE){
		  ANNUAL_GROWTH_PRODUCTIVITY = pow((AVERAGE_PRODUCTIVITY/AV_PRODUCTIVITY_START),(4.0/((DAY-CONST_TRANSITION_PHASE))))-1;
		  ANNUAL_GROWTH_WEIGHTED_PRODUCTIVITY = pow((AVERAGE_PRODUCTIVITY_WEIGHTED/AV_PRODUCTIVITY_WEIGHTED_START),(4.0/((DAY-CONST_TRANSITION_PHASE))))-1;
		  ANNUAL_GROWTH_FRONTIER = pow((FRONTIER/FRONTIER_START),(4.0/((DAY-CONST_TRANSITION_PHASE))))-1;
	  }

	  if(SUM_BUYBACKS >1e-6)
		  RATIO_REAL_INVESTMENT_BUYBACKS = SUM_REAL_INVESTMENTS / SUM_BUYBACKS;
	  else
		  RATIO_REAL_INVESTMENT_BUYBACKS = 1.0;

	  AV_FRACTION_BONUS_INCOME = 0.0;
	  AV_FRACTION_SHARE_INCOME = 0.0;

	  int n = 0;
	  START_INCOME_INFO_MESSAGE_LOOP

	  	  	  if(income_info_message->total_income>0){

	  	  		 n++;
	  	  		 AV_FRACTION_SHARE_INCOME += income_info_message-> share_based_income/income_info_message->total_income;
	  	  		 AV_FRACTION_BONUS_INCOME += income_info_message-> bonus_income/income_info_message->total_income;

	  	  	  }

	  FINISH_INCOME_INFO_MESSAGE_LOOP

	  if(n>0){
		  AV_FRACTION_BONUS_INCOME = AV_FRACTION_BONUS_INCOME / n;
	 	  AV_FRACTION_SHARE_INCOME = AV_FRACTION_SHARE_INCOME / n;

	  }

	  if(AVERAGE_PRODUCTIVITIES.size==10)
		remove_double(&AVERAGE_PRODUCTIVITIES,0);


	  if(DAY ==19500){

	  		NO_SHARES_OUTSTANDING_500 = NO_SHARES_OUTSTANDING;

	  	}

	add_double(&AVERAGE_PRODUCTIVITIES, AVERAGE_PRODUCTIVITY);


	//printf("HERFINDAHL_INDEX %f",HERFINDAHL_INDEX);

	return 0;
}
