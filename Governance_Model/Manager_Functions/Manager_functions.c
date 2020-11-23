
#include "../header.h"
#include "../Manager_agent_header.h"
#include "../my_library_header.h"
#include "Manager_aux_headers.h"

#define EPSILON 1e-12	//epsilon small for checking div zero
#define MAXI 1e+9
#include <time.h>
/*
* Function executed at launch to set up specific memory variables
*/

int Manager_init(){
 

	TAX_RATE_CORPORATE = CONST_GENERAL_TAX_RATE;
	TAX_RATE_INCOME = CONST_GENERAL_TAX_RATE;
	MANAGER_FIX_INCOME  = CONST_MANAGER_FIX_INCOME;
	NO_OUTSTANDING_SHARES = CONST_INITIAL_NUMBER_OF_SHARES;
	FRACTION_SHARES_LONGTERM_INVESTORS = CONST_SHARES_LONGTERMISTS;
	NO_SHARES_HOLD_BY_LONGTERM_INVESTORS = FRACTION_SHARES_LONGTERM_INVESTORS*NO_OUTSTANDING_SHARES;
	SHORT_TERM_CAPITAL_GAIN_TAX_RATE = CONST_GENERAL_TAX_RATE;
	LONG_TERM_CAPITAL_GAIN_TAX_RATE = CONST_GENERAL_TAX_RATE;
	SHORT_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE = CONST_GENERAL_TAX_RATE;
	LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE = CONST_GENERAL_TAX_RATE;
	MARGINAL_COSTS = WAGE / PRODUCTIVITY;
	PRICE = (1+CONST_MARK_UP)*MARGINAL_COSTS;
	INFLUENCE_MANAGER = CONST_INFLUENCE_MANAGER;
	INFLUENCE_LONGTERM_INVESTORS =  (1- CONST_INFLUENCE_MANAGER)*FRACTION_SHARES_LONGTERM_INVESTORS;
	INFLUENCE_SHORTTERM_INVESTORS =  (1- CONST_INFLUENCE_MANAGER)*(1-FRACTION_SHARES_LONGTERM_INVESTORS);

	add_init_info_message(ID, PRICE, PRODUCTIVITY);


	return 0;

}


/*
* Function executed to initialize competitors in the internal memory
*/

int Manager_init2(){


	START_INIT_INFO_MESSAGE_LOOP

		add_competitor(&COMPETITORS,init_info_message->id, init_info_message->price,init_info_message->productivity);

	FINISH_INIT_INFO_MESSAGE_LOOP

	SHARE_PRICE = Manager_determine_initial_shareprice();

	SHARE_PRICE_HISTORY[0] = SHARE_PRICE;
	SHARE_PRICE_HISTORY[1] = SHARE_PRICE;

	FUNDAMENTAL_VALUE = SHARE_PRICE;




	return 0;
}


/*
* Function to determine the current dividends
*/
int Manager_set_dividend(){


	//Compute dividends as pct from the liquid assets
	TOTAL_DIVIDENDS = SAVINGS * CONST_DIVIDEND_RATE;
	DIVIDEND_PER_SHARE = TOTAL_DIVIDENDS / NO_OUTSTANDING_SHARES;

	//Adjust payment account
	SAVINGS -= TOTAL_DIVIDENDS;


	if(DIVIDEND_LONG_HISTORY.size ==  HISTORY_TIME_WINDOW){

		remove_double(&DIVIDEND_LONG_HISTORY,0);

	}

	add_double(&DIVIDEND_LONG_HISTORY,DIVIDEND_PER_SHARE);

	return 0;
}

/*
* Function to set variables at the beginning of each period
*/

int Manager_set_variables(){


	double average_productivity = 0.0;
	TECHNOLOGICAL_FRONTIER = 0.0;

	for(int i=0; i < COMPETITORS.size; i++ ){

		average_productivity +=  COMPETITORS.array[i].productivity;

		if( COMPETITORS.array[i].productivity > TECHNOLOGICAL_FRONTIER)
			TECHNOLOGICAL_FRONTIER = COMPETITORS.array[i].productivity;


	}


	SHARES_AS_COMPENSATION_PER_PERIOD = CONST_SHARES_AS_COMPENSATION_PER_PERIOD;


	if(COMPETITORS.size>0){

		average_productivity = average_productivity/COMPETITORS.size;

	}else{

		average_productivity = 1.0;

	}

	if(FRONTIER_HISTORY.size >= CONST_PATENT_LENGTH){

		remove_double(&FRONTIER_HISTORY,0);


	}

	add_double(&FRONTIER_HISTORY,TECHNOLOGICAL_FRONTIER);


	//TECH_PROGRESS_GROWTH_RATE = CONST_TECH_PROGRESS_GROWTH_RATE * pow(PRODUCTIVITY / average_productivity, CONST_RETURNS_TO_SCALE_INNOVATION);

	TECH_PROGRESS_GROWTH_RATE = CONST_TECH_PROGRESS_GROWTH_RATE * max(1,pow(FRONTIER_HISTORY.array[0]/PRODUCTIVITY  ,CONST_RETURNS_TO_SCALE_INNOVATION));



	CARA_VARIANCE =   pow(CONST_CARA_STANDARD_DEVIATION * DIVIDEND_PER_SHARE,2);

	double dividend_expectations_t_minus_1 = DIVIDEND_EXPECTATIONS;

	DIVIDEND_EXPECTATIONS =   (1-CONST_DIVIDEND_EXPECTATIONS)* dividend_expectations_t_minus_1 +   CONST_DIVIDEND_EXPECTATIONS*DIVIDEND_PER_SHARE;

	double q = 1/(1+CONST_INTEREST_RATE);

	double dv = (q)/(1- q)*DIVIDEND_PER_SHARE;




	//FUNDAMENTAL_VALUE = 0.01*dv + (1-0.01)*FUNDAMENTAL_VALUE;

	double cara_coeff = CONST_CARA_PARAMETER/((double) NO_SHORTTERM_INVESTORS);

	FUNDAMENTAL_VALUE =  (DIVIDEND_EXPECTATIONS*(1-SHORT_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE) + FUNDAMENTAL_VALUE*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE) - cara_coeff*CARA_VARIANCE * (1-FRACTION_SHARES_LONGTERM_INVESTORS)*NO_OUTSTANDING_SHARES)/(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME));


	double g_share_price = 0.0;
	double g_dividend = 0.0;


	//Possibility to adjust the fraction of shares held by LTI; parameter ist set to 0 by default
	if(HISTORY_TIME_WINDOW == DIVIDEND_LONG_HISTORY.size && DAY > CONST_TRANSITION_PHASE){


		g_share_price = (SHARE_PRICE_LONG_HISTORY.array[SHARE_PRICE_LONG_HISTORY.size -1] - SHARE_PRICE_LONG_HISTORY.array[0])/SHARE_PRICE_LONG_HISTORY.array[0];

		g_share_price = g_share_price / SHARE_PRICE_LONG_HISTORY.size;

		g_dividend = (DIVIDEND_LONG_HISTORY.array[DIVIDEND_LONG_HISTORY.size -1] - DIVIDEND_LONG_HISTORY.array[0])/DIVIDEND_LONG_HISTORY.array[0];

		g_dividend = g_dividend /DIVIDEND_LONG_HISTORY.size;

		FRACTION_SHARES_LONGTERM_INVESTORS = min(1.0,max(0.0,FRACTION_SHARES_LONGTERM_INVESTORS -  CONST_ADJUST_SHARE_LONGTERMISTS*(g_share_price - g_dividend )));

	}



	INFLUENCE_LONGTERM_INVESTORS =  (1- CONST_INFLUENCE_MANAGER)*FRACTION_SHARES_LONGTERM_INVESTORS;
	INFLUENCE_SHORTTERM_INVESTORS =  (1- CONST_INFLUENCE_MANAGER)*(1-FRACTION_SHARES_LONGTERM_INVESTORS);


	//RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE = SHARE_PRICE / FUNDAMENTAL_VALUE;
	RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE = g_share_price - g_dividend;


	return 0;
}



/*
* Function to set the optimal real investments and buybacks
*/

int Manager_set_buyback_and_real_investment(){


		double budget_constraint_old = BUDGET_CONSTRAINT;
		double real_investment_old = REAL_INVESTMENT;

		double delta_share_price = (SHARE_PRICE_HISTORY[0] - SHARE_PRICE_HISTORY[1])/SHARE_PRICE_HISTORY[1];


		//This is to delete the firm in case the firm cannot pay the fixed costs
		if(SAVINGS < MANAGER_FIX_INCOME)
			return 1;
		
		BUDGET_CONSTRAINT = max(0.0,SAVINGS - MANAGER_FIX_INCOME);



		double cara_coeff = CONST_CARA_PARAMETER/((double) NO_SHORTTERM_INVESTORS);

		//double growth_expectation= (1+FRACTION_OPTIMISTS*max(0,delta_share_price) + (1-FRACTION_OPTIMISTS)*min(0,delta_share_price));
		//double growth_expectation= max(1,1 + delta_share_price);

		double growth_expectation = 1.0;

		
		X1 = DIVIDEND_EXPECTATIONS*(1-SHORT_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE) + SHARE_PRICE*growth_expectation*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE) - cara_coeff*CARA_VARIANCE * (1-FRACTION_SHARES_LONGTERM_INVESTORS)*NO_OUTSTANDING_SHARES;

		X1 = max(0,X1);

		X2 = (FRACTION_OPTIMISTS*(1-SHORT_TERM_CAPITAL_GAIN_TAX_RATE)*CONST_IMPACT_BUYBACKS_SHAREPRICE_OPTIMISTS+(1-FRACTION_OPTIMISTS)*CONST_IMPACT_BUYBACKS_SHAREPRICE_PESSIMISTS )*SHARE_PRICE /(NO_OUTSTANDING_SHARES) + cara_coeff*(1-FRACTION_SHARES_LONGTERM_INVESTORS)*CARA_VARIANCE ;

		if(SALES > 0.0){

		if(DAY > 100  && BUDGET_CONSTRAINT > 1e-10){

			//Adjust the productivity by the expected productivity growth
			for(int i=0; i < COMPETITORS.size;i++){

				COMPETITORS.array[i].productivity= (1+AV_PRODUCTIVITY_GROWTH)*COMPETITORS.array[i].productivity;

			}


			// Determine $\delta \Pi$ which is the expected change in profits due to the innovation
			double add_profit_inno = Manager_profit_function((1+TECH_PROGRESS_GROWTH_RATE) * PRODUCTIVITY) - Manager_profit_function(PRODUCTIVITY);

			//Determine the budget constraint


			// Determine maximum investment: if all savings flow into r&d


			double max_investment = max(0.0, BUDGET_CONSTRAINT);

			//Find the root of the difference of the marginal value of buybacks and real investments
			double optimal_investment = Manager_implicit_investment_function_root_solver( add_profit_inno, X1, X2, BUDGET_CONSTRAINT);




			//The real investment is finally determined
			REAL_INVESTMENT = max(0, min(max_investment, optimal_investment));

			//Buybacks is
			BUYBACK = BUDGET_CONSTRAINT - REAL_INVESTMENT;


		}else{

			REAL_INVESTMENT = BUDGET_CONSTRAINT;
			BUYBACK = 0.0;


		}



		SUBSTITUTION_EFFECT = 0.0;
		RESOURCE_EFFECT = 0.0;
			
			
		// Variables used for analysis
		if(budget_constraint_old >0  && BUDGET_CONSTRAINT>0 ){
			//SUBSTITUTION_EFFECT = (budget_constraint_old - REAL_INVESTMENT)/(BUDGET_CONSTRAINT - REAL_INVESTMENT);
			//RESOURCE_EFFECT = (BUDGET_CONSTRAINT - budget_constraint_old)/(BUDGET_CONSTRAINT - REAL_INVESTMENT);

			SUBSTITUTION_EFFECT = (REAL_INVESTMENT/budget_constraint_old) - (real_investment_old / budget_constraint_old);
			RESOURCE_EFFECT = (REAL_INVESTMENT / BUDGET_CONSTRAINT) - (REAL_INVESTMENT/budget_constraint_old);
		}


		RD_INTENSITY = REAL_INVESTMENT/SALES;

		}else{

			REAL_INVESTMENT = 0.0;
			BUYBACK = 0.0;


		}


return 0;
}

/*
* Function to set the financial market interactions
*/

int Manager_financial_market_interaction(){


	//Determine the share price after placing the buybacks on the market


		SHARE_PRICE_HISTORY[1] = SHARE_PRICE_HISTORY[0];

		//New share price
		SHARE_PRICE = (X1 + sqrt(pow(X1,2)+ 4*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME))*X2*BUYBACK))/(2*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME)));


		// Determine the number of bought back shares
		BOUGHT_BACK_SHARES = BUYBACK/SHARE_PRICE;

		// New number of oustanding shares
		NO_OUTSTANDING_SHARES = NO_OUTSTANDING_SHARES - BUYBACK/SHARE_PRICE;

		NO_SHARES_HOLD_BY_LONGTERM_INVESTORS = FRACTION_SHARES_LONGTERM_INVESTORS*NO_OUTSTANDING_SHARES;

		if(DAY>100){

			// Possibility to consider options instead of shares; not used in the paper
			if(SHARE_PRICE> SHARE_PRICE_HISTORY[1]){


				NO_OUTSTANDING_SHARES += SHARES_AS_COMPENSATION_PER_PERIOD;

				SAVINGS += SHARE_PRICE_HISTORY[1]*CONST_FRACTION_OPTIONS*SHARES_AS_COMPENSATION_PER_PERIOD;

				ISSUED_SHARES = SHARES_AS_COMPENSATION_PER_PERIOD;

			}else{

				NO_OUTSTANDING_SHARES += (1-CONST_FRACTION_OPTIONS)*SHARES_AS_COMPENSATION_PER_PERIOD;

				ISSUED_SHARES = (1-CONST_FRACTION_OPTIONS)*SHARES_AS_COMPENSATION_PER_PERIOD;

			}

		}






		SHARE_PRICE_HISTORY[0] = SHARE_PRICE;


		if(SHARE_PRICE_LONG_HISTORY.size ==  HISTORY_TIME_WINDOW){

			remove_double(&SHARE_PRICE_LONG_HISTORY,0);

		}

		add_double(&SHARE_PRICE_LONG_HISTORY,SHARE_PRICE);



		SAVINGS -= BUYBACK;


return 0;
}


/*
* Function to carry out R&/D activities
*/
int Manager_execute_real_investment(){



	SAVINGS -= REAL_INVESTMENT;

	if(SALES>0.0){
		INNOVTION_ARRIVAL_RATE = CONST_MAX_INNOVATION_PROBABILITY * (1 - exp((-1)*CONST_INNOVATION_INTENSITY*pow(REAL_INVESTMENT/SALES, CONST_POWER_PROBABILITY_FUNCTION) ) );


		EX_ANTE_EXPECTED_PRODUCTIVITY_GROWTH = INNOVTION_ARRIVAL_RATE*TECH_PROGRESS_GROWTH_RATE;

	//Draw uniform on [0,1]
	 double random_draw = random_unif();

	// printf("DAY %d ID %d  PRODUCTIVITY %f  INNOVTION_ARRIVAL_RATE %f TECH_PROGRESS_GROWTH_RATE %f   SHARE_PRICE %f   	No_SHARES %f\n",DAY,ID ,PRODUCTIVITY,INNOVTION_ARRIVAL_RATE, TECH_PROGRESS_GROWTH_RATE,SHARE_PRICE,NO_OUTSTANDING_SHARES);
	//If innovation draw is successful 
	if(random_draw < INNOVTION_ARRIVAL_RATE && DAY > CONST_TRANSITION_PHASE )
   		PRODUCTIVITY = (1+TECH_PROGRESS_GROWTH_RATE) * PRODUCTIVITY;



	 LOG_PRODUCTIVITY = log(PRODUCTIVITY);

}

return 0;
}

/*
* Function to set the new price 
*/
int Manager_set_price(){


	MARGINAL_COSTS = WAGE / PRODUCTIVITY;
	// Mark-up pricing
	PRICE = (1+CONST_MARK_UP)*MARGINAL_COSTS;

	add_competitor_info_message(ID, PRICE, PRODUCTIVITY,RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE);



return 0;
}


/*
* Function to collect data from the goods market
*/
int Manager_receive_goods_market_signals(){


	reset_competitor_array(&COMPETITORS);

	AVERAGE_RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE = 0.0;

	START_COMPETITOR_INFO_MESSAGE_LOOP

		add_competitor(&COMPETITORS,competitor_info_message->id, competitor_info_message->price,competitor_info_message->productivity);

	AVERAGE_RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE += competitor_info_message->ratio_share_price_fundamental;

	FINISH_COMPETITOR_INFO_MESSAGE_LOOP

	AVERAGE_RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE = AVERAGE_RATIO_SHARE_PRICE_FUNDAMENTAL_VALUE/COMPETITORS.size;

	START_MARKET_INFO_MESSAGE_LOOP

		MARKET_SIZE = market_info_message->market_size;

		WAGE = market_info_message->wage;

		AV_PRODUCTIVITY_GROWTH = market_info_message->av_productivity_growth;

	FINISH_MARKET_INFO_MESSAGE_LOOP


	//Determine rank

	

	if(CONST_SWITCH_COLLECT_RANKS){
	

		if(DAY > CONST_TRANSITION_PHASE){
			RANK_PRODUCTIVITY = return_rank(COMPETITORS);

			add_double(&RANK_PRODUCTIVITY_ARRAY, RANK_PRODUCTIVITY);


		}


	}


return 0;
}

/*
* Function to set the goods market interactions
*/

int Manager_goods_market_interaction(){


	OUTPUT = Manager_demand_function(PRICE, COMPETITORS, MARKET_SIZE, CONST_ELASTICITY_SUBSTITUTION);

	LABOUR_DEMAND = OUTPUT / PRODUCTIVITY;

	MARKET_PROFIT =  (PRICE - MARGINAL_COSTS) * OUTPUT;


	if(!CONST_INNOVATION_LINKED_RD_INTENSITY)
		SALES = 1.0;
	else
		SALES = PRICE*OUTPUT;

	SAVINGS += MARKET_PROFIT;

	OPERATING_PROFIT = MARKET_PROFIT - MANAGER_FIX_INCOME;

	add_output_info_message(ID, OUTPUT, PRODUCTIVITY, REAL_INVESTMENT, BUYBACK, SHARE_PRICE, NO_OUTSTANDING_SHARES,TOTAL_DIVIDENDS);




	if(CONST_SWITCH_COLLECT_RANKS){
		if(DAY ==10000){

			double av_rank= 0.0;
			double mean_squared_dev = 0.0;

			for(int i=0; i < RANK_PRODUCTIVITY_ARRAY.size; i++){

				av_rank += RANK_PRODUCTIVITY_ARRAY.array[i];

			}

			av_rank = av_rank/(RANK_PRODUCTIVITY_ARRAY.size);


			for(int i=0; i < RANK_PRODUCTIVITY_ARRAY.size; i++){

				mean_squared_dev += pow(RANK_PRODUCTIVITY_ARRAY.array[i] - av_rank,2);

			}

			mean_squared_dev = mean_squared_dev/(RANK_PRODUCTIVITY_ARRAY.size);


			add_rank_info_message(ID,mean_squared_dev);



		}


	}


return 0;
}

/*
* Function to set the tax payment
*/
int Manager_tax_accounting(){

	TAX_PAYMENT_CORPORATE = max(0,TAX_RATE_CORPORATE*(OPERATING_PROFIT - CONST_TAX_DEDUCTION_INVESTMENTS*REAL_INVESTMENT));

	SAVINGS -= TAX_PAYMENT_CORPORATE;

return 0;
}

/*
* Function to set and payout the manager's remunertaion
*/
int Manager_realize_remuneration(){



	double fixed_income = min(SAVINGS, MANAGER_FIX_INCOME);

	MANAGER_INCOME_BONUS = CONST_MANAGER_BONUS_PCT * max(0,MARKET_PROFIT - TAX_PAYMENT_CORPORATE);

	//MANAGER_INCOME_SHARE_BASAED = CONST_SHARES_AS_COMPENSATION_PER_PERIOD * SHARE_PRICE;


	MANAGER_INCOME_SHARE_BASAED = CONST_FRACTION_OPTIONS*CONST_SHARES_AS_COMPENSATION_PER_PERIOD*max(0,SHARE_PRICE_HISTORY[0] - SHARE_PRICE_HISTORY[1]) + (1-CONST_FRACTION_OPTIONS)*CONST_SHARES_AS_COMPENSATION_PER_PERIOD * SHARE_PRICE;


	MANAGER_TOTAL_INCOME =    fixed_income + MANAGER_INCOME_SHARE_BASAED + MANAGER_INCOME_BONUS;

	MANAGER_NET_INCOME = (1- TAX_RATE_INCOME)* MANAGER_TOTAL_INCOME;


	SAVINGS -= (MANAGER_INCOME_BONUS + fixed_income);

	add_income_info_message(ID,MANAGER_TOTAL_INCOME, MANAGER_INCOME_SHARE_BASAED, MANAGER_INCOME_BONUS);



return 0;
}

/*
* Function transition of savings in the next period; receive interests
*/

int Manager_update_saving(){




	if(REAL_INVESTMENT + BUYBACK >0)
		SHARE_REAL_INVESTMENT = REAL_INVESTMENT / (REAL_INVESTMENT + BUYBACK);


	SAVINGS = (1+CONST_INTEREST_RATE)* SAVINGS;



return 0;
}

/*
* Function to update the policy variables
*/

int Manager_policy_update(){


	START_POLICY_MESSAGE_LOOP


	    TAX_RATE_CORPORATE = policy_message->tax_rate_corporate;

	    TAX_RATE_INCOME = policy_message->tax_rate_income;

	    SHORT_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE = policy_message->short_term_capital_gain_on_dividend_tax_rate;

	    LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE = policy_message->long_term_capital_gain_on_dividend_tax_rate;

	    SHORT_TERM_CAPITAL_GAIN_TAX_RATE = policy_message->short_term_capital_gain_tax_rate;

	   	LONG_TERM_CAPITAL_GAIN_TAX_RATE = policy_message->long_term_capital_gain_tax_rate;

	FINISH_POLICY_MESSAGE_LOOP




return 0;
}

