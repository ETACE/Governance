#include "../header.h"
#include "../Manager_agent_header.h"
#include "Manager_aux_headers.h"
#include "../my_library_header.h"

#include <gsl/gsl_errno.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_roots.h>



double return_rank(competitor_array competitors){
	int i;

	double rank = 0;

	double *data;
	double *ranks;


	int size = competitors.size;

	/*define temp (static) arrays of required size*/

	/*For the sorted data*/
	data = malloc(size*sizeof(double));
	/*For the quantiles*/
	ranks = malloc(size*sizeof(double));

	for(i=0; i< competitors.size;i++){

		data[i] = competitors.array[i].productivity;


	}

/*Sort ascending*/
  qsort (data, size, sizeof(double), compare);

  int_array tmp_rnks;
  init_int_array(&tmp_rnks);

  int rnk = competitors.size;


  for(i=0; i< competitors.size;i++){

	  add_int(&tmp_rnks, rnk);

		if(fabs(data[i]-data[i+1])>0.0 || i== competitors.size -1){

			double mean_rnk = 0;

		  for(int j = 0; j <tmp_rnks.size;j++ ){

			  mean_rnk +=  tmp_rnks.array[j];

		  }

		  ranks[i] = mean_rnk/tmp_rnks.size;

		  if(tmp_rnks.size>1){

			  for(int j = 1; j <tmp_rnks.size;j++ ){

				  ranks[i-j] = mean_rnk/tmp_rnks.size;

			  }

		  }


		  reset_int_array(&tmp_rnks);

	  }

	  rnk--;

  }


  for(i=0; i < size; i++){

	  if(fabs(data[i]- PRODUCTIVITY) < 1e-10){

		  rank = ranks[i];
		  break;

	  }

  }

  free_int_array(&tmp_rnks);

  return rank;

}


/*Demand function; rteturns the current demand given the current competitors' characteristics as well as the market size and the elasticity of substitution*/
double Manager_demand_function(double p, competitor_array competitors, double m, double rho){

	double d = 0.0;
	int n = competitors.size;

	double sum_denom = p*pow((n * p),(1.0/(rho-1)));

	for(int i=0; i < competitors.size; i++){

		if(competitors.array[i].id!=ID)
			sum_denom +=   competitors.array[i].price* pow(n * competitors.array[i].price,1.0/(rho-1));

	}

	d= m * pow(n * p,1.0/(rho-1))/sum_denom;

	return d;

}


/*Aux function to determine a reasonable range for the initial share price*/
double Manager_determine_initial_shareprice(){

	double shareprice;

	double dividend =   CONST_DIVIDEND_RATE*(1+CONST_INTEREST_RATE)*(1-CONST_MANAGER_BONUS_PCT)*((1-TAX_RATE_CORPORATE)*(Manager_profit_function(PRODUCTIVITY) - MANAGER_FIX_INCOME) - TAX_RATE_CORPORATE* MANAGER_FIX_INCOME)/(1-TAX_RATE_CORPORATE*(1+CONST_INTEREST_RATE)*(1-CONST_MANAGER_BONUS_PCT)*(1-CONST_DIVIDEND_RATE));

	dividend = dividend  /NO_OUTSTANDING_SHARES;

	shareprice = dividend * (1-TAX_RATE_CORPORATE)/(CONST_INTEREST_RATE*(1-TAX_RATE_CORPORATE));

	return shareprice;

}

/*Profit function given a productivity level*/
double Manager_profit_function(double prod){


	double mc = WAGE / prod ;

	double price = (1+CONST_MARK_UP) * mc;
	double output = Manager_demand_function(price, COMPETITORS, MARKET_SIZE, CONST_ELASTICITY_SUBSTITUTION);

	double profit =  (price - mc)*output;

	return profit;

}


/*structure for investment function*/
struct implicit_investment_function_params
  {

	double dpi,x1,x2, budget;


  };


/*Implicit investment function*/
double Manager_implicit_investment_function (double x, void *params)
{
  struct implicit_investment_function_params *p
    = (struct implicit_investment_function_params *) params;

  double diff;

  double dpi = p->dpi;  // delta pi
  double x1 = p->x1;
  double x2 = p->x2;
  double budget = p->budget;


  double buyback = max(0,budget - x);

  //Expected profit level after innovation round
  double profit = Manager_profit_function(PRODUCTIVITY) + dpi* CONST_MAX_INNOVATION_PROBABILITY * (1 - exp((-1)*CONST_INNOVATION_INTENSITY*pow(x/SALES,CONST_POWER_PROBABILITY_FUNCTION) ) );


  //Marginal effect in profits wrt real investments
  //double dpi_di =CONST_MAX_INNOVATION_PROBABILITY *CONST_INNOVATION_INTENSITY* dpi * exp((-1)*CONST_INNOVATION_INTENSITY*sqrt(x/SALES))/(2*sqrt(x/SALES));

  double dpi_di =CONST_MAX_INNOVATION_PROBABILITY *CONST_INNOVATION_INTENSITY* dpi * CONST_POWER_PROBABILITY_FUNCTION* exp((-1)*CONST_INNOVATION_INTENSITY*pow(x/SALES,CONST_POWER_PROBABILITY_FUNCTION))*(pow(x/SALES, CONST_POWER_PROBABILITY_FUNCTION-1));


  //Marginal effect in the share price wrt buybacks
  double dv_db = x2 /(sqrt(pow(x1,2)+ 4*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME))*x2*buyback));

  //Expected shares price given buyback
  double share_price = (x1 + sqrt(pow(x1,2)+ 4*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME))*x2*buyback))/(2*(1-FRACTION_OPTIMISTS*SHORT_TERM_CAPITAL_GAIN_TAX_RATE + CONST_INTEREST_RATE*(1-TAX_RATE_INCOME)));


  double shares_hold_by_longterm_investors = FRACTION_SHARES_LONGTERM_INVESTORS*(NO_OUTSTANDING_SHARES - buyback/share_price);


  double dn_db = (-1)*FRACTION_SHARES_LONGTERM_INVESTORS*(share_price - dv_db * buyback)/(pow(share_price,2));


  //Expected dividends in the next period
  double dividends = CONST_DIVIDEND_RATE*(1+CONST_INTEREST_RATE)*(1-CONST_MANAGER_BONUS_PCT)*((1-TAX_RATE_CORPORATE)*profit + TAX_RATE_CORPORATE*(CONST_TAX_DEDUCTION_INVESTMENTS*x +MANAGER_FIX_INCOME));

  //Marginal effect in dividends per share wrt real investments
  double dds_di = CONST_DIVIDEND_RATE*(1+CONST_INTEREST_RATE)*(1-CONST_MANAGER_BONUS_PCT)*((1-TAX_RATE_CORPORATE)*dpi_di + CONST_TAX_DEDUCTION_INVESTMENTS*TAX_RATE_CORPORATE)/(NO_OUTSTANDING_SHARES + SHARES_AS_COMPENSATION_PER_PERIOD - buyback/share_price );

  //Marginal effect in dividends wrt buybacks
  double dds_db = (((share_price - buyback*dv_db)*dividends)/(share_price))/(pow(NO_OUTSTANDING_SHARES + SHARES_AS_COMPENSATION_PER_PERIOD - buyback/share_price ,2));

  //Marginal value of real investments for the manager
  double dm_di = (1-TAX_RATE_INCOME)*CONST_MANAGER_BONUS_PCT*((1-TAX_RATE_CORPORATE)*dpi_di + CONST_TAX_DEDUCTION_INVESTMENTS*TAX_RATE_CORPORATE);

  //Marginal value of buybacks for the manager
  double dm_db = (1-TAX_RATE_INCOME)*CONST_FRACTION_OPTIONS*SHARES_AS_COMPENSATION_PER_PERIOD* max(0,dv_db) + (1-TAX_RATE_INCOME)*(1-CONST_FRACTION_OPTIONS)*SHARES_AS_COMPENSATION_PER_PERIOD* dv_db;

  //Marginal value of buybacks for the short-term investor
  //double ds_db = (dv_db - SHORT_TERM_CAPITAL_GAIN_TAX_RATE*max(0,dv_db ))*(NO_OUTSTANDING_SHARES - NO_SHARES_HOLD_BY_LONGTERM_INVESTORS);

  double ds_db = max(0,dv_db)*(1- SHORT_TERM_CAPITAL_GAIN_TAX_RATE);


  //Marginal value of r&d for the long-term investor
  double dl_di = (1-LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE)*dds_di*shares_hold_by_longterm_investors;


  double diff_shares_longterm = NO_SHARES_HOLD_BY_LONGTERM_INVESTORS -shares_hold_by_longterm_investors;

  //Marginal value of buybacks for the long-term investor
  double dl_db = (1-LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE)*(shares_hold_by_longterm_investors * dds_db + dn_db * dividends/(NO_OUTSTANDING_SHARES + SHARES_AS_COMPENSATION_PER_PERIOD - buyback/share_price));

  dl_db = dl_db + NO_SHARES_HOLD_BY_LONGTERM_INVESTORS*dv_db;



  //Value of the manager
  double m = (1-TAX_RATE_INCOME)*(MANAGER_FIX_INCOME +CONST_MANAGER_BONUS_PCT*(profit*(1-TAX_RATE_CORPORATE) + TAX_RATE_CORPORATE*(CONST_TAX_DEDUCTION_INVESTMENTS*x + MANAGER_FIX_INCOME)) )+ (1-SHORT_TERM_CAPITAL_GAIN_TAX_RATE)*CONST_FRACTION_OPTIONS*max(0.0,SHARES_AS_COMPENSATION_PER_PERIOD*(share_price-SHARE_PRICE))
  + (1-SHORT_TERM_CAPITAL_GAIN_TAX_RATE)*(1-CONST_FRACTION_OPTIONS)*SHARES_AS_COMPENSATION_PER_PERIOD*share_price;
  //Value of the short-term investors
  double s = (1 - SHORT_TERM_CAPITAL_GAIN_TAX_RATE)*max(0, share_price - SHARE_PRICE) + (1-SHORT_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE)*DIVIDEND_PER_SHARE;
  //Value of the long-term investors
  //double l = (shares_hold_by_longterm_investors*(1-LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE)*dividends/(NO_OUTSTANDING_SHARES + SHARES_AS_COMPENSATION_PER_PERIOD - buyback/share_price) +  NO_SHARES_HOLD_BY_LONGTERM_INVESTORS*(share_price - SHARE_PRICE) - LONG_TERM_CAPITAL_GAIN_TAX_RATE*  max(0,(NO_SHARES_HOLD_BY_LONGTERM_INVESTORS -shares_hold_by_longterm_investors)*(share_price - SHARE_PRICE_IN_T0) ));

  double deltav = share_price - SHARE_PRICE;

  //TODO Check max(s-S,0)!!!!!!
  double l = (shares_hold_by_longterm_investors*(1-LONG_TERM_CAPITAL_GAIN_ON_DIVIDEND_TAX_RATE)*dividends/(NO_OUTSTANDING_SHARES + SHARES_AS_COMPENSATION_PER_PERIOD - buyback/share_price) +  NO_SHARES_HOLD_BY_LONGTERM_INVESTORS*(share_price - SHARE_PRICE) );

  if(fabs(INFLUENCE_LONGTERM_INVESTORS)<1e-14)
	  l = 1;


  double factor = pow(m,INFLUENCE_MANAGER)*pow(l,INFLUENCE_LONGTERM_INVESTORS)*pow(s,INFLUENCE_SHORTTERM_INVESTORS);

  //Marginal value of real investments
  MVI = ( INFLUENCE_MANAGER* pow(m ,-1)* dm_di + INFLUENCE_LONGTERM_INVESTORS* pow(l ,-1)*dl_di)*factor;

  //marginal value of buybacks
 // double mvb = (INFLUENCE_MANAGER* pow(m ,-1)* dm_db + INFLUENCE_LONGTERM_INVESTORS* pow(l ,-1)*dl_db + INFLUENCE_SHORTTERM_INVESTORS*pow(s ,-1)*ds_db)*factor;

  MVB = (INFLUENCE_MANAGER* pow(m ,-1)* dm_db + INFLUENCE_LONGTERM_INVESTORS* pow(l ,-1)*dl_db + INFLUENCE_SHORTTERM_INVESTORS*pow(s ,-1)*ds_db)*factor;

  diff = MVI -MVB;

	M = m;
	FACTOR = factor;
	L=l;
	S=s;
	DM_DB=dm_db;
	DS_DB=ds_db;
	DL_DB= dl_db;
	DV_DB = dv_db;
	DDS_DB = dds_di;
	DN_DB = dn_db;


  if(diff!=diff){
//	printf("diff %f  %f\n",INFLUENCE_LONGTERM_INVESTORS , x );
  }

  return diff;

}


/*Root solver for investment function*/
double Manager_implicit_investment_function_root_solver( double dpi,double x1,double x2, double budget ){


	 int status;
	 int iter = 0, max_iter = 100;
	 const gsl_root_fsolver_type *T;
	 gsl_root_fsolver *s;
	 double root = 0;
	 double x_lo = 1e-10, x_hi = budget;
	 gsl_function F;

	 struct implicit_investment_function_params params = {dpi,x1,x2,budget };


	 F.function = &Manager_implicit_investment_function;
	 F.params = &params;



	  T = gsl_root_fsolver_brent;
	  s = gsl_root_fsolver_alloc (T);
	  gsl_set_error_handler_off();
	  gsl_root_fsolver_set (s, &F, x_lo, x_hi);

	  //printf ("using %s method\n",     gsl_root_fsolver_name (s));

	//  printf ("%5s [%9s, %9s] %9s \n","iter", "lower", "upper", "root");

		  do
			{
			  iter++;
			  status = gsl_root_fsolver_iterate (s);

			  if(status==GSL_EBADFUNC){

				 // printf("NDD\n");
				  //break;

			  }



			  root = gsl_root_fsolver_root (s);

			  x_lo = gsl_root_fsolver_x_lower (s);
			  x_hi = gsl_root_fsolver_x_upper (s);


			  status = gsl_root_test_interval (x_lo, x_hi,
											   0, 0.000000000001);





			 // if (status == GSL_SUCCESS)
			  //  printf ("Converged:\n");

		   //   printf ("%5d [%.7f, %.7f]  %+.7f \n",         iter, x_lo, x_hi,root);
			}
		  while ((status == GSL_CONTINUE && iter < max_iter));

	  gsl_root_fsolver_free (s);


	  if(status==GSL_EBADFUNC)
	  	  root = 0.0;

	return root;

}



