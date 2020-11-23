/*
 * manager_auxiliary_heders.h
 * Headers for manager auxiliary functions
 * 
 * Author: Sander 
van der Hoog
 * Date: 6.5.2016
 */

double Manager_determine_initial_shareprice();
double Manager_demand_function(double p, competitor_array  competitors, double m, double rho);
double Manager_profit_function(double prod);
double Manager_implicit_investment_function_root_solver( double dpi,double x1,double x2, double budget );
double Manager_implicit_investment_function (double x, void *params);
double return_rank(competitor_array competitors);
