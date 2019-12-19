#include <libraries needed>

// define largesr time point that can be recorded
#define MAX_DATA 100000

int main( int count_of_command_line_arg, list_of_all_command_line_arg){
	// These are parameters that I will have to input in the command line
	double parm1 = strtod(argv[1], pointer_to_char)  //interprets the contents of the string(char array) as a floating pt. no. and returns value as a double
	double parms = strtod(argv[2], pointer_to_char)  //command line input has to be inn same order

	// Check that tmax is not larger than the array used for storing results.
   	// Here we assume that data is recorded each integer time step, i.e.
	// tmax=MAX_DATA is largest tmax we can use.
	if(t_max> (maximum_array_used_for_storing_result - 1)) {printf("t_max = %d\n", max_array - 1); break and exit;}
	// print the values of the parameters
	printf("parms =, parm2= ", parm1, parm2)
	
	// Initialise
	double current_time = 0;
	double next_time_pt_recorded = 0;
	int row_no_for_data_entry = 0;
	double data_array [Max_data_row][3_col]

	// Record initial state
	double data_array[row_1][0] = current_t;
	double data_array[row_1][1] = density_sp1;
	double data_array[row-1][2] = density_sp2;
	advance row++;

	// generate random number
	srand(use_computer_time)
		
	// start the simulation
	do{ if current_time>next_time_pt_recorded {
		double data_array[row_1][0] = current_t;
	double data_array[row_1][1] = density_sp1;
	double data_array[row-1][2] = density_sp2;
	advance row++;
	advance next_time_pt_recorded++;
		}
	   //rate of each event
	   double event_1 = func1(parms1,parms2);
	   double event2 = func2(parms1, parms2);
	   
	   // total event rate
	   double total = event1 + event2;
	   
	   //determinne the next event, and update time
	    double r = (double)rand() / (double)RAND_MAX;
            t_current += -1/total * log10(r);
	   
	   //determinne the next event, and update event/populations
	   r = (double)rand() / (double)RAND_MAX;
           double p = r * total;
           double sum = event1;
           if      (p <= event1) { advance++; sum += event2;}
           else if (p <= event1+event2) necxt advance/receed
           else advance/receed;
	   
	   
	}
	while(current_time < T_max && (population>0));
	//record final state of data
	double data_array[row_1][0] = current_t;
	double data_array[row_1][1] = density_sp1;
	double data_array[row-1][2] = density_sp2;
	advance row++;


}
