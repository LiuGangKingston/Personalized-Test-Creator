#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define SINGLE_STRING_SIZE 200

extern void check_single_string_size_(int * cst1);

extern void get_generated_i_(int * in_number, int * the_result);
extern void get_generated_r_(int * in_number, double * the_result);
extern void get_generated_c_(int * in_number, double   the_result[2]);
extern void get_generated_s_(int * in_number, int * the_sequence, char the_string[SINGLE_STRING_SIZE]);
extern void get_generated_l_(int * in_number, int * the_sequence, int * the_result, char the_string[SINGLE_STRING_SIZE]);
extern void get_generated_v_(int * in_number, double   the_result[3]);

extern void get_generated_mtx_bracket_(int * in_number, int * the_result);
extern void get_generated_mtx_sizes_(  int * in_number, int   the_result[2]);
extern void get_generated_mtx_element_i_(int * in_number, int * row_number, int * col_number, int * the_result);
extern void get_generated_mtx_element_r_(int * in_number, int * row_number, int * col_number, double * the_result);
extern void get_generated_mtx_element_c_(int * in_number, int * row_number, int * col_number, double   the_result[2]);
extern void get_generated_mtx_element_s_(int * in_number, int * row_number, int * col_number, char the_string[SINGLE_STRING_SIZE]);

extern void get_input_string_(int * in_number, int * sequential_number, char the_string[SINGLE_STRING_SIZE]);

extern void set_calculated_i_(int * cal_number, int * the_result);
extern void set_calculated_r_(int * cal_number, double * the_result);
extern void set_calculated_c_(int * cal_number, double   the_result[2]);
extern void set_calculated_v_(int * cal_number, double   the_result[3]);
extern void set_calculated_s_(int * cal_number, char the_string[SINGLE_STRING_SIZE]);

extern void set_calculated_mtx_bracket_(int * cal_number, int * the_result);

extern void allocate_mtx_i_(int * cal_number, int sizes[2]);
extern void allocate_mtx_r_(int * cal_number, int sizes[2]);
extern void allocate_mtx_c_(int * cal_number, int sizes[2]);
extern void allocate_mtx_s_(int * cal_number, int sizes[2]);

extern void set_calculated_mtx_element_i_(int * cal_number, int * row_number, int * col_number, int * the_result);
extern void set_calculated_mtx_element_r_(int * cal_number, int * row_number, int * col_number, double * the_result);
extern void set_calculated_mtx_element_c_(int * cal_number, int * row_number, int * col_number, double   the_result[2]);
extern void set_calculated_mtx_element_s_(int * cal_number, int * row_number, int * col_number, char the_string[SINGLE_STRING_SIZE]);

extern void get_generated_accuracy_(int * in_number, double the_result[3]);
extern void set_calculated_accuracy_(int * cal_number, double the_result[3]);
extern void get_total_n_of_input_strings_(int * in_number, int * the_result);

extern void get_input_iii_(int * in_number, int    the_result[3]);
extern void get_input_rrr_(int * in_number, double the_result[3]);
extern void get_input_ccc_(int * in_number, double the_result[6]);
extern void get_input_vvv_(int * in_number, double the_result[9]);

extern void get_the_constant_value_of_(char the_string[SINGLE_STRING_SIZE], double * the_result);



void c_constants_to_be_verified()
{    int aconst = SINGLE_STRING_SIZE;
     check_single_string_size_(&aconst);
}

int cget_generated_i(int input)
{   int the_result;
    int in_number ;
    in_number = input;
    get_generated_i_(&in_number, &the_result);
    return(the_result);
}

double cget_generated_r(int input)
{   double the_result;
    int in_number ;
    in_number = input;
    get_generated_r_(&in_number, &the_result);
    return(the_result);
}

void cget_generated_c(int input, double the_result[2])
{    int in_number ;
     in_number = input;
     get_generated_c_(&in_number, the_result);
}

int cget_generated_s(int input, char the_string[SINGLE_STRING_SIZE])
{   int the_sequence;
    int in_number ;
    in_number = input;
    get_generated_s_(&in_number, &the_sequence, the_string);
    the_sequence--;
    return(the_sequence);
}

int cget_generated_l(int input, char the_string[SINGLE_STRING_SIZE])
{   int   the_sequence;
    int   the_result;
    int   in_number ;
    in_number = input;
    get_generated_l_(&in_number, &the_sequence, &the_result, the_string);
    the_sequence--;
    return(the_result);
}

void cget_generated_v(int input, double the_result[3])
{    int in_number ;
     in_number = input;
     get_generated_v_(&in_number, the_result);
}


int cget_generated_mtx_bracket(int input)
{   int the_result;
    int in_number ;
    in_number = input;
    get_generated_mtx_bracket_(&in_number, &the_result);
    return(the_result);
}

void cget_generated_mtx_sizes(int input, int the_result[2])
{    int in_number ;
     in_number = input;
     get_generated_mtx_sizes_(&in_number, the_result);
}

int cget_generated_mtx_element_i(int input, int row_number, int col_number)
{   int  the_result;
    int  in_number ;
    int  rw_number ;
    int  cl_number ;
    in_number = input;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    get_generated_mtx_element_i_(&in_number, &rw_number, &cl_number, &the_result);
    return(the_result);
}

double cget_generated_mtx_element_r(int input, int row_number, int col_number)
{   double the_result;
    int  in_number ;
    int  rw_number ;
    int  cl_number ;
    in_number = input;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    get_generated_mtx_element_r_(&in_number, &rw_number, &cl_number, &the_result);
    return(the_result);
}

void cget_generated_mtx_element_c(int input, int row_number, int col_number, double the_result[2])
{   int  in_number ;
    int  rw_number ;
    int  cl_number ;
    in_number = input;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    get_generated_mtx_element_c_(&in_number, &rw_number, &cl_number, the_result);
}

void cget_generated_mtx_element_s(int input, int row_number, int col_number, char the_string[SINGLE_STRING_SIZE])
{   int  in_number ;
    int  rw_number ;
    int  cl_number ;
    in_number = input;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    get_generated_mtx_element_s_(&in_number, &rw_number, &cl_number, the_string);
}

void cget_input_string(int input, int sequential_number, char the_string[SINGLE_STRING_SIZE])
{   int in_number ;
    int sq_number ;
    in_number = input;
    sq_number = sequential_number + 1;
    get_input_string_(&in_number, &sq_number, the_string);
}

int cget_total_n_of_input_strings(int input)
{   int  the_result;
    int  in_number ;
    in_number = input;
    get_total_n_of_input_strings_(&in_number, &the_result);
    return(the_result);
}

void cget_generated_accuracy(int input, double the_result[3])
{    int in_number ;
     in_number = input;
     get_generated_accuracy_(&in_number, the_result);
}

void cget_input_iii(int input, int the_result[3])
{    int in_number ;
     in_number = input;
     get_input_iii_(&in_number, the_result);
}

void cget_input_rrr(int input, double the_result[3])
{    int in_number ;
     in_number = input;
     get_input_rrr_(&in_number, the_result);
}

void cget_input_ccc(int input, double the_result[6])
{    int in_number ;
     in_number = input;
     get_input_ccc_(&in_number, the_result);
}

void cget_input_vvv(int input, double the_result[9])
{    int in_number ;
     in_number = input;
     get_input_vvv_(&in_number, the_result);
}




void cset_calculated_i(int cal_number, int the_value)
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_i_(&compute_number, &the_value);
}

void cset_calculated_r(int cal_number, double the_value)
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_r_(&compute_number, &the_value);
}

void cset_calculated_c(int cal_number, double the_result[2])
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_c_(&compute_number, the_result);
}

void cset_calculated_v(int cal_number, double the_result[3])
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_v_(&compute_number, the_result);
}

void cset_calculated_s(int cal_number, char the_string[SINGLE_STRING_SIZE])
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_s_(&compute_number, the_string);
}

void cset_calculated_mtx_bracket(int cal_number, int the_value)
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_mtx_bracket_(&compute_number, &the_value);
}

void c_allocate_mtx_i(int cal_number, int the_result[2])
{   int compute_number ;
    compute_number = cal_number;
    allocate_mtx_i_(&compute_number, the_result);
}

void c_allocate_mtx_r(int cal_number, int the_result[2])
{   int compute_number ;
    compute_number = cal_number;
    allocate_mtx_r_(&compute_number, the_result);
}

void c_allocate_mtx_c(int cal_number, int the_result[2])
{   int compute_number ;
    compute_number = cal_number;
    allocate_mtx_c_(&compute_number, the_result);
}

void c_allocate_mtx_s(int cal_number, int the_result[2])
{   int compute_number ;
    compute_number = cal_number;
    allocate_mtx_s_(&compute_number, the_result);
}

void cset_matrix_element_i(int cal_number, int row_number, int col_number, int the_value)
{   int  compute_number ;
    int  rw_number ;
    int  cl_number ;
    compute_number = cal_number;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    set_calculated_mtx_element_i_(&compute_number, &rw_number, &cl_number, &the_value);
}

void cset_matrix_mtx_element_r(int cal_number, int row_number, int col_number, double the_value)
{   int  compute_number ;
    int  rw_number ;
    int  cl_number ;
    compute_number = cal_number;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    set_calculated_mtx_element_r_(&compute_number, &rw_number, &cl_number, &the_value);
}

void cset_matrix_element_c(int cal_number, int row_number, int col_number, double the_result[2])
{   int  compute_number ;
    int  rw_number ;
    int  cl_number ;
    compute_number = cal_number;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    set_calculated_mtx_element_c_(&compute_number, &rw_number, &cl_number, the_result);
}

void cset_matrix_element_s(int cal_number, int row_number, int col_number, char the_string[SINGLE_STRING_SIZE])
{   int  compute_number ;
    int  rw_number ;
    int  cl_number ;
    compute_number = cal_number;
    rw_number = row_number + 1;
    cl_number = col_number + 1;
    set_calculated_mtx_element_s_(&compute_number, &rw_number, &cl_number, the_string);
}

void cset_calculated_accuracy(int cal_number, double the_result[3])
{   int compute_number ;
    compute_number = cal_number;
    set_calculated_accuracy_(&compute_number, the_result);
}


double cget_the_constant_value_of(char the_string[SINGLE_STRING_SIZE])
{   double the_result;
    get_the_constant_value_of_(the_string, &the_result);
    return(the_result);
}





