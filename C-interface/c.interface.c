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




void verify_c_constants()
{    int aconst = SINGLE_STRING_SIZE;
     check_single_string_size_(&aconst);
}

int cgets_generated_i(int input)
{   int the_result;
    int in_number ;
    in_number = input;
    get_generated_i_(&in_number, &the_result);
    return(the_result);
}

double cgets_generated_r(int input)
{   double the_result;
    int in_number ;
    in_number = input;
    get_generated_r_(&in_number, &the_result);
    return(the_result);
}

void cgets_generated_c(int input, double the_result[2])
{    int in_number ;
     in_number = input;
     get_generated_c_(&in_number, the_result);
}

int cgets_generated_s(int input, char the_string[SINGLE_STRING_SIZE])
{   int the_sequence;
    int in_number ;
    in_number = input;
    get_generated_s_(&in_number, &the_sequence, the_string);
    the_sequence--;
    return(the_sequence);
}

int cgets_generated_l(int input, char the_string[SINGLE_STRING_SIZE])
{   int   the_sequence;
    int   the_result;
    int   in_number ;
    in_number = input;
    get_generated_l_(&in_number, &the_sequence, &the_result, the_string);
    the_sequence--;
    return(the_result);
}

void cgets_generated_v(int input, double the_result[3])
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




void c_solve_the_question_(int * question_id)
{   // Predefined varaibles ready for use:
    int input_number, input_number1, input_number2, input_number3, input_number4;
    int cal_number;

    int size[2], sizea[2], sizeb[2], sizec[2], sized[2], sizee[2], sizef[2], sizeg[2];
    int bkt, bkta, bktb;

    int i,   i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8,  i9;
    int ia,  ia1, ia2, ia3, ia4, ia5, ia6, ia7, ia8, ia9;
    int iaa, ibb, icc, idd, iee, iff, igg, ihh, iii, ijj;

    double r,   r1,  r2,  r3,  r4,  r5,  r6,  r7,  r8,  r9;
    double ra,  ra1, ra2, ra3, ra4, ra5, ra6, ra7, ra8, ra9;
    double raa, rbb, rcc, rdd, ree, rff, rgg, rhh, rii, rjj;

    double c[2],   c1[2],  c2[2],  c3[2],  c4[2],  c5[2],  c6[2],  c7[2],  c8[2],  c9[2];
    double ca[2],  ca1[2], ca2[2], ca3[2], ca4[2], ca5[2], ca6[2], ca7[2], ca8[2], ca9[2];
    double caa[2], cbb[2], ccc[2], cdd[2], cee[2], cff[2], cgg[2], chh[2], cii[2], cjj[2];

    double v[3],   v1[3],  v2[3],  v3[3],  v4[3],  v5[3],  v6[3],  v7[3],  v8[3],  v9[3];
    double va[3],  va1[3], va2[3], va3[3], va4[3], va5[3], va6[3], va7[3], va8[3], va9[3];
    double vaa[3], vbb[3], vcc[3], vdd[3], vee[3], vff[3], vgg[3], vhh[3], vii[3], vjj[3];

    char str[SINGLE_STRING_SIZE],   str1[SINGLE_STRING_SIZE],  str2[SINGLE_STRING_SIZE];
    char str3[SINGLE_STRING_SIZE],  str4[SINGLE_STRING_SIZE],  str5[SINGLE_STRING_SIZE];
    char stra[SINGLE_STRING_SIZE],  stra1[SINGLE_STRING_SIZE], stra2[SINGLE_STRING_SIZE];
    char stra3[SINGLE_STRING_SIZE], stra4[SINGLE_STRING_SIZE], stra5[SINGLE_STRING_SIZE];
    char straa[SINGLE_STRING_SIZE], strbb[SINGLE_STRING_SIZE], strcc[SINGLE_STRING_SIZE];
    char strdd[SINGLE_STRING_SIZE], stree[SINGLE_STRING_SIZE], strff[SINGLE_STRING_SIZE];

    // End of the predefined varaibles.

    // Constant checks with FORTRAN code.
    verify_c_constants();


    switch( * question_id )
    {
	case 60:
      input_number = 1;
      input_number1 = 2;
      bkt = cget_generated_mtx_bracket(input_number);
      cget_generated_mtx_sizes(input_number, size);
      cget_generated_mtx_sizes(input_number1, sizea);
      if (size[1] != sizea[0]) {
         printf("Stopped for inconsistent array sizes %d %d for Question %d \n", size[1], sizea[0], * question_id);
         exit(2);
      }
      sizeb[0] = size[0];
      sizeb[1] = sizea[1];
      cal_number = 1;
      c_allocate_mtx_i(cal_number, sizeb);
      cset_calculated_mtx_bracket(cal_number, bkt);

      for (i = 0; i < sizeb[0]; i++) {
          for (ia = 0; ia < sizeb[1]; ia++) {
              iii = 0;
              for (iaa = 0; iaa < size[1]; iaa++) {

                  iii = iii + cget_generated_mtx_element_i(input_number, i, iaa) *
                              cget_generated_mtx_element_i(input_number1, iaa, ia) ;
              }
              cset_matrix_element_i(cal_number, i, ia, iii);
          }
      }

      input_number = 3;
      input_number1 = 4;
      bkt = cget_generated_mtx_bracket(input_number);
      cget_generated_mtx_sizes(input_number, size);
      cget_generated_mtx_sizes(input_number1, sizea);
      if (size[1] != sizea[0]) {
         printf("Stopped for inconsistent string array sizes %d %d for Question %d \n", size[1], sizea[0], * question_id);
         exit(2);
      }
      sizeb[0] = size[0];
      sizeb[1] = sizea[1];
      cal_number = 2;
      c_allocate_mtx_s(cal_number, sizeb);
      cset_calculated_mtx_bracket(cal_number, bkt);

      for (i = 0; i < sizeb[0]; i++) {
          for (ia = 0; ia < sizeb[1]; ia++) {
              iii = 10;
              strcpy(straa, " \0");
              for (iaa = 0; iaa < size[1]; iaa++) {

                  cget_generated_mtx_element_s(input_number, i, iaa, str);
                  cget_generated_mtx_element_s(input_number1, iaa, ia, stra);
                  iii = iii + strlen(str) + strlen(stra) + 12;
                  if (iii > SINGLE_STRING_SIZE) {
                      printf("Stopped for too long string %d for Question %d \n", iii, * question_id);
                      exit(2);
                  }
                  strcat( straa, str);
                  strcat( straa, " \\times ");
                  strcat( straa, stra);
                  if (iaa < size[1] -1 ) {
                      strcat( straa, " + ");
                  }
              }
              cset_matrix_element_s(cal_number, i, ia, straa);
          }
      }


			break;

	case 81:
			break;

	default:
			break;
    }
}


