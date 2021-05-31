#include <string>
#include <cstring>
#include <iostream>
using namespace std;
#include "cpp.interface.cpp"


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
         cout << "Stopped for inconsistent array sizes " <<size[1]
              <<", " << sizea[0] << " for Question " << * question_id << endl;
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
         cout << "Stopped for inconsistent array sizes " <<size[1]
              <<", " << sizea[0] << " for Question " << * question_id << endl;
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
                      cout << "Stopped for too long string " <<iii << " for Question " << * question_id << endl;
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


