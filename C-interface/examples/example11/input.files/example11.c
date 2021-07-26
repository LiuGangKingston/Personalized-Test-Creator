#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "c.interface.c"


void c_solve_the_question_(int * question_id)
{   // Predefined varaibles ready for use:
    int input_number, input_number1, input_number2, input_number3, input_number4;
    int cal_number, cal_number1, cal_number2, cal_number3, cal_number4;

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
    c_constants_to_be_verified();

    // A constant as an example (more in the *.interface.cpp file):
    // printf("The the constant is %f \n", cget_the_constant_value_of("GRAVITATIONAL-CONSTANT\0"));


    switch( * question_id )
    {

    case(1):
      input_number = 1;
      input_number1 = 2;
      cal_number = 1;
      r = cget_generated_r(input_number1);
      if(r!=0.0e0){
         cget_generated_v(input_number,v);
         va[0] = v[0]/r;
         va[1] = v[1]/r;
         va[2] = v[2]/r;
         cset_calculated_v(cal_number,va);
      }
      break;

    case(2):
      input_number = 4;
      r = cget_generated_r(input_number);
      for(i=1; i<4; i++) {
         if(r!=0.0e0){
            r2 = cget_generated_r(i)/r;
            cset_calculated_r(i,r2);
            cal_number = i+3;
            r3 = r2 * 3600 * 3600 / 1000;
            cset_calculated_r(cal_number,r3);
         }
      }
      break;

    case(3):
      for(i=1; i<21; i++) {
          i1 = 20 + i;
          if(i == 1) {i1 =10;}
          if(i == 2) {i1 =3;}
          cset_calculated_i(i,i1);
      }
      break;

    case(4):
      input_number = 1;
      input_number1 = 2;
      cal_number = 1;
      i = cget_generated_i(input_number1);
      i1 = 0;
      if(i != 0) {i1 = cget_generated_i(input_number)/i;}
      cset_calculated_i(cal_number, i1);
      break;

    case(5):
      strcpy(str3, "$T$\0");
      strcpy(str4, "$F$\0");
      input_number = 1;
      input_number1 = 2;
      cal_number = 1;
      ia1 = cget_generated_s(input_number1,stra1);
      if((cget_generated_i(input_number)%2) == 0){
            if(ia1 == 0) {cset_calculated_s(cal_number, str3);}
            else         {cset_calculated_s(cal_number, str4);}
      } else {
            if(ia1 == 0) {cset_calculated_s(cal_number, str4);}
            else         {cset_calculated_s(cal_number, str3);}
      }

      input_number = 3;
      input_number1 = 4;
      cal_number = 2;
      ia1 = cget_generated_s(input_number1,stra1);
      if(cget_generated_s(input_number,stra2) < 2){
            if(ia1 == 0) {cset_calculated_s(cal_number, str3);}
            else         {cset_calculated_s(cal_number, str4);}
      } else {
            if(ia1 == 0) {cset_calculated_s(cal_number, str4);}
            else         {cset_calculated_s(cal_number, str3);}
      }

      input_number = 5;
      input_number1 = 6;
      cal_number = 3;
      ia1 = cget_generated_s(input_number1,stra1);
      if(cget_generated_s(input_number,stra2) == 0){
            if(ia1 == 0) {cset_calculated_s(cal_number, str3);}
            else         {cset_calculated_s(cal_number, str4);}
      } else {
            if(ia1 == 0) {cset_calculated_s(cal_number, str4);}
            else         {cset_calculated_s(cal_number, str3);}
      }
      break;




    case(21):
      input_number = 4;
      r = cget_generated_r(input_number);
      for(i=1; i<4; i++) {
         if(r!=0.0e0){
            r2 = cget_generated_r(i)/r;
            cset_calculated_r(i,r2);
            cal_number = i+3;
            r3 = r2 * 3600 * 3600 / 1000;
            cset_calculated_r(cal_number,r3);
         }
      }
      break;




    case(22):
      input_number = 4;
      r = cget_generated_r(input_number);
      for(i=1; i<4; i++) {
         if(r!=0.0e0){
            r2 = cget_generated_r(i)/r;
            cset_calculated_r(i,r2);
            cal_number = i+3;
            r3 = r2 * 3600 * 3600 / 1000;
            cset_calculated_r(cal_number,r3);
         }
      }
      break;




    case(23):
      input_number = 4;
      r = cget_generated_r(input_number);
      for(i=1; i<4; i++) {
         if(r!=0.0e0){
            r2 = cget_generated_r(i)/r;
            cset_calculated_r(i,r2);
            cal_number = i+3;
            r3 = r2 * 3600 * 3600 / 1000;
            cset_calculated_r(cal_number,r3);
         }
      }
      break;




    case(24):
      for(i=1; i<9; i++) {
         i2 = i*2;
         i3 = i2 + 1;
         i8 = 18;
         r5 = cget_generated_r(i3);
         if(r5!=0.0e0){
            ra = cget_generated_r(i8) * cget_generated_r(1) * cget_generated_r(i2) / (r5*r5);
            cal_number = i;
            cset_calculated_r(cal_number,ra);
         }
      }
      break;




    case(25):
      break;




    case(26):
      cal_number = 1;
      input_number = 2;
      r1 = 1 - cget_generated_r(input_number);
      cset_calculated_r(cal_number,r1);
      cal_number++;
      input_number = 4;
      r2 = 1 - cget_generated_r(input_number);
      cset_calculated_r(cal_number,r2);
      cal_number++;
      cset_calculated_r(cal_number,r1*r2);
      break;




    case(27):
    /*  cal_number1 = 1;
      cal_number2 = 2;
      input_number1 = 2;
      input_number2 = 4;
      r1 = 1 - cget_generated_r(input_number1);
      r2 = 1 - cget_generated_r(input_number2);
      cset_calculated_r(cal_number1,r1);
      cset_calculated_r(cal_number2,r2);
      */

      r2 = cget_generated_r(2);
      r4 = cget_generated_r(4);
      cset_calculated_r(1,1-r2);
      cset_calculated_r(2,1-r4);

      i = cget_generated_l(1,str1);
      if(i){   ra3 =   r2;
               ra4 = 1-r2;
      } else { ra3 = 1-r2;
               ra4 =   r2;
      }
      cset_calculated_r(3, ra3);
      cset_calculated_r(4, ra4);

      i = cget_generated_l(3,str1);
      if(i){   ra5 =   r4;
               ra6 = 1-r4;
      } else { ra5 = 1-r4;
               ra6 =   r4;
      }
      cset_calculated_r(5, ra5);
      cset_calculated_r(6, ra6);

      cset_calculated_r(7, ra3 * ra5);
      cset_calculated_r(8, ra3 * ra6);
      cset_calculated_r(9, ra4 * ra5);
      cset_calculated_r(10,ra4 * ra6);

      cset_calculated_r(11, ra3 * ra5 + ra3 * ra6 + ra4 * ra5 + ra4 * ra6);

      break;




    case(28):

      r1 = cget_generated_i(1);
      r2 = cget_generated_i(2);
      ia = cget_generated_s(3,stra);

      if(ia == 0) {
         strcpy(str, "ADDITION\0");
         cset_calculated_s(1, str);
         cset_calculated_r(2, r1 + r2);
      }

      if(ia == 1) {
         strcpy(str, "SUBTRACTION\0");
         cset_calculated_s(1, str);
         cset_calculated_r(2, r1 - r2);
      }

      if(ia == 2) {
         strcpy(str, "MULTIPLICATION\0");
         cset_calculated_s(1, str);
         cset_calculated_r(2, r1 * r2);
      }

      if(ia == 3) {
         strcpy(str, "DIVISION\0");
         cset_calculated_s(1, str);
         cset_calculated_r(2, r1 / r2);
      }

      break;




    case(50):
      cget_generated_v(1, v1);
      r2 = cget_generated_r(2);
      if(r2!=0.0e0){
         for(i=0; i<3; i++) {
            v[i] = v1[i] / r2;
         }
      }
      cset_calculated_v(1,v);
      break;




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




	case 70:

	  ia1 = cget_generated_i(1);
	  ia2 = cget_generated_i(2);
	  ia3 = cget_generated_i(3);

	  i1 = ia3;
      cset_calculated_i(1, i1);   // A in equation of A*x^2+B*x+C=0

      i3 = -ia3*(ia1 + ia2);
      cset_calculated_i(3, i3);
                                   // B in equation of A*x^2+B*x+C=0

      strcpy(str, "+\0");
      if(i3 <= 0) {strcpy(str, " \0");}
      cset_calculated_s(2, str);

      i5 = ia3*(ia1 * ia2);
      cset_calculated_i(5, i5);
                                   // C in equation of A*x^2+B*x+C=0

      strcpy(str, "+\0");
      if(i5 <= 0) {strcpy(str, " \0");}
      cset_calculated_s(4, str);

      i6 = i1 * (ia1 * ia1); // A*(x1)^2 for verifying purpose
      i7 = i3 * ia1;         // B*x1 for verifying purpose
      i8 = i6 + i7;          // A*(x1)^2 +B*x1 for verifying purpose
      i9 = i8 + i5;          // A*(x1)^2 +B*x1 +C for verifying purpose

      iaa = i1 * (ia2 * ia2);  // A*(x2)^2 for verifying purpose
      ibb = i3 * ia2;          // B*x2 for verifying purpose
      icc = iaa + ibb;         // A*(x2)^2 +B*x2 for verifying purpose
      idd = icc + i5;          // A*(x2)^2 +B*x2 +C for verifying purpose

      cset_calculated_i(6, i6);
      cset_calculated_i(7, i7);
      cset_calculated_i(8, i8);
      cset_calculated_i(9, i9);
      cset_calculated_i(10, iaa);
      cset_calculated_i(11, ibb);
      cset_calculated_i(12, icc);
      cset_calculated_i(13, idd);

      break;



	default:
      break;
    }
}


