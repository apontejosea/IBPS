/* Hello World program */
#include<stdio.h>
#include<string.h>


/******************************* 
 * Determine number of fields
 * 
 *
 *******************************/
double model_asset_hours(int age, char * employment, 
                         char * gender, double income) {

  int const EMPLOYMENT_SIZE = 10;
  int const GENDER_SIZE = 2;

  int employment_index;
  int gender_index;

  double coef[EMPLOYMENT_SIZE][GENDER_SIZE];

  printf("%6.9f", 
      (double)age + (double)income);
}

//TODO: Check that the R model is consistent with C routines
void main() {
  model_asset_hours(6.0, "Unemployed", "FEMALE", 60050);
}
