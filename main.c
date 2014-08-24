#include<stdio.h>
#include<string.h>

double gender_term(char *gender) {
  double term;
  if(strcmp(gender, "MALE")==0) {
      term = 1.0;
  }
  else {
    term = 3.0;
  }
  return(term);
}

double employment_term(char *employment) {
  return(1); 
}

double asset_hours_model(int age, char * employment, 
                         char * gender, double income) {

  double const INCOME_COEF = 2.0;
  double const AGE_COEF = 1.0;

  // ACTUAL OUTPUT
  printf("Output is: %6.9f",
      AGE_COEF * (double)age + INCOME_COEF * (double)income + 
      gender_term(gender) + employment_term(employment));

  // JUST FOR DEBUGGING
  printf("Gender is: %s\n", gender);
  printf("Employment is: %s\n", employment);
  printf("AGE_COEF: %4.5f\n", AGE_COEF);
  printf("INCOME_COEF: %4.5f\n", INCOME_COEF);
  printf("Gender term: %4.5f\n", gender_term(gender));
  printf("Employment term: %4.5f\n", employment_term(employment));
  // END OF DEBUGGING CODE
}

//TODO: Check that the R model is consistent with C routines
void main() {
  model_asset_hours(6.0, "Unemployed", "FEMALE", 60050);
}
