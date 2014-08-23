#include<stdio.h>
#include<string.h>

double gender_term(char *gender) {
  double term;
  if(strcmp(gender, "MALE")==0) {
      term = 1.0;
  }
  else if(strcmp(gender, "FEMALE")==0) {
    term = 2.0;
  }
  else {
    term = 3.0;
  }
  return(term);
}

double employment_term(char *employment) {
//  switch(employment) {
//    case '.':          
//      return(1.0);
//    case 'Consultant': 
//      return(1.0);
//    case 'Private':    
//      return(1.0);
//    case 'PSFederal':  
//      return(1.0);
//    case 'PSLocal':    
//      return(1.0);
//    case 'PSState':    
//      return(1.0);
//    case 'SelfEmp':    
//      return(1.0);
//    case 'Unemployed': 
//      return(1.0);
//    case 'Volunteer': 
//      return(1.0);
//    default: 
//      return(3.0);
//  }
  return(1); 
}

double model_asset_hours(int age, char * employment, 
                         char * gender, double income) {

  double const INCOME_COEF = 2.0;
  double const AGE_COEF = 1.0;

  // JUST FOR DEBUGGING
  printf("Gender is: %s\n", gender);
  printf("Employment is: %s\n", employment);
  printf("AGE_COEF: %4.5f\n", AGE_COEF);
  printf("INCOME_COEF: %4.5f\n", INCOME_COEF);
  printf("Gender term: %4.5f\n", gender_term(gender));
  printf("Employment term: %4.5f\n", employment_term(employment));
  // END OF DEBUGGING CODE

  // ACTUAL OUTPUT
  printf("%6.9f",
      AGE_COEF * (double)age + INCOME_COEF * (double)income + 
      gender_term(gender) + employment_term(employment));
}


//TODO: Check that the R model is consistent with C routines
void main() {
  model_asset_hours(6.0, "Unemployed", "MALE", 60050);
}
