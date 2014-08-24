#include<stdio.h>
#include<string.h>


double employment_term(char *employment) {
  double term;
  if(strcmp(employment, "Consultant")==0) {
    term = 10.1161473128515;
  }
  else if(strcmp(employment, "Private")==0) {
    term = 8.58694472885789;
  }
  else if(strcmp(employment, "PSFederal")==0) {
    term = 8.79276428660723;
  }
  else if(strcmp(employment, "PSLocal")==0) {
    term = 9.55544448729503;
  }
  else if(strcmp(employment, "PSState")==0) {
    term = 6.41965037754055;
  }
  else if(strcmp(employment, "SelfEmp")==0) {
    term = 17.4228179633809;
  }
  else if(strcmp(employment, "Unemployed")==0) {
    term = -17.2020498125385;
  }
  else if(strcmp(employment, "Volunteer")==0) {
    term = 16.5336740160314;
  }
  else {
    term = 0.0;
  }
  return(term);
}

double gender_term(char *gender) {
  double term;
  if(strcmp(gender, "Male")==0) {
    term = 4.32123904644334;
  }
  else {
    term = 0.0;
  }
  return(term);
}

double model_hours(int age, double income, char * employment, char * gender) {

  double const AGE_COEF = -0.005083496853896;
  double const INCOME_COEF = -0.000022198445190;

  return( AGE_COEF * (double)age + INCOME_COEF * (double)income +
    employment_term(employment) + gender_term(gender) +
    30.5975011887793 );
}

// For testing purposes
void main(int argc, char *argv[]) {

  int age;
  double income;
  char * employment;
  char * gender;

  if(argc == 5) {
    age = atoi(argv[1]);
    sscanf(argv[2],"%lf",&income);
    employment = argv[3];
    gender = argv[4];
  }
else {
  printf("Wrong number of arguments.\n");
  return;
}

  printf("Input:\n");
  printf("------\n");
  printf("Age: %1i\n", age);
  printf("Income: %4.5f\n", income);
  printf("Employment: %s\n", employment);
  printf("Gender: %s\n", gender);
  printf("\nOutput:\n");
  printf("------\n");
  printf("Prediction from model_hours: %4.5f\n", model_hours(age, income, employment, gender));
}