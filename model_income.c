#include<stdio.h>
#include<string.h>


double marital_term(char *marital) {
  double term;
  if(strcmp(marital, "Divorced")==0) {
    term = 20019.7160959302;
  }
  else if(strcmp(marital, "Married")==0) {
    term = -38234.9787572086;
  }
  else if(strcmp(marital, "Married-spouse-absent")==0) {
    term = 2882.25952284864;
  }
  else if(strcmp(marital, "Unmarried")==0) {
    term = 14650.1118149451;
  }
  else if(strcmp(marital, "Widowed")==0) {
    term = 4589.21135952353;
  }
  else {
    term = 0.0;
  }
  return(term);
}

double occupation_term(char *occupation) {
  double term;
  if(strcmp(occupation, "Cleaner")==0) {
    term = 3154.66806260141;
  }
  else if(strcmp(occupation, "Clerical")==0) {
    term = 15216.5768390251;
  }
  else if(strcmp(occupation, "Executive")==0) {
    term = -4647.22496654794;
  }
  else if(strcmp(occupation, "Farming")==0) {
    term = 2328.18714207758;
  }
  else if(strcmp(occupation, "Home")==0) {
    term = -1459.36742923379;
  }
  else if(strcmp(occupation, "Machinist")==0) {
    term = 4982.16652739281;
  }
  else if(strcmp(occupation, "Military")==0) {
    term = -6197.90853999854;
  }
  else if(strcmp(occupation, "Professional")==0) {
    term = 5826.95127752641;
  }
  else if(strcmp(occupation, "Protective")==0) {
    term = -10645.7041020745;
  }
  else if(strcmp(occupation, "Repair")==0) {
    term = -15677.3834407823;
  }
  else if(strcmp(occupation, "Sales")==0) {
    term = -855.971707590416;
  }
  else if(strcmp(occupation, "Service")==0) {
    term = 3740.47450123567;
  }
  else if(strcmp(occupation, "Support")==0) {
    term = 2874.88376797108;
  }
  else if(strcmp(occupation, "Transport")==0) {
    term = -17406.6080893882;
  }
  else {
    term = 0.0;
  }
  return(term);
}

double education_term(char *education) {
  double term;
  if(strcmp(education, "Bachelor")==0) {
    term = -18555.9619361224;
  }
  else if(strcmp(education, "College")==0) {
    term = -9719.14005224527;
  }
  else if(strcmp(education, "Doctorate")==0) {
    term = -26879.9428747236;
  }
  else if(strcmp(education, "HSgrad")==0) {
    term = -6760.916201357;
  }
  else if(strcmp(education, "Master")==0) {
    term = -23420.843118432;
  }
  else if(strcmp(education, "Preschool")==0) {
    term = -40984.9027245245;
  }
  else if(strcmp(education, "Professional")==0) {
    term = -8609.31768148089;
  }
  else if(strcmp(education, "Vocational")==0) {
    term = -16877.8364960482;
  }
  else if(strcmp(education, "Yr10")==0) {
    term = -19317.56939451;
  }
  else if(strcmp(education, "Yr11")==0) {
    term = -11009.2374808438;
  }
  else if(strcmp(education, "Yr12")==0) {
    term = -3205.08785747406;
  }
  else if(strcmp(education, "Yr1t4")==0) {
    term = -38931.3405056208;
  }
  else if(strcmp(education, "Yr5t6")==0) {
    term = -917.284669120278;
  }
  else if(strcmp(education, "Yr7t8")==0) {
    term = 6943.44364426559;
  }
  else if(strcmp(education, "Yr9")==0) {
    term = 12546.4632504629;
  }
  else {
    term = 0.0;
  }
  return(term);
}

double model_income(int age, int hours, char * marital, char * occupation, char * education) {

  double const AGE_COEF = -685.154011559694709;
  double const HOURS_COEF = -744.173830295619837;

  return( AGE_COEF * (double)age + HOURS_COEF * (double)hours +
    marital_term(marital) + occupation_term(occupation) + education_term(education) +
    166276.194069942 );
}

// For testing purposes
void main(int argc, char *argv[]) {

  int age;
  int hours;
  char * marital;
  char * occupation;
  char * education;

  if(argc == 6) {
    age = atoi(argv[1]);
    hours = atoi(argv[2]);
    marital = argv[3];
    occupation = argv[4];
    education = argv[5];
  }
else {
  printf("Wrong number of arguments.\n");
  return;
}

  printf("Input:\n");
  printf("------\n");
  printf("Age: %1i\n", age);
  printf("Hours: %1i\n", hours);
  printf("Marital: %s\n", marital);
  printf("Occupation: %s\n", occupation);
  printf("Education: %s\n", education);
  printf("\nOutput:\n");
  printf("------\n");
  printf("Prediction from model_income: %4.5f\n", model_income(age, hours, marital, occupation, education));
}