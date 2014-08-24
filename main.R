r2c  <- function(x) {
  type_pairs <-list(C=c('int',    'double', 'char *',   'bool',   'char *'),
                    R=c('integer','numeric','character','logical','factor'))
  type_pairs$C[type_pairs$R == x]
}

c_type <- function(dataset) {
  unlist(lapply(head(dataset, 1), function(x) r2c(class(x))))
}

#TODO: ...
print_c_formula <- function(coef)  {
  num_coef <- paste(toupper(names(clean_cf$numerical)), 'COEF * (double)', sep='_')
  part_1   <- paste(paste0(num_coef, tolower(names(clean_cf$numerical))), collapse=' + ')
  cat_coef <- paste0(tolower(names(clean_cf$categorical)))
  part_2   <- paste(paste0(cat_coef, '_term(', cat_coef, ')'), collapse=' + ')
  cat('\n  return(', paste0(part_1, ' +\n    ', part_2, ');\n'))
}

parameters_string <- function(var_names, types) {
  c_params   <- paste0(types, ' ', tolower(var_names))
  parameters <- paste(c_params, collapse=', ')
  parameters
}

generate_c_model_function <- function(coefficients, routine_name, types) {
  var_names  <- c(names(coefficients$numerical), 
                  names(coefficients$categorical))
  param_str  <- parameters_string(var_names, types)

  cat(sprintf('double %s(%s) {\n\n', routine_name, param_str))

  for(i in 1:length(coefficients$numerical)) {
    cat(sprintf('  double const %s_COEF = %6.15f;\n', toupper(names(coefficients$numerical)[i]), coefficients$numerical[[i]]))
  }

  print_c_formula(coefficients)

  # cat(sprintf('  // JUST FOR DEBUGGING\n'))
  # cat(sprintf('  printf("Gender is: %s\n", gender);\n'))
  # cat(sprintf('  printf("Employment is: %s\n", employment);\n'))
  # cat(sprintf('  printf("AGE_COEF: %4.5f\n", AGE_COEF);\n'))
  # cat(sprintf('  printf("INCOME_COEF: %4.5f\n", INCOME_COEF);\n'))
  # cat(sprintf('  printf("Gender term: %4.5f\n", gender_term(gender));\n'))
  # cat(sprintf('  printf("Employment term: %4.5f\n", employment_term(employment));\n'))
  # cat(sprintf('  // END OF DEBUGGING CODE\n'))
  cat(sprintf('}\n'))
}

print_c_term_if_statement <- function(term_name, coef) {
  for(i in 1:length(coef)) {
    cat(sprintf('  '))
    if(i > 1)
      cat(sprintf('else '))
    cat(sprintf('if(strcmp(%s, "%s")==0) {\n', term_name, names(coef)[i]))
    cat(sprintf('    term = %s;\n', coef[i]))
    cat(sprintf('  }\n'))
  }
  cat(sprintf('  else {\n'))
  cat(sprintf('    term = 0.0;\n'))
  cat(sprintf('  }\n'))
}

# term_name: lower case name of the categorical variable
# coef: named vector of coefficients for categorical variable
generate_c_term_function <- function(term_name='gender', coef=c(MALE=1)) {
  cat(sprintf('double %s_term(char *%s) {\n', term_name, term_name))
  cat(sprintf('  double term;\n'))
  print_c_term_if_statement(term_name, coef)
  cat(sprintf('  return(term);\n'))
  cat(sprintf('}\n\n'))
}

# Returns a list of coefficient splitted by type: 
#  categorical variables, numerical variables & intercept (if any)
# output: list(Gender=list(MALE=1.2),
#             Employment=list(Private=3.4,PSLocal=5.5, ...))
clean_coef <- function(lm_fit) {
  contrasts     <- names(attr(qr(lm_fit)$qr, 'contrasts'))
  cf            <- coef(lm_fit)
  result        <- list(categorical=list(), numerical=list())
  for(i in 1:length(contrasts)) {
    m           <- regexpr(contrasts[i], names(cf))
    pos         <- attr(m, 'match.length')
    values      <- substr(names(cf), pos+1, nchar(names(cf)))[pos>0]
    result$categorical[[contrasts[i]]] <- cf[pos>0]
    names(result$categorical[[contrasts[i]]]) <- values
  }

  m     <- regexpr(paste(contrasts, collapse='|'), names(cf))
  pos   <- attr(m, 'match.length')
  ind   <- which(pos<0)[-1]
  for(i in 1:length(ind)) {
    result$numerical[[names(cf)[ind[i]]]]  <- as.numeric(cf[ind[i]])
  }

  if('(Intercept)'%in% names(cf)) {
    result$intercept <- as.numeric(cf['(Intercept)'])
  }

  result
}

#TODO: add void to all meta functions
#TODO: finalize
generate_c_main_function <- function(fields, routine_name, types) {
  # cat_fields <- names(clean_cf$categorical)
  # num_fields <- names(clean_cf$numerical)
  # all_fields <- c(num_fields, cat_fields)
  param_str  <- parameters_string(fields, types[fields])
    
  cat('// For testing purposes\n')
  cat(paste0('void main(', param_str, ') {\n'))
  cat(paste0('  printf("%4.5f", ', routine_name, '(', 
             paste(tolower(fields), collapse=', '), '));\n'))
  cat('}')
}

r_class <- function(dataset) {
  unlist(lapply(head(dataset, 1), function(x) class(x)))
}

generate_c_file <- function(routine_name, lm_fit, types) {
  clean_cf     <- clean_coef(lm_fit)
  num_fields   <- names(clean_cf$numerical)
  cat_fields   <- names(clean_cf$categorical)
  all_fields   <- c(num_fields, cat_fields)
  # routine_name <- paste0('model_', tolower(field_names$y))
  # types        <- c_type(dataset[1, field_names$x])[all_fields]

  tryCatch({
    sink(paste0(routine_name, '.c'))
    cat(sprintf('#include<stdio.h>\n'))
    cat(sprintf('#include<string.h>\n\n\n'))
    if(length(clean_cf$categorical)>0) {
      for(i in 1:length(clean_cf$categorical)) {
        generate_c_term_function(tolower(names(clean_cf$categorical)[i]),
                                 coef=clean_cf$categorical[[i]])
      }
    }
    generate_c_model_function(clean_cf, routine_name, types)
    generate_c_main_function(all_fields, routine_name, types)
    sink() 
    }, error=function(e) {
      sink()
      cat(print(e))
  })
}

# TODO: file names should be dynamic
# TODO: make function name consistent with others
print_model_output <- function(lm_fit, routine_name) {
  model_output_file <- paste0(routine_name,'.txt')
  tryCatch({
    sink(model_output_file)
      cat('COEFFICIENTS:\n\n')
      cat(paste0(coef(lm_fit), sep='\n'))
      cat('\n\n')
      cat('ANOVA:\n\n')
      print(anova(lm_fit))
      cat('SUMMARY:\n\n')
      print(summary(lm_fit))
    sink()
  }, error = function(e) {
    print(e)
    sink()
  })
}

fit_lm  <- function(dataset, field_names) {
  txt_formula<- paste(field_names$y, '~',
                      paste0(field_names$x, collapse='+'))
  return(lm(txt_formula, data=dataset))
}

# TODO: Any preferred way to name the C routines?
# I'm thinking model_<datafilename>_<dependentvar>.c
generate_model_routine <- function(data_file, field_names) {
  dataset   <- read.csv(data_file, stringsAsFactors = T)
  lm_fit    <- fit_lm(dataset, field_names)
  routine_name <- paste0('model_', tolower(field_names$y))
  print_model_output(lm_fit, routine_name)
  generate_c_file(routine_name, lm_fit, types)
}

run <- function(model_par_sets) {
  for(i in 1:length(model_par_sets)) {
    generate_model_routine(data_file='audit.csv', 
                           field_names=model_par_sets[[i]])
  }
}

par_sets <- list( A=list(x=c('Age', 'Employment', 'Gender', 'Income'),
                         y='Hours'),
                  B=list(x=c('Age','Marital','Occupation', 'Education',
                             'Hours'), 
                         y='Income'))

run(par_sets)
