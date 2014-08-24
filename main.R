############################################################################
# Author: Jose Aponte
# Email:  apontejosea@gmail.com
# Test:   Translating R Results to a C Routine
# Date:   8/24/2014
#
# ============
# INSTRUCTIONS
# ============
# Run the following from the R console:
#   source('main.R')
# 
# EXTRA: To test the output from the R linear models vs the output from the 
# C routines, uncomment the code at the bottom of this file and run the 
# command above.
############################################################################


r2c  <- function(x) {
  type_pairs <-list(C=c('int',    'double', 'char *',   'bool',   'char *'),
                    R=c('integer','numeric','character','logical','factor'))
  type_pairs$C[type_pairs$R == x]
}

c_type <- function(dataset) {
  unlist(lapply(head(dataset, 1), function(x) r2c(class(x))))
}

print_c_formula <- function(clean_cf)  {
  num_coef <- paste(toupper(names(clean_cf$numerical)), 'COEF * (double)', sep='_')
  part_1   <- paste(paste0(num_coef, tolower(names(clean_cf$numerical))), collapse=' + ')
  cat_coef <- paste0(tolower(names(clean_cf$categorical)))
  part_2   <- paste(paste0(cat_coef, '_term(', cat_coef, ')'), collapse=' + ')
  intercept <- ''
  if(!is.null(clean_cf$intercept)) {
    intercept  <- clean_cf$intercept
  }
  full_formula <- paste(part_1, part_2, intercept, sep=' +\n    ')
  cat('\n  return(', full_formula, ');\n')
}

parameters_string <- function(var_names, types) {
  c_params   <- paste0(types, ' ', tolower(var_names))
  parameters <- paste(c_params, collapse=', ')
  parameters
}

generate_c_model_function <- function(fields, coefficients, routine_name, types) {
  param_str  <- parameters_string(fields, types[fields])

  cat(sprintf('double %s(%s) {\n\n', routine_name, param_str))

  for(i in 1:length(coefficients$numerical)) {
    cat(sprintf('  double const %s_COEF = %6.15f;\n', toupper(names(coefficients$numerical)[i]), coefficients$numerical[[i]]))
  }

  print_c_formula(coefficients)

  cat(sprintf('}\n\n'))
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
# Example output: list(Gender=list(MALE=1.2),
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

print_c_main_dynamic_statements <- function(fields, types, routine_name) {
  num_args   <- length(fields)
  fields_lc  <- tolower(fields)
  cat(paste0('  if(argc == ', num_args+1, ') {\n'))
  types <- types[fields]
  for(i in 1:length(fields)) {
    statement <- 
      switch(types[i], 
             'int'    = paste0(fields_lc[i], ' = atoi(argv[', i, ']);'),
             'double' = paste0('sscanf(argv[',i,'],"%lf",&',fields_lc[i],');'),
             'char *' = paste0(fields_lc[i], ' = argv[',i,'];'))
    cat(paste0('    ', statement, '\n'))
  }
  cat('  }\n')
  cat('else {\n')
  cat('  printf("Wrong number of arguments.\\n");\n')
  cat('  return;\n')
  cat('}\n\n')

  cat('  printf("Input:\\n");\n')
  cat('  printf("------\\n");\n')
  for(i in 1:length(fields)) {
    out_format <- switch(types[i], 
                         'int'='%1i', 'double'='%4.5f', 'char *'='%s')
    cat(paste0('  printf("', fields[i], ': ', 
               out_format, '\\n", ', tolower(fields[i]), ');\n'))
  }

  cat('  printf("\\nOutput:\\n");\n')
  cat('  printf("------\\n");\n')
  cat(paste0('  printf("Prediction from ', routine_name, ': %4.5f\\n", ', 
             routine_name, '(', paste(tolower(fields), collapse=', '), '));\n'))

}

generate_c_main_function <- function(fields, routine_name, types) {
  param_str  <- parameters_string(fields, types[fields])
    
  # browser()
  cat('// For testing purposes\n')
  cat('void main(int argc, char *argv[]) {\n\n')
  cat(paste0('  ', paste0(types[fields], ' ', tolower(fields), ';'), 
             collapse='\n'))
  cat('\n\n')
  print_c_main_dynamic_statements(fields, types, routine_name)
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
    generate_c_model_function(all_fields, clean_cf, routine_name, types)
    generate_c_main_function(all_fields, routine_name, types)
    sink() 
    }, error=function(e) {
      sink()
      cat(print(e))
  })
}

generate_model_output <- function(lm_fit, routine_name) {
  model_output_file <- paste0(routine_name,'.txt')
  tryCatch({
    sink(model_output_file)
      cat('==============\n')
      cat('MODEL FORMULA:\n')
      cat('==============\n\n')
      print(formula(lm_fit), showEnv=F)
      cat('\n\n')

      cat('=============\n')
      cat('COEFFICIENTS:\n')
      cat('=============\n\n')
      print(clean_coef(lm_fit))
      cat('\n\n')

      cat('========\n')
      cat('SUMMARY:\n')
      cat('========\n\n')
      print(summary(lm_fit))

      cat('======\n')
      cat('ANOVA:\n')
      cat('======\n\n')
      print(anova(lm_fit))
      cat('\n\n')
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

generate_model_routine <- function(data_file, field_names) {
  dataset   <- read.csv(data_file, stringsAsFactors = T)
  lm_fit    <- fit_lm(dataset, field_names)

  types <- c_type(dataset[1,field_names$x])
  routine_name <- paste0('model_', tolower(field_names$y))
  generate_model_output(lm_fit, routine_name)
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

########################################
# UNCOMMENT THE CODE BELOW FOR TESTING #
########################################

# system('gcc model_hours.c -o model_hours.exe')
# system('gcc model_income.c -o model_income.exe')
# 
# audit_data  <- read.csv('audit.csv', stringsAsFactors = T)
# 
# test_data <- list(A=data.frame(Age=45, Income=89000, Employment='Private', Gender='Male', stringsAsFactors=F),
#                   B=data.frame(Age=34, Hours=42, Marital='Unmarried', Occupation='Executive', Education='College', stringsAsFactors=F))
# 
# cat('\n\n')
# cat('=================\n')
# cat('First Model\n')
# cat('=================\n')
# system(paste('model_hours', paste(test_data[[1]], collapse=' ')))
# lm_fit    <- fit_lm(audit_data, par_sets[[1]])
# cat('R Prediction: ', predict(lm_fit, test_data[[1]]), '\n')
# cat('\n\n')
# cat('=================\n')
# cat('Second Model\n')
# cat('=================\n')
# system(paste('model_income', paste(test_data[[2]], collapse=' ')))
# lm_fit    <- fit_lm(audit_data, par_sets[[2]])
# cat('R Prediction:', predict(lm_fit, test_data[[2]]), '\n')
# cat('\n\n')
