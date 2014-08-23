r2c  <- function(x) {
  type_pairs <- list(C=c('int',     'double',  'char *',    'bool',    'char *'),
                     R=c('integer', 'numeric', 'character', 'logical', 'factor'))
  type_pairs$C[type_pairs$R == x]
}

c_type <- function(.df) {
  unlist(lapply(.df, function(x) r2c(class(x))))
}

#TODO: ...
generate_c_formula <- function(x, lm_fit, coef, .data)  {
  "x"
}

parameters_string <- function(field_names, .data) {
  var_names  <- unlist(c(field_names$y, field_names$x))
  types      <- c_type(.data[1, var_names])
  c_params   <- paste0(types, ' ', tolower(var_names))
  parameters <- paste(c_params, collapse=', ')
  parameters
}

generate_c_file <- function(field_names, lm_fit) {
  file_name <- paste0('model_', tolower(field_names$y), '.c')
  param_str <- parameters_string(field_names, .data)
  c_formula <- with(field_names, generate_c_formula(x, y))
  contrasts <- names(attr(qr(lm_fit)$qr, 'contrasts'))
  print(param_str)
}

#TODO: use this function to generate clean coefficients
generate_coef <- function(lm_fit) {
  .coef <- coef(lm_fit)
}

#TODO: file names should be dynamic
#TODO: make function name consistent with others
print_model_output <- function(lm_fit, 
                               model_output_file='model_output.txt') {
  sink(model_output_file)
  cat('COEFFICIENTS:\n\n')
  cat(paste0(coef(lm_fit), sep='\n'))
  cat('\n\n')
  cat('ANOVA:\n\n')
  print(anova(lm_fit))
  cat('SUMMARY:\n\n')
  print(summary(lm_fit))
  sink()
}

# TODO: remove defaults at the end
fit_lm  <- function(.data, 
                    field_names=list(x=c('Age', 'Employment',
                                        'Gender', 'Income'), y='Hours')) {
  txt_formula<- paste(field_names$y, '~',
                      paste0(field_names$x, collapse='*'))
  return(lm(txt_formula, data=.data, qr=T))
}

generate_model_routine <- function(data_file, field_names) {
  .data  <- read.csv(data_file, stringsAsFactors = T)
  lm_fit <- fit_lm(.data, field_names)
  browser()
  print_model_output(lm_fit)
  generate_c_file(field_names, lm_fit)
  cat('DONE\n')
}

#TODO: Any preferred way to name the C routines?
run <- function() {
  # to enable afterwards
  #   model_par_sets 
  #     <- list(A=list(x=c('Age', 'Employment', 'Gender', 'Income'), 
  #                    y='Hours'),
  #          B=list(x=c('Age','Marital','Occupation', 'Education','Hours'), 
  #                    y='Income'))
  # 
  model_par_sets <- 
    list(A=list(x=c('Age', 'Employment', 'Gender', 'Income'), y='Hours'))

  for(i in length(model_par_sets)) {
    generate_model_routine(data_file='audit.csv', 
                           field_names=model_par_sets[[i]])
  }
}

run()

Income ~ Age + Marital Occupation + Education + Hours
