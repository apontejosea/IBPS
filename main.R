# TODO: remove defaults at the end
generate_lm  <- function(data_file='audit.csv', 
                         fieldnames=list(x=c('Age', 'Employment', 
                                             'Gender', 'Income'),
                                         y='Hours')) {

  #TODO: Ensure parameters are well formed. E.g. number of vars
  txt_formula<- paste(fieldnames$y, '~', paste0(fieldnames$x, collapse='+'))
  input_data <- read.csv(data_file)
  lm(txt_formula, data=input_data)

  # Generate model output
  #TODO: file names should be dynamic
  sink('model_output.txt')
  cat('COEFFICIENTS:\n')
  cat(paste0(coef(lm_fit), sep='\n'))
  cat('\n\n')
  cat('ANOVA:\n')
  print(anova(lm_fit))
  cat('SUMMARY:\n')
  print(summary(lm_fit))
  sink()

  # Generate C routine
  #TODO: file names should be dynamic
  sink('routine.c')
  
  sink()
}
