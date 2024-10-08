#Alligator cosimmrSTAN vs MixSIAR timing-------------
#cosimmrSTAN----------------
library(cosimmrSTAN)
alligator_data = cosimmrSTAN::alligator_data

Length = alligator_data$length
Sclass = as.factor(alligator_data$sclass)

formula = alligator_data$mixtures ~ Length + (1|Sclass)

in_alli<-cosimmrSTAN_load(formula,
                          source_names = alligator_data$source_names,
                          source_means = alligator_data$source_means,
                          source_sds = alligator_data$source_sds,
                          correction_means = alligator_data$TEF_means,
                          correction_sds = alligator_data$TEF_sds)

#out_alli = cosimmr_stan(in_alli)


#MixSIAR--------------
mix.filename <- system.file("extdata", "alligator_consumer.csv", package = "MixSIAR")
source.filename <- system.file("extdata", "alligator_sources_simplemean.csv", package = "MixSIAR")
discr.filename <- system.file("extdata", "alligator_TEF.csv", package = "MixSIAR")

mix <- load_mix_data(filename=mix.filename,
                          iso_names=c("d13C","d15N"),
                          factors=c("sclass"),
                          fac_random=c(TRUE),
                          fac_nested=NULL,
                          cont_effects="Length")

source <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix)

discr <- load_discr_data(filename=discr.filename, mix)


  
  
# Define model structure and write JAGS model file
model_filename <- paste0("MixSIAR_model_", 5, ".txt")
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)
MixSIAR = run_model(run="short", mix, source, discr, model_filename, alpha.prior=1)


  microbenchmark(cosimmrSTAN_VB =  cosimmr_stan(in_alli),
                 cosimmrSTAN_MCMC = cosimmr_stan(in_alli, type = "STAN_MCMC"),
                 MixSIAR = run_model(run="short", mix, source, discr, model_filename, alpha.prior=1), times = 10L)


##Wolves timing cosimmrSTAN and MixSIAR comparison-------------------
  wolves_data = cosimmrSTAN::wolves_data
  
  Pack = as.factor(wolves_data$pack)
  
  formula = wolves_data$y ~ (1|Pack)
  
  in_wolves<-cosimmrSTAN_load(formula,
                              source_names = wolves_data$source_names,
                              source_means = wolves_data$wolves_sources[,c(3,5)],
                              source_sds =  wolves_data$wolves_sources[,c(4,6)],
                              correction_means = wolves_data$c_mean,
                              correction_sds = wolves_data$c_sd)

  
  mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")
  
  # Load mixture data
  mix <- load_mix_data(filename=mix.filename, 
                       iso_names=c("d13C","d15N"), 
                       factors=c("Pack"), 
                       fac_random=c(TRUE), 
                       fac_nested=c(FALSE), 
                       cont_effects=NULL)
  
  
  # Load source data
  source.filename <-"wolves_sources_only3.csv" #MixSIAR uses a separate source for each pack but I will just be using 3 for illustrative purposes
  # Load source data
  source <- load_source_data(filename=source.filename, source_factors = NULL, conc_dep=FALSE, data_type="means", mix)
  # Load discrimination data
  discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
  # Load discrimination data
  discr <- load_discr_data(filename=discr.filename, mix)
  
  
  model_filename <- "MixSIAR_model.txt"
  resid_err <- TRUE
  process_err <- TRUE
  write_JAGS_model(model_filename, resid_err, process_err, mix, source)
  
  microbenchmark(cosimmrSTAN_VB =  cosimmr_stan(in_wolves),
                 cosimmrSTAN_MCMC = cosimmr_stan(in_wolves, type = "STAN_MCMC"),
                 MixSIAR = run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1), 
                 times = 10L)
  

  
  
  
  
  
## Geese timing cosimmrSTAN and MixSIAR comparison----------------
  
 ##cosimmrSTAN 
  geese_data = cosimmrSTAN::geese_data
  Time = as.factor(geese_data$groups)
  formula = geese_data$mixtures ~ Time
  
  in_geese = cosimmrSTAN_load(formula,
                              source_names = geese_data$source_names,
                              source_means = geese_data$source_means,
                              source_sds = geese_data$source_sds,
                              correction_means = geese_data$correction_means,
                              correction_sds = geese_data$correction_sds,
                              concentration_means = geese_data$concentration_means,
                              scale_x = FALSE)
  
  #MixSIAR
  
  # Load mix data
  mix.filename <- system.file("extdata", "geese_consumer.csv", package = "MixSIAR")
  mix <- load_mix_data(filename=mix.filename,
                       iso_names=c("d13C","d15N"),
                       factors="Group",
                       fac_random=FALSE,
                       fac_nested=FALSE,
                       cont_effects=NULL)
  
  # Load source data
  source.filename <- system.file("extdata", "geese_sources.csv", package = "MixSIAR")
  source <- load_source_data(filename=source.filename,
                             source_factors=NULL,
                             conc_dep=TRUE,
                             data_type="means",
                             mix)
  
  # Load discrimination/TDF data
  discr.filename <- system.file("extdata", "geese_discrimination.csv", package = "MixSIAR")
  discr <- load_discr_data(filename=discr.filename, mix)
  
  
  # Define model structure and write JAGS model file
  model_filename <- "MixSIAR_model.txt"
  resid_err <- TRUE
  process_err <- FALSE
  write_JAGS_model(model_filename, resid_err, process_err, mix, source)
  
 
 # jags.1 <- run_model(run="short", mix, source, discr, model_filename, alpha.prior=1)
  
  microbenchmark(cosimmrSTAN_VB = cosimmr_stan(in_geese),
                 cosimmrSTAN_MCMC = cosimmr_stan(in_geese, type = "STAN_MCMC"),
                 MixSIAR = run_model(run="short", mix, source, discr, model_filename, alpha.prior=1))
  
  
  
  
  
  
  
  