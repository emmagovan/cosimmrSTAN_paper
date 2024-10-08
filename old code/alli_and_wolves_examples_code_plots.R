## Alligator example in cosimmrSTAN------------
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

plot(in_alli, colour_by_cov = TRUE, cov_name = "Length")
out_alli = cosimmr_stan(in_alli)
#cosimmrSTAN_MCMC = cosimmr_stan(in_alli, type = "STAN_MCMC")

plot(out_alli, type = "prop_histogram")
plot(out_alli, type = "beta_fixed_histogram")
plot(out_alli, type = "beta_random_histogram")
plot(out_alli, type = "covariates_plot", one_plot = TRUE, cov_name = "Length")
plot(out_alli, type = "covariates_plot", one_plot = FALSE, cov_name = "Sclass", n_pred_samples = 1)
x_df_alli = data.frame(Length = c(50, 200), Sclass = as.factor(c("Adult", "Small juvenile")))

pred_alli_out = predict(out_alli, x_pred = x_df_alli)

summary(pred_alli_out, type = "statistics", obs = c(1,2))

## Wolves example in cosimmrSTAN-----------
library(cosimmrSTAN)
wolves_data = cosimmrSTAN::wolves_data

Pack = as.factor(wolves_data$pack)

formula = wolves_data$y ~ (1|Pack)

in_wolves<-cosimmrSTAN_load(formula,
                          source_names = wolves_data$source_names,
                          source_means = wolves_data$wolves_sources[,c(3,5)],
                          source_sds =  wolves_data$wolves_sources[,c(4,6)],
                          correction_means = wolves_data$c_mean,
                          correction_sds = wolves_data$c_sd)

plot(in_wolves, colour_by_cov = TRUE, cov_name = "Pack")
out_wolves = cosimmr_stan(in_wolves)

plot(out_wolves, type = "prop_histogram")
plot(out_wolves, type = "beta_random_histogram")
plot(out_wolves, type = "covariates_plot", one_plot = FALSE, cov_name = "Pack")


##Geese example 
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

plot(in_geese, colour_by_cov = TRUE, cov_name = "Time")
out_geese = cosimmr_stan(in_geese)
plot(out_geese, type = "prop_histogram", obs = c(250))
plot(out_geese, type = "beta_fixed_histogram")

plot(out_geese, type = "covariates_plot", one_plot = FALSE, cov_name = "Time", n_pred_samples = 1)


