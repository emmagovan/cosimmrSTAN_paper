library(cosimmrSTAN)
library(MixSIAR)
library(ggplot2)
library(viridis)

#This is just setting the transparency for the MixSIAR plots
alpha = 0.5

##Fixed effects Geese example---------------
#Fixed effects cosimmrSTAN-----------------------
geese_data = cosimmrSTAN::geese_data
Time = as.factor(geese_data$groups)
formula = geese_data$mixtures ~ Time

in_geese = with(geese_data, cosimmrSTAN_load(formula,
                                             source_names = source_names,
                                             source_means = source_means,
                                             source_sds = source_sds,
                                             correction_means = correction_means,
                                             correction_sds = correction_sds,
                                             concentration_means = concentration_means,
                                             scale_x = FALSE))

plot(in_geese, cov_name = "Time")
out_geese = cosimmr_stan(in_geese)

convergence_check(out_geese)


plot(out_geese, type = "prop_density", obs = c(1)) +xlim(0,1) +ylim(0,20) #To make it match MixSIAR
plot(out_geese, type = "prop_density", obs = c(250)) +xlim(0,1)
plot(out_geese, type = "beta_fixed_density") +ggtitle("beta density plot for Time Period 8 covariate") 

plot(out_geese, type = "covariates_plot", one_plot = FALSE, cov_name = "Time")



##Fixed Effects MixSIAR-------------
# Load mix data
mix.filename <- system.file("extdata", "geese_consumer.csv", package = "MixSIAR")
mix_fixed <- load_mix_data(filename=mix.filename,
                     iso_names=c("d13C","d15N"),
                     factors="Group",
                     fac_random=FALSE,
                     fac_nested=FALSE,
                     cont_effects=NULL)

# Load source data
source.filename <- system.file("extdata", "geese_sources.csv", package = "MixSIAR")
source_fixed <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=TRUE,
                           data_type="means",
                           mix_fixed)

# Load discrimination/TDF data
discr.filename <- system.file("extdata", "geese_discrimination.csv", package = "MixSIAR")
discr_fixed <- load_discr_data(filename=discr.filename, mix_fixed)


# Define model structure and write JAGS model file
model_filename <- "MixSIAR_model.txt"
resid_err <- TRUE
process_err <- FALSE
write_JAGS_model(model_filename, resid_err, process_err, mix_fixed, source_fixed)


jags.fixed <- run_model(run="short", mix_fixed, source_fixed, discr_fixed, model_filename, alpha.prior=1)

## Want to plot MixSIAR output same as cosimmrSTAN
#We need to reorder sources to be in the right order
#And we want group 1 plotted


out_all = jags.fixed$BUGSoutput$sims.list$p.fac1[,1,] #This is selecting Time 1
out_all_p = cbind(out_all[,4], out_all[,2], out_all[,3], out_all[,4])


colnames(out_all_p) = c("Zostera", "Grass", "U. lactuca", "Enteromorpha")

df <- reshape2::melt(out_all_p)


colnames(df) = c("Num", "Source", "Proportion")


g <- ggplot(df, aes(
  x = Proportion,
  fill = Source
)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_density(aes(y = after_stat(density)), alpha = alpha, linetype = 0) +
  theme_bw() +
  ggtitle("Proportions: Observation 1") +
  ylab("Density") +
  facet_wrap("~ Source") +
  theme(legend.position = "none") +xlim(0,1) +ylim(0,20)
print(g) 




##Random Effects Wolves example-------------------

##Random Effects cosimmrSTAN--------------------
wolves_data = cosimmrSTAN::wolves_data
Pack = as.factor(wolves_data$pack)
formula = wolves_data$y ~ (1|Pack)

in_wolves<-with(wolves_data, cosimmrSTAN_load(formula,
                            source_names = source_names,
                            source_means = s_mean,
                            source_sds = s_sd,
                            correction_means = c_mean,
                            correction_sds = c_sd,
                            scale_x = FALSE))

plot(in_wolves, cov_name = "Pack") +xlab("d13C") + ylab("d15N")

out_wolves = cosimmr_stan(in_wolves)

convergence_check(out_wolves)

summary(out_wolves, type = "statistics")
plot(out_wolves, type = c("prop_density", "beta_random_density", "covariates_plot"), 
        one_plot = FALSE, 
        cov_name = "Pack")


plot(out_wolves, type = "prop_density") + xlim(0,1)
plot(out_wolves, type = "beta_random_density") + ggtitle("beta density plot for Pack covariate, level 1") + theme_bw()
plot(out_wolves, type = "omega_density")



##Ternary plot
#install.packages("ggtern")
library(ggtern)
p = out_wolves$output$p
# Assuming p is an array of dimensions n_samples * n_obs * 3 (for 3 sources)
n_samples <- dim(p)[1]
n_obs <- dim(p)[2]
n_sources <- dim(p)[3]

# Calculate the mean proportions for each observation
mean_data <- apply(p, c(2, 3), mean)  # This computes the mean across samples (dimension 1)

# Convert the mean data into a data frame for ggtern
df_means <- as.data.frame(mean_data)
df_means = cbind(df_means, out_wolves$input$covariates_df)
colnames(df_means) <- c("Deer", "Salmon", "MMammals", "Pack")

# Create the ternary plot for the mean proportions of each observation

ggtern(data = df_means, aes(x = Deer, y = Salmon, z = MMammals, colour = Pack)) +
  geom_point() +
  theme_bw() +
  labs(x = "Deer", y = "Salmon", z = "Marine Mammals") +
  ggtitle("Mean Proportions for Each Observation") + scale_fill_viridis()



## Random Effects MixSIAR-----------------

mix.filename <- system.file("extdata", "wolves_consumer.csv", package = "MixSIAR")

# Load mixture data
mix_wolves <- load_mix_data(filename=mix.filename, 
                     iso_names=c("d13C","d15N"), 
                     factors=c("Pack"), 
                     fac_random=c(TRUE), 
                     fac_nested=c(FALSE), 
                     cont_effects=NULL)


# Load source data
source.filename <-"wolves_sources_only3.csv" #MixSIAR uses a separate source for each pack but I will just be using 3 for illustrative purposes
# Load source data
source_wolves <- load_source_data(filename=source.filename, 
                                  source_factors = NULL, 
                                  conc_dep=FALSE, 
                                  data_type="means", 
                                  mix_wolves)
# Load discrimination data
discr.filename <- system.file("extdata", "wolves_discrimination.csv", package = "MixSIAR")
# Load discrimination data
discr_wolves <- load_discr_data(filename=discr.filename, mix_wolves)


model_filename <- "MixSIAR_model.txt"
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix_wolves, source_wolves)


# jags.1 <- run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1)
jags.wolves = run_model(run="normal", mix_wolves, source_wolves, discr_wolves, model_filename, alpha.prior = 1)





## Want to plot MixSIAR output same as cosimmrSTAN



out_all = jags.wolves$BUGSoutput$sims.list$p.fac1[,1,] #This is selecting pack 1
out_all_p = cbind(out_all[,1], out_all[,3], out_all[,2])


colnames(out_all_p) = c("Deer", "Salmon", "Marine Mammals")

df <- reshape2::melt(out_all_p)


colnames(df) = c("Num", "Source", "Proportion")

g <- ggplot(df, aes(
  x = Proportion,
  fill = Source
)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_density(aes(y = after_stat(density)), alpha = alpha, linetype = 0) +
  theme_bw() +
  ggtitle("Proportions: Observation 1") +
  facet_wrap("~ Source") +
  theme(legend.position = "none") +xlim(0,1) + ylim(0,20)
print(g) 

##Mixed Effects Alligator example --------------
#Mixed Effects cosimmrSTAN----------
alligator_data = cosimmrSTAN::alligator_data

Length = alligator_data$length
Sclass = as.factor(alligator_data$sclass)

formula = alligator_data$mixtures ~ Length + (1|Sclass)

in_alli<-with(alligator_data, cosimmrSTAN_load(formula,
                          source_names = source_names,
                          source_means = source_means,
                          source_sds = source_sds,
                          correction_means = TEF_means,
                          correction_sds = TEF_sds))

plot(in_alli, cov_name = "Length")

out_alli = cosimmr_stan(in_alli)

convergence_check(out_alli)

plot(out_alli, 
     type = c("prop_density", 
              "beta_fixed_density", 
              "beta_random_density"))


plot(out_alli, 
     type = c("prop_density")) +ylim(0,8) +xlim(0,1)

plot(out_alli, type = "covariates_plot", cov_name = "Length")
plot(out_alli, type = "covariates_plot", one_plot = FALSE, cov_name = "Sclass")
plot(out_alli, type = "omega_density")


x_df_alli = data.frame(Length = c(50, 200), 
                       Sclass = as.factor(c("Adult", "Small juvenile")))

pred_alli_out = predict(out_alli, x_pred = x_df_alli)

summary(pred_alli_out, type = "statistics", obs = c(1,2))

#Mixed Effects MixSIAR-------------------

mix.filename <- system.file("extdata", "alligator_consumer.csv", package = "MixSIAR")
source.filename <- system.file("extdata", "alligator_sources_simplemean.csv", package = "MixSIAR")
discr.filename <- system.file("extdata", "alligator_TEF.csv", package = "MixSIAR")

mix_alli <- load_mix_data(filename=mix.filename,
                     iso_names=c("d13C","d15N"),
                     factors=c("sclass"),
                     fac_random=c(TRUE),
                     fac_nested=NULL,
                     cont_effects="Length")

source_alli <- load_source_data(filename=source.filename,
                           source_factors=NULL,
                           conc_dep=FALSE,
                           data_type="means",
                           mix_alli)

discr_alli <- load_discr_data(filename=discr.filename, mix_alli)




# Define model structure and write JAGS model file
model_filename <- paste0("MixSIAR_model_", 5, ".txt")
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix_alli, source_alli)
jags.alli = run_model(run="short", mix_alli, source_alli, discr_alli, model_filename, alpha.prior=1)


output_JAGS(jags.alli, mix_alli, source_alli, output_options = list(summary_save = FALSE, summary_name = "summary_statistics",
                                                                  sup_post = FALSE, plot_post_save_pdf = FALSE, plot_post_name = "posterior_density",
                                                                  sup_pairs = FALSE, plot_pairs_save_pdf = FALSE, plot_pairs_name = "pairs_plot", sup_xy
                                                                  = FALSE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", gelman = TRUE, heidel =
                                                                    FALSE, geweke = FALSE, diag_save = TRUE, diag_name = "diagnostics", indiv_effect =
                                                                    FALSE, plot_post_save_png = FALSE, plot_pairs_save_png = FALSE, plot_xy_save_png =
                                                                    FALSE, diag_save_ggmcmc = FALSE))


## Want to plot MixSIAR output same as cosimmrSTAN
##We want Marine then freshwater for obs 1


out_all = jags.alli$BUGSoutput$sims.list$p.ind[,1,] #This is selecting individual 1
out_all_p = cbind(out_all[,2], out_all[,1]) 

colnames(out_all_p) = c("Marine", "Freshwater")

df <- reshape2::melt(out_all_p)


colnames(df) = c("Num", "Source", "Proportion")

g <- ggplot(df, aes(
  x = Proportion,
  fill = Source
)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_density(aes(y = after_stat(density)), alpha = alpha, linetype = 0) +
  theme_bw() +
  ggtitle("Proportions: Observation 1") +
  facet_wrap("~ Source") +
  theme(legend.position = "none") +xlim(0,1) +ylim(0,8)
print(g) 



