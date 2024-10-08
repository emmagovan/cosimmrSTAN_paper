##Timing for wolves example cosimmrSTAN vs MixSIAR

##cosimmrSTAN--------------------
library(cosimmrSTAN)
library(MixSIAR)
library(microbenchmark)
wolves_data = cosimmrSTAN::wolves_data
pack = wolves_data$pack
formula = wolves_data$y ~ (1|pack)

in_wolves<-cosimmrSTAN_load(formula,
                            source_names = wolves_data$source_names,
                            source_means = wolves_data$s_mean,
                            source_sds = wolves_data$s_sd,
                            correction_means = wolves_data$c_mean,
                            correction_sds = wolves_data$c_sd)

#wolves_out = cosimmr_stan(in_wolves)

## MixSIAR-----------------

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


# jags.1 <- run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1)
MixSIAR = run_model(run="normal", mix, source, discr, model_filename, alpha.prior = 1)





## Want to plot MixSIAR output same as cosimmrSTAN
#Sources are in order 1,3,2 we want to reorder


out_all = MixSIAR$BUGSoutput$sims.list$p.fac1[,1,] #This is selecting pack 1
out_all_p = cbind(out_all[,1], out_all[,3], out_all[,2])


colnames(out_all_p) = c("Deer", "Salmon", "Marine Mammals")

df <- reshape2::melt(out_all_p)


colnames(df) = c("Num", "Source", "Proportion")

library(ggplot2)
library(viridis)
g <- ggplot(df, aes(
  x = Proportion,
  fill = Source
)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_histogram(binwidth = 0.05, alpha = 0.5) +
  theme_bw() +
  ggtitle("Proportions: Observation 1") +
  facet_wrap("~ Source") +
  theme(legend.position = "none")
print(g) 




