#Testing raw source and fitted source fns

#Raw
cons = read.csv("palmyra_consumer.csv")
disc = read.csv("palmyra_discrimination.csv")
sources = read.csv("palmyra_sources.csv")
y = as.matrix(cons[,2:3])
colnames(sources) = c("Source", "d13C", "d15N" )
p_load = cosimmrSTAN_load(formula = y~1,
                          source_names = unique(sources$Source),
                          source = sources,
                          raw_source = TRUE)

p_load$source_means #In order reef lagoon pelagic
p_load$source_sds

#Versus if we just get the mean and sd
library(dplyr)
summary_stats <- sources %>%
  group_by(Source) %>%
  summarize(
    mean_d13C = mean(d13C, na.rm = TRUE),
    sd_d13C = sd(d13C, na.rm = TRUE),
    mean_d15N = mean(d15N, na.rm = TRUE),
    sd_d15N = sd(d15N, na.rm = TRUE)
  )
summary_stats



###----------Fitted
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
                                             scale_x = FALSE,
                                             n_each_source = c(10,5,7,20),
                                             hierarchical_fitting = TRUE))

#Without fitting we get 
# > in_geese$source_means
# [,1]  [,2]
# [1,] -11.17  6.49
# [2,] -30.88  4.43
# [3,] -11.17 11.19
# [4,] -14.06  9.82

#fitted:
in_geese$source_means

# > in_geese$source_sds
# [,1] [,2]
# [1,] 1.21 1.46
# [2,] 0.64 2.27
# [3,] 1.96 1.11
# [4,] 1.17 0.83

#fitted:
in_geese$source_sds
