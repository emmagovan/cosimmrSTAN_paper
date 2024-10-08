#Fixed Effects
library(cosimmrSTAN)
library(readxl)
path <- system.file("extdata", "geese_data.xls", package = "simmr")
geese_data <- lapply(excel_sheets(path), read_excel, path = path)
targets <- geese_data[[1]] #[1:20,]
sources <- geese_data[[2]]
TEFs <- geese_data[[3]]
concdep <- geese_data[[4]]
N <- nrow(targets)
K <- nrow(sources)
J <- 2
L <- length(unique(targets$Time))
Time = as.factor(targets$Time)
y = as.matrix(targets[,1:2]) # Data matri
q = concdep[,2:3] # cond dep matri
s_mean = sources[,2:3] # source mean
c_mean = TEFs[,2:3] # corr mean
s_sd = sources[,4:5] # s_sd matri
c_sd = TEFs[,4:5] # c_sd matri
Sex = as.factor(targets$Sex)
Wing = targets$Wing
Weight = targets$`Net Wt`

formula = y ~ Weight + (1|Time)

in_geese_all<-cosimmrSTAN_load(formula,
                           source_names =sources$Sources,
                           source_means = s_mean,
                           source_sds = s_sd,
                           source = NULL,
                           correction_means = c_mean,
                           correction_sds = c_sd,
                           concentration_means = q,
                           scale_x = FALSE)

out_geese = cosimmr_stan(in_geese_all)



plot(in_geese_all, colour_by_cov = TRUE, cov_name = "Weight")
plot(in_geese_all, colour_by_cov = TRUE, cov_name = "Time")



plot(out_geese, type = c("beta_fixed_histogram", "beta_random_histogram", "prop_histogram"))

plot(out_geese, type = c("prop_histogram"), obs = 251)

x_pred_df = data.frame(Wing =c(200, 300), Weight =c(1200, 1300), Time = as.factor(c(1,1)), Sex = as.factor(c(1,1)))

pred_out = predict(out_geese, x_pred = x_pred_df)

