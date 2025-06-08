
USDGBP = read.csv("USDGBPTomasz.csv")


#2 January 1980-21 May 1996, 4,274 values
USDGBP$Date <- as.Date(USDGBP$Date,format="%d/%m/%Y")

Nolan_USDGBP = subset(USDGBP, Date >= as.Date('1980-01-02') & Date <= as.Date('1996-05-21'))
# n=nrow(Nolan_USDGBP)
# log_returns_v1 =log(Nolan_USDGBP[-1,2]/Nolan_USDGBP[-n,2])
log_returns = diff(log(Nolan_USDGBP[,2]))

### estimation
install.packages("libstable4u")
library(libstable4u)


pars_init = stable_fit_init(log_returns, parametrization = 0L)
stable_fit_koutrouvelis(log_returns, pars_init = pars_init, parametrization = 0L)
#1.681630e+00  7.490379e-02  3.954086e-03 -2.077659e-05
pars_est_K = stable_fit_koutrouvelis(log_returns, pars_init = pars_init, parametrization = 0L)
stable_fit_mle(log_returns, pars_init = pars_est_K, parametrization = 0L)
# 1.690994015 0.027574136 0.004018359 0.000024855
stable_fit_mle2d(log_returns, pars_init = pars_est_K, parametrization = 0L)
# 1.530762e+00  5.355036e-02  3.703696e-03 -4.982218e-05




[1]  1.536626e+00 -2.156478e-02  3.743957e-03 -2.008458e-05
[1]  1.536626e+00  6.541289e-02  3.736580e-03 -6.080292e-05
