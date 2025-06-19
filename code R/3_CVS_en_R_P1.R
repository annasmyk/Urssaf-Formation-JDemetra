 # Seasonal adjustment in R with JD+ ----------------------------------
 
 library("dplyr")
 library("readxl")
 library("zoo")
 library("xts")
 library("tsbox")
 library("imputeTS")
 library("lubridate")
 library("rjd3toolkit")
 library("RJDemetra")
 library("rjd3x13")
 library("rjd3tramoseats")

 # also see readme files for simple examples 

ipi <- read.csv2("Data/IPI_nace4.csv")
str(ipi)
ipi$DATE <- as.Date(ipi$DATE, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
#View(ipi)

# si donnees excel
# ipi_nace4_ind <- read_excel("Data/ipi_nace4_ind.xlsx")
# View(ipi_nace4_ind)

ipi[1,"RF3030"]

 # creating a TS object from a data frame
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2024, 1))
y_raw[1]

y_raw_2 <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2024, 6))

y_new<- window(y_raw_2, start=c(2012,1))



# X13 v2
sa_x13_v2 <- RJDemetra::x13(y_raw, spec = "RSA5c")
sa_x13_v2  # Naviguer avec $...

#  see help pages for default spec names, identical in v2 and v3
# Tramo-Seats
sa_ts_v2 <- RJDemetra::tramoseats(y_raw, spec = "RSAfull")
sa_ts_v2$final$series |> class()
sa_ts_v2$final$forecasts

# X13 v3
sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA3")
sa_x13_v3$user_defined




 # sa_x13_v3$result$preadjust
 # sa_x13_v3$result$decomposition
sa_x13_v3$result$final$d11final
 sa_x13_v3$result$mstats
 sa_x13_v3$result$diagnostics

plot(sa_x13_v3$result$final$d11final)
lines(y_raw, col="red")


 sa_x13_v3$estimation_spec$x11
 sa_x13_v3$estimation_spec$benchmarking

 sa_x13_v3$result_spec 

 sa_x13_v3$user_defined


# Tramo seats v3

sa_ts_v3 <- rjd3tramoseats::tramoseats(y_raw, spec = "RSAfull")
print(sa_ts_v3)
summary(sa_ts_v3)

sa_ts_v3$result

 sa_ts_v3$result$preprocessing
 sa_ts_v3$result$decomposition
 sa_ts_v3$result$final
 sa_ts_v3$result$diagnostics

 sa_ts_v3$estimation_spec

 sa_ts_v3$estimation_spec$tramo
 sa_ts_v3$estimation_spec$seats
 sa_ts_v3$estimation_spec$benchmarking

 sa_ts_v3$result_spec  

 sa_ts_v3$user_defined

# In V2
#Reg-Arima part from X13 only (different default spec names, cf help pages)
 regA_v2 <- RJDemetra::regarima_x13(y_raw, spec = "RG5c")
 regA_v2

# Tramo only
 tramo_v2 <- RJDemetra::regarima_tramoseats(y_raw,spec = "TRfull")
 tramo_v2
 
 
# IN v3
# X13
 sa_regarima_v3 <- rjd3x13::regarima(y_raw, spec = "RG3")
summary(sa_regarima_v3)
# Tramo seats
sa_tramo_v3 <- rjd3tramoseats::tramo(y_raw, spec = "TRfull")

#  "fast." versions...(just results, cf output structure)


# X11 in V2 (spec option)
 X11_v2 <- RJDemetra::x13(y_raw, spec = "X11")
 X11_v2

# X11 in v3 (specific function)
 
x11_v3 <- rjd3x13::x11(y_raw)  


# v2 "output"

Model_sa <- RJDemetra::x13(y_raw, spec = "RSA5")

 Model_sa$regarima
 Model_sa$decomposition


 sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
 sa_x13_v3$result
 sa_x13_v3$estimation_spec
 sa_x13_v3$result_spec
 sa_x13_v3$user_defined

# Version 2 : display of Main Results table (from GUI)
 sa_x13_v2$final$series #y, sa,t,s,i
 sa_x13_v2$final$forecasts

# Version 3

 # final seasonally adjusted series
 sa_x13_v3$result$final$d11final

### Pre adjustment series 
 
# Version 2
ts_p<-sa_x13_v2$regarima$model$effects #MTS object
y_lin<-ts_p[,1]

#forecast accessible only via user defined output (cf below)

# Version 3: "x11 names" : preadjustement effets as stored in the A table
# see doc chap x11 for names

sa_x13_v3$result$preadjust

# Decomposition (no preadjustment effect)
# Version 3
sa_x13_v3$result$decomposition$d5  #tables from D1 to D13


# Version 2
 print(sa_x13_v2)
 sa_x13_v2$decomposition$mstats
 sa_x13_v2$decomposition$s_filter
 sa_x13_v2$decomposition$t_filter

# version 3 (more diagnostics available by default)
 print(sa_x13_v2)
 sa_x13_v3$result$diagnostics$td.ftest.i

# User defined output 
# Version 2
 user_defined_variables("X13-ARIMA")
 user_defined_variables("TRAMO-SEATS")
 
 # exemple doc 
 y<- ipi_c_eu[, "FR"]
 user_defined_variables("X13-ARIMA")
 m <- x13(y,"RSA5c", userdefined=c("decomposition.b1","ycal","residuals.kurtosis" ))
 m$user_defined$decomposition.b1 # serie linearisee
 m$user_defined$ycal
 m$user_defined$residuals.kurtosis
 user_defined_variables("TRAMO-SEATS")
 m <- tramoseats(y,"RSAfull", userdefined=c("ycal","variancedecomposition.seasonality"))
 m$user_defined$ycal
 m$user_defined$variancedecomposition.seasonality
 
 
 

 # Version 3: more specific functions
 userdefined_variables_tramoseats("tramoseats")
 userdefined_variables_tramoseats("tramo")  restriction

 userdefined_variables_x13("regarima") restriction
 userdefined_variables_x13()


# version 3
 ud <- userdefined_variables_x13()[15:17]  #b series
 ud
 sa_x13_v3_UD <- rjd3x13::x13(y_raw, "RSA5c", userdefined = ud)
 sa_x13_v3_UD$user_defined  #names

 # retrieve the object
 sa_x13_v3_UD$user_defined$cal
 
 
 ## Plots in v3

 #remotes::install_github("AQLT/ggdemetra3", INSTALL_opts = "--no-multiarch")
 library("ggdemetra3")
  
# Plot of the final decomposition
 plot(sa_x13_v3)
 # avec le format autoplot 
 library("ggplot2")
 autoplot(sa_x13_v3)

 #Plot SI ratios
 siratioplot(sa_x13_v3)
# avec le format autoplot 
 ggsiratioplot(sa_x13_v3)

########################################################################################
###                         Specification customization
 
############################# version 2 
## (att: bien preciser le package ) pour les fonctions qui ont un nom identique 

 #changing estimation span, imposing additive model and
 # adding user defined outliers

 #  first create a new spec modifying the previous one
 spec_1 <- RJDemetra::x13_spec("RSA3") # from a default spec 
 #or
 spec_1 <- RJDemetra::x13_spec(sa_x13_v2) # extracting from a model 
 
 
 
 spec_2 <- RJDemetra::x13_spec(spec_1, estimate.from = "2015-01-01",
                    usrdef.outliersEnabled = TRUE,
                    usrdef.outliersType = c("LS", "AO"),
                    usrdef.outliersDate = c("2008-10-01", "2018-01-01"),
                    transform.function = "None")  # additive model
 
  # here the reg-arima model will be estimated from  "2004-01-01"
  # the decomposition will be run on the whole span

# Estimation with new spec  (new sa processing)
 sa_x13_v2_2 <- RJDemetra::x13(y_raw, spec_2)
 # print spec and see 
 spec_2
 
 sa_x13_v2_2$regarima$specification$regression$userdef$outliers

##############################Version 3
 
#start with default spec
 
spec_1 <- x13_spec("RSA3")
#or start with existing spec (no extraction function needed)




# ##### set basic : series span for the estimation
x13_spec_d <- rjd3toolkit::set_basic(spec_1,
                                     type = "From", d0 = "2014-01-01",
                                     preliminary.check = TRUE,
                                     preprocessing = TRUE
)


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)


## define span until d1, excluded
x13_spec_d <- set_basic(x13_spec_d,
                        type = "From", d0 = "2014-01-01",
                        preliminary.check = TRUE,
                        preprocessing = TRUE
)

y_raw
print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
m$result$final$d11final
start(m$result$final$d11final)
end(m$result$final$d11final)

## Last observation (dynamic choice)
x13_spec_d <- set_basic(x13_spec_d,
                        type = "Last", n1 = 60,
                        preliminary.check = TRUE,
                        preprocessing = TRUE
)

print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

# Excluding : N first and P Last obs
x13_spec_d <- set_basic(x13_spec_d,
                        type = "Excluding", n0 = 5, n1 = 3,
                        preliminary.check = TRUE,
                        preprocessing = TRUE
)


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)


# ##### set estimate : length for the arima model only, can be combined with series span
x13_spec_d <- rjd3x13::x13_spec("rsa3") # re init
x13_spec_d <- rjd3toolkit::set_estimate(x13_spec_d, "From", d0 = "2015-01-01")


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)


## set  transform : log or not
##
x13_spec_d <- rjd3toolkit::set_transform(x13_spec_d,
                                         fun = "Log",
                                         outliers = TRUE
) # when auto choice: big outlier detection for test: new v3 feature


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

## Modify automatic outlier detection parameters
x13_spec_d <- rjd3toolkit::set_outlier(x13_spec_d,
                                       span.type = "From", d0 = "2012-01-01",
                                       outliers.type = c("LS", "AO"), # LS are excluded
                                       critical.value = 5,
                                       tc.rate = 0.85
)


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
start(m$result$final$d11final)
end(m$result$final$d11final)

# Modify automatic arima model estimation parameters (not advised to tweak this)
x13_spec_d <- set_automodel(x13_spec_d,
                            enabled = TRUE, # automatic detection
                            cancel = 0.06,
                            ub1 = 1.05,
                            ub2 = 1.15,
                            reducecv = 0.15,
                            ljungboxlimit = 0.96,
                            tsig = 1.5,
                            ubfinal = 1.06,
                            checkmu = FALSE,
                            balanced = TRUE
)

# Customized arima model specification
x13_spec_d <- rjd3x13::x13_spec("rsa3") # re init
# disable automatic arima modelling
x13_spec_d <- set_automodel(x13_spec_d, enabled = FALSE)
# customize arima model
x13_spec_d <- set_arima(x13_spec_d,
                        mean = 0.2,
                        mean.type = "Fixed",
                        p = 1, d = 2, q = 0,
                        bp = 1, bd = 1, bq = 0,
                        coef = c(0.6, 0.7),
                        coef.type = c("Initial", "Fixed")
)


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
summary(m)


# ### set benchmarking
x13_spec_d <- rjd3toolkit::set_benchmarking(x13_spec_d,
                                            enabled = TRUE,
                                            target = "ORIGINAL",
                                            rho = 0.8,
                                            lambda = 0.5,
                                            forecast = FALSE,
                                            bias = "None"
)
# output will have to be retrieved in user defined output
userdefined_variables_x13() # list of items
sa_x13_d <- rjd3x13::x13(y_raw, x13_spec_d, userdefined = "benchmarking.result")
y_bench<-sa_x13_d$user_defined$benchmarking.result
plot(y_bench)
# creating a spec from default
x13_spec_d <- rjd3x13::x13_spec("rsa3")



### set_tradingdays
# JD+ built in regressors, no national calendar unless defined)
x13_spec_d <- rjd3toolkit::set_tradingdays(x13_spec_d,
                                           option = "WorkingDays", test = "None",
                                           coef = 0,
                                           # coef.type = c("Fixed", "Estimated", "Fixed"),
                                           leapyear = "LeapYear"
)

print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)

summary(m)

m$result$preprocessing$description$preadjustment

m$result$preprocessing$estimation$parameters$description

# ### set_easter
x13_spec_d <- rjd3x13::x13_spec("rsa3") # re init
x13_spec_d <- set_easter(x13_spec_d,
                         enabled = TRUE,
                         duration = 12
)


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)
m$result$preprocessing$description

### Pre specified outliers
x13_spec_d <- rjd3x13::x13_spec("rsa3") # re init
# Pre-specified outliers
x13_spec_d <- rjd3toolkit::add_outlier(x13_spec_d, type = c("AO", "LS"), date = c("2020-03-01", "2020-04-01"))


print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)


# Adding a ramp
x13_spec_d <- rjd3toolkit::add_ramp(x13_spec_d, start = "2021-01-01", end = "2021-12-01")

print(x13_spec_d)
# check results
m <- rjd3x13::x13(y_raw, x13_spec_d)

# auxiliary regressors 
m$result$preprocessing$estimation$X

### X11 parameters 

spec_2 <- rjd3x13::set_x11(spec_1, henderson.filter = 13)
rjd3x13::x13(y_raw, spec_2)
 





 

