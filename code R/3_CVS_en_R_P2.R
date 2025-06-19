# Seasonal adjustment in R with JD+ : PART 2 ----------------------------------

library("dplyr")
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
ipi$DATE <- as.Date(ipi$DATE, format = "%d/%m/%Y")
ipi[, -1] <- sapply(ipi[, -1], as.numeric)
# View(ipi)
# creating a TS object from a data frame
y_raw <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2024, 1))
y_new <- ts(ipi[, "RF3030"], frequency = 12, start = c(1990, 1), end = c(2024, 6))


tail(y_raw)
tail(y_new)

# X13 v2
sa_x13_v2 <- RJDemetra::x13(y_raw, spec = "RSA5c")
sa_x13_v2  # Naviguer avec $...

#  see help pages for default spec names, identical in v2 and v3
# Tramo-Seats
sa_ts_v2 <- RJDemetra::tramoseats(y_raw, spec = "RSAfull")
sa_ts_v2

# X13 v3
sa_x13_v3 <- rjd3x13::x13(y_raw, spec = "RSA5")
sa_x13_v3

### IN version 2

# load TD regressors 

regs <- read_excel("Data/reg_cjo_m.xlsx")
View(regs)

mts_regs<-ts(regs[,-1],frequency = 12, start=c(1990,1))
class(mts_regs)
head(mts_regs)
# att date incluse, num
mts_reg1_LY <-window(mts_regs[,2:3], start= c(2000,1))


## attention : preciser package (mêmes noms dans rjd3x13)
spec_1<-RJDemetra::x13_spec("RSA3")


### defining user defined trading days
spec_td <- RJDemetra::x13_spec(spec_1,
                    tradingdays.option = "UserDefined",
                    tradingdays.test ="None",
                    usrdef.varEnabled = TRUE,
                    #the user defined variable will be assigned to the calendar component
                    usrdef.varType="Calendar",
                    usrdef.var= mts_reg1_LY )  #regressors have to be a single or multiple TS
# new sa processing
sa_x13_v2_4 <- RJDemetra::x13(y_raw, spec_td)
# user defined intervention variable

start(y_raw)
end(y_raw)

var_aux <- ts(c(rep(0,length(y_raw))), start=c(2012,1))
window(var_aux, start=c(2020), end=c(2020,12))<-1
plot(var_aux)

spec_td_t <- RJDemetra::x13_spec(spec_td,
                     usrdef.varEnabled = TRUE,
                     #the user defined variable will be assigned to the trend component
                     usrdef.varType = "Trend",
                     usrdef.var = var_aux )  #x has to to be a single or multiple TS
# print spec 

# see results 


#new sa processing
sa_x13_v2_5 <- RJDemetra::x13(y_raw, spec_td_t)
summary(sa_x13_v2_5)

# var aux to trend
sa_x13_v2_5$regarima$specification$regression$userdef$variables$series
sa_x13_v2_5$regarima$regression.coefficients

sa_x13_v2_5$r
  
####################################

################# Version 3


# Creating calendar regressors from a calendar 

# Step 1: Create national (or other see doc) calendar if needed

# French calendar 
french_calendar <- national_calendar(
  days = list(
    fixed_day(7, 14), # Bastille Day
    fixed_day(5, 8, validity = list(start = "1982-05-08")), # End of 2nd WW
    special_day("NEWYEAR"),
    special_day("CHRISTMAS"),
    special_day("MAYDAY"),
    special_day("EASTERMONDAY"),
    special_day("ASCENSION"),
    special_day("WHITMONDAY"),
    special_day("ASSUMPTION"),
    special_day("ALLSAINTSDAY"),
    special_day("ARMISTICE")
  )
)

# Luxembourg ?

lux_calendar <- national_calendar(
  days = list(
    fixed_day(6, 23), 
    fixed_day(12, 26),
    fixed_day(5, 9, validity = list(start = "2019-01-01")), # End of 2nd WW
    special_day("NEWYEAR"),
    special_day("CHRISTMAS"),
    special_day("MAYDAY"),
    special_day("EASTERMONDAY"),
    special_day("ASCENSION"),
    special_day("WHITMONDAY"),
    special_day("ASSUMPTION"),
    special_day("ALLSAINTSDAY")
  )
)



### Step 2: Create regressors

# create set of (6) regressors every day is different, contrast with Sunday, based on french national calendar
regs_td <- rjd3toolkit::calendar_td(
  calendar = lux_calendar,
  # formats the regressor like your raw series (length, frequency..)
  s = y_raw,
  groups = c(1, 2, 3, 4, 5, 6, 0),
  contrasts = TRUE
)

# create an intervention variable (to be allocated to "trend")
iv1 <- intervention_variable(
  s = y_raw,
  starts = "2015-01-01",
  ends = "2015-12-01"
)


# regressors can be any TS object

#  Step 3: Create a modelling context 

# Gather regressors into a list
my_regressors <- list(
  Monday = regs_td[, 1],
  Tuesday = regs_td[, 2],
  Wednesday = regs_td[, 3],
  Thursday = regs_td[, 4],
  Friday = regs_td[, 5],
  Saturday = regs_td[, 6],
  reg1 = iv1
)

# create modelling context
my_context <- modelling_context(variables = my_regressors)
# check variables present in modelling context
rjd3toolkit::.r2jd_modellingcontext(my_context)$getTsVariableDictionary()


### Step 4: Add regressors to specification  

x13_spec <- rjd3x13::x13_spec("rsa3")
x13_spec_user_defined <- rjd3toolkit::set_tradingdays(
  x = x13_spec,
  option = "UserDefined",
  uservariable = c(
    "r.Monday", "r.Tuesday", "r.Wednesday",
    "r.Thursday", "r.Friday", "r.Saturday"
  ),
  test = "None"
)


###



# add intervention variable to spec, choosing the component to allocate the effects to TREND
x13_spec_user_defined <- add_usrdefvar(
  x = x13_spec_user_defined,
  group = "r",
  name = "reg1",
  label = "iv1",
  regeffect = "Trend"
)

x13_spec_user_defined$regarima$regression$users



# Step 4: Estimate with context 

sa_x13_ud <- rjd3x13::x13(y_raw, x13_spec_user_defined, context = my_context)
sa_x13_ud$result$preprocessing
summary(sa_x13_ud)


############## REFRESH


library(dygraphs)
dygraph(y_raw)

# start with a simple s

# identify working specs



# here from previous estimation with user defined variables 
current_result_spec <- sa_x13_ud$result_spec
current_domain_spec <- sa_x13_ud$estimation_spec

# step1 : generate NEW spec for refresh (new estimation spec)
refreshed_spec <- rjd3x13::x13_refresh(current_result_spec,
                                       # point spec to be refreshed
                                       current_domain_spec,
                                       # domain spec (set of constraints)
                                       policy = "Fixed"
)

# apply the new spec on new data : y_new = y_raw + 6 months
sa_x13_v3_refreshed <- rjd3x13::x13(y_new, refreshed_spec, context=my_context)
summary(sa_x13_v3_refreshed)

# example with re identification of outliers
refreshed_spec <- rjd3x13::x13_refresh(current_result_spec,
                                       # point spec to be refreshed
                                       current_domain_spec,
                                       # domain spec (set of constraints)
                                       policy = "Outliers",
                                       period = 12,
                                       start = c(2020,1)) #start of re-detection
refreshed_spec
# ajout d'un point aberrant dans y_new sur la fin de periode 
tail(y_new)
window(y_new, start=c(2024,3),end=c(2024,3))<-12
tail(y_new)

# apply the new spec on new data : y_new = y_raw + 6 months
sa_x13_v3_refreshed <- rjd3x13::x13(y_new, refreshed_spec, context=my_context)
summary(sa_x13_v3_refreshed)
# on a bien L'AO idnetifié en mars 2024
                                       

# apply the new spec on new data : y_new = y_raw + 1 month...or more 
# take into consideration what will happen to point revised in the (recent) past, not only new points 


spec_x13_ref <- x13_refresh(current_result_spec,
                            current_domain_spec,
                            policy = "Current",
                            period = 12,
                            start = c(2024, 1),
                            end = end(y_new)
)


sa_x13_v3_refreshed <- rjd3x13::x13(y_new, spec_x13_ref,context=my_context)
summary(sa_x13_v3_refreshed)

sa_x13_v3_refreshed$result
