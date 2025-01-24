# FIRST-STEP REGRESSION
# Outputs: Table 1. This is used to choose the appropriate instruments.
# 2 sec

# Packages
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.database = "../Database/"
path.results = "../Results/"
file.metadata = "Metadata P09.xlsx"
file.database = "Cooperation Peru.xlsx"
file.table1 = "Table 1.xlsx"

# Functions
source("Functions.R")

# Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Calculated Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# Get all explanatory variables and potential instruments for first-stage regression
endogenous = df_variables[df_variables$Role.in.Dataset=="Endogenous" & !is.na(df_variables$Table.1), "Variable.R"]
exogenous = df_variables[df_variables$Role.in.Dataset=="Exogenous" & !is.na(df_variables$Table.1), "Variable.R"]

# Create Table 1 dataframe
variables = c(endogenous, exogenous)
nv = length(variables)
Variables = rep("", 2*nv)
Variables[2*(1:nv)-1] = variables
Variables = c(Variables, "(Intercept)", "")
df_table1 = data.frame(  Variable = Variables
                        , Cooperation.Logit = NA
                        , Cooperation.OLS = NA
                        , Incoming.Spillovers = NA
                        , Appropriability = NA
                        , Role.in.Logit.Regression = NA
                        , Role.in.OLS.Regression = NA
                        )

# Cooperation Logit
formula = paste(variables, collapse = " + ")
formula = paste("Cooperation.Bin ~", formula)
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_table1$Variable==v)
  p = df_coeff[v, 4]
  df_table1[i, "Cooperation.Logit"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table1[i+1, "Cooperation.Logit"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}

# Cooperation OLS
formula = paste(variables, collapse = " + ")
formula = paste("Cooperation.Bin ~", formula)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_table1$Variable==v)
  p = df_coeff[v, 4]
  df_table1[i, "Cooperation.OLS"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table1[i+1, "Cooperation.OLS"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}

# Incoming.Spillovers
formula = paste(exogenous, collapse = " + ")
formula = paste("Incoming.Spillovers ~ Appropriability +", formula)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_table1$Variable==v)
  p = df_coeff[v, 4]
  df_table1[i, "Incoming.Spillovers"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table1[i+1, "Incoming.Spillovers"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}

# Appropriability
formula = paste(exogenous, collapse = " + ")
formula = paste("Appropriability ~ Incoming.Spillovers +", formula)
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = row.names(df_coeff)
for (v in var){
  print(v)
  i = which(df_table1$Variable==v)
  p = df_coeff[v, 4]
  df_table1[i, "Appropriability"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table1[i+1, "Appropriability"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}

# Assign logit and OLS roles: instrument, explanatory or neither
cooperation.logit.sig = grepl("*", df_table1$Cooperation.Logit, fixed = T)
cooperation.ols.sig = grepl("*", df_table1$Cooperation.OLS, fixed = T)
incoming.spillovers.sig = grepl("*", df_table1$Incoming.Spillovers, fixed = T)
appropriability.sig = grepl("*", df_table1$Appropriability, fixed = T)
df_table1$Role.in.Logit.Regression = ifelse(!cooperation.logit.sig & incoming.spillovers.sig & appropriability.sig, "Instrument for Inc. Spill & App", df_table1$Role.in.Logit.Regression)
df_table1$Role.in.Logit.Regression = ifelse(!cooperation.logit.sig & incoming.spillovers.sig & !appropriability.sig, "Instrument for Inc. Spill", df_table1$Role.in.Logit.Regression)
df_table1$Role.in.Logit.Regression = ifelse(!cooperation.logit.sig & !incoming.spillovers.sig & appropriability.sig, "Instrument for App", df_table1$Role.in.Logit.Regression)
df_table1$Role.in.Logit.Regression = ifelse(cooperation.logit.sig, "Explanatory", df_table1$Role.in.Logit.Regression)
df_table1$Role.in.OLS.Regression = ifelse(!cooperation.ols.sig & incoming.spillovers.sig & appropriability.sig, "Instrument for Inc. Spill & App", df_table1$Role.in.OLS.Regression)
df_table1$Role.in.OLS.Regression = ifelse(!cooperation.ols.sig & incoming.spillovers.sig & !appropriability.sig, "Instrument for Inc. Spill", df_table1$Role.in.OLS.Regression)
df_table1$Role.in.OLS.Regression = ifelse(!cooperation.ols.sig & !incoming.spillovers.sig & appropriability.sig, "Instrument for App", df_table1$Role.in.OLS.Regression)
df_table1$Role.in.OLS.Regression = ifelse(cooperation.ols.sig, "Explanatory", df_table1$Role.in.OLS.Regression)
df_table1[df_table1$Variable=="(Intercept)", c("Role.in.Logit.Regression", "Role.in.OLS.Regression")] = NA # The intercept is neither explanatory nor an instrument.

# Format and save to results folder
names(df_table1) = gsub(".", " ", names(df_table1), fixed = T)
df_table1$Variable = gsub(".", " ", df_table1$Variable, fixed = T)
df_table1$Variable = gsub("RD", "R&D", df_table1$Variable, fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_table1, file = paste0(path.results, file.table1), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)
