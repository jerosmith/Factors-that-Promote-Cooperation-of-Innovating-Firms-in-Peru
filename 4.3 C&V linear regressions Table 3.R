# CASSIMAN & VEUGELERS LINEAR REGRESSIONS FOR TABLE 3
# 2 sec

# Packages
library(openxlsx)
library(AER)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
path.database = "../Database/"
path.results = "../Results/"
file.table1 = "Table 1.xlsx"
file.database = "Cooperation Peru.xlsx"
file.table4 = "Table 4 - Linear.xlsx"

# Functions
source("Functions.R")

# Load Table 1 results and data
df_table1 = read.xlsx(paste0(path.results, file.table1))
df_table1$Variable = gsub(" ", ".", df_table1$Variable)
df_table1$Variable = gsub("R&D", "RD", df_table1$Variable)
df_table1 = df_table1[df_table1$Variable!="Incoming.Spillovers", ]
df_table1 = df_table1[!is.na(df_table1$Variable), ]
df_data = read.xlsx(paste0(path.database, file.database))

# Read instrumental and explanatory variables
instruments.s = df_table1[df_table1$Role.in.OLS.Regression %in% c("Instrument for Inc. Spill", "Instrument for Inc. Spill & App"), "Variable"]
instruments.a = df_table1[df_table1$Role.in.OLS.Regression %in% c("Instrument for App", "Instrument for Inc. Spill & App"), "Variable"]
explanatory.c = df_table1[df_table1$Role.in.OLS.Regression %in% "Explanatory", "Variable"]
explanatory.c = setdiff(explanatory.c, "Appropriability")

# Create dataframe for regression results (Table 3)
explanatory = c("Incoming.Spillovers", "Appropriability", explanatory.c)
nv = length(explanatory)
variable.rows = rep("", 2*nv)
variable.rows[2*(1:nv)-1] = explanatory
df_table4 = data.frame(  Variable = c(variable.rows, "R2", "Wu-Hausman p-value")
                       , R1 = NA
                       , R2 = NA
                       , R3 = NA
                       , R4 = NA
                       , R5 = NA
                       , R6 = NA
                       )
row.names = variable.rows
nr = length(row.names)
row.names[seq(2,nr,2)] = row.names[seq(1,nr,2)]
nr = length(row.names)
row.names[seq(1,nr,2)] = paste0(row.names[seq(1,nr,2)], ".Estimate")
row.names[seq(2,nr,2)] = paste0(row.names[seq(2,nr,2)], ".Std.Error")
row.names = c(row.names, "R2", "Wu-Hausman p-value")
row.names(df_table4) = row.names

# Regression (1) OLS
formula = paste("Cooperation ~ Incoming.Spillovers + Appropriability +", paste(explanatory.c, collapse = " + "))
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R1"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R1"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R1"] = round(summary(mod)$r.squared, 2)

# Regression (2) OLS without Appropriability
formula = paste("Cooperation ~ Incoming.Spillovers +", paste(explanatory.c, collapse = " + "))
mod = lm(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R2"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R2"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R2"] = round(summary(mod)$r.squared, 2)

# Regression (3) IV with Appropriability as Endogenous 
# Same as (1) but IV
formula = paste("Cooperation ~ Incoming.Spillovers + Appropriability +", paste(explanatory.c, collapse = " + "), "|", paste(instruments.s, collapse = " + "), "+", paste(instruments.a, collapse = " + "), "+", paste(explanatory.c, collapse = " + "))
# formula = "Cooperation ~ Incoming.Spillovers + Appropriability + Brand + Patent + Industrial.Design + Copyright + Origin + Employee.Confidentiality + NDA.Supplier.Client + Revenue.Scaled | Obstacle.Environment + Utility.Model + Science + Innovation + Expenditure.Innovation + Expenditure.RD + Workers + Brand + Patent + Industrial.Design + Copyright + Origin + Employee.Confidentiality + NDA.Supplier.Client + Revenue.Scaled"
mod = ivreg(data = df_data, formula = formula)
# summary(mod)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R3"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R3"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R3"] = round(summary(mod)$r.squared, 2)
df_table4["Wu-Hausman p-value", "R3"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (4) IV with Appropriability as Instrument
# Same as (2) but IV
formula = paste("Cooperation ~ Incoming.Spillovers +", paste(explanatory.c, collapse = " + "), "|", "Appropriability +", paste(instruments.s, collapse = " + "), "+", paste(explanatory.c, collapse = " + "))
mod = ivreg(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R4"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R4"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R4"] = round(summary(mod)$r.squared, 2)
df_table4["Wu-Hausman p-value", "R4"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (5) Cooperation with Suppliers and Customers (IV)
# Dependent variable is Cooperation.Firms. Appropriation is included in the model.
formula = paste("Cooperation.Firms ~ Incoming.Spillovers + Appropriability +", paste(explanatory.c, collapse = " + "), "|", paste(instruments.s, collapse = " + "), "+", paste(explanatory.c, collapse = " + "))
mod = ivreg(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R5"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R5"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R5"] = round(summary(mod)$r.squared, 2)
df_table4["Wu-Hausman p-value", "R5"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Regression (6) Cooperation with Research Institutions (IV)
# Dependent variable is Cooperation.Universities. Appropriation is included in the model.
formula = paste("Cooperation.Universities ~ Incoming.Spillovers + Appropriability +", paste(explanatory.c, collapse = " + "), "|", paste(instruments.s, collapse = " + "), "+", paste(explanatory.c, collapse = " + "))
mod = ivreg(data = df_data, formula = formula)
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table4[paste0(v, ".Estimate"), "R6"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table4[paste0(v, ".Std.Error"), "R6"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_table4["R2", "R6"] = round(summary(mod)$r.squared, 2)
df_table4["Wu-Hausman p-value", "R6"] = format(summary(mod, diagnostics = T)$diagnostics["Wu-Hausman", "p-value"], digits = 2)

# Format and save to results folder
names(df_table4) = c("Variable", "(1) OLS", "(2) OLS without Appropriability", "(3) IV with Appropriability as Explanatory", "(4) IV with Appropriability as Instrument",  "(5) Cooperation with Suppliers and Customers (IV)", "(6) Cooperation with Research Institutions (IV)")
df_table4$Variable = gsub(".Scaled", "", df_table4$Variable)
df_table4$Variable = gsub(".", " ", df_table4$Variable, fixed = T)
df_table4$Variable = gsub("RD", "R&D", df_table4$Variable, fixed = T)
names(df_table4) = gsub(".", " ", names(df_table4), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_table4, file = paste0(path.results, file.table4), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)