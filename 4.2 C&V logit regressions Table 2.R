# CASSIMAN & VEUGELERS LOGIT REGRESSIONS FOR TABLE 2
# 7 sec

# Packages
library(openxlsx)

# Clear memory and start stopwatch
rm(list = ls())
t0 = Sys.time()

# Parameters
prob.threshold = 0.5
path.database = "../Database/"
path.results = "../Results/"
file.table1 = "Table 1.xlsx"
file.database = "Cooperation Peru.xlsx"
file.table2 = "Table 2 - Logit.xlsx"
file.marginal = "Table 3 - Logit Marginal Effects.xlsx"

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
instruments.s = df_table1[df_table1$Role.in.Logit.Regression %in% c("Instrument for Inc. Spill", "Instrument for Inc. Spill & App"), "Variable"]
instruments.a = df_table1[df_table1$Role.in.Logit.Regression %in% c("Instrument for App", "Instrument for Inc. Spill & App"), "Variable"]
explanatory.c = df_table1[df_table1$Role.in.Logit.Regression %in% "Explanatory", "Variable"]
explanatory.c = setdiff(explanatory.c, "Appropriability")

# Create dataframe for regression results (Table 2)
explanatory = c("Incoming.Spillovers", "Appropriability", explanatory.c)
nv = length(explanatory)
variable.rows = rep("", 2*nv)
variable.rows[2*(1:nv)-1] = explanatory
df_table2 = data.frame(  Variable = c(variable.rows, "Accuracy", "Specificity", "Sensitivity")
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
row.names = c(row.names, "Accuracy", "Specificity", "Sensitivity")
row.names(df_table2) = row.names

# Calculate means of explanatory variables, for calculating marginal effects. Exclude Appropriability
explanatory.noapp = c("Incoming.Spillovers", explanatory.c) # Explanatory variables without Appropriability
df = df_data[, explanatory.noapp]
means = apply(df, MARGIN = 2, FUN = mean)
means.intercept = c(1, means) # Add 1 to multiply by intercept

# Create dataframe for marginal effects
df_marginal = data.frame(  Variable = explanatory.noapp
                         , Mean = round(means, 2)
                         , R2 = NA
                         , R4 = NA
                         , R2.10 = NA
                         , R4.10 = NA
                         )

# Stage 1 for 2SLS regressions 4-7: predict explanatory variables from instruments
# 2 sets of predicted values for Incoming.Spillovers: with and without Appropriability
# Incoming.Spillovers.Hat1 is predicted without Appropriability as instrument
# Incoming.Spillovers.Hat2 is predicted with Appropriability as instrument
mod1 = lm(data = df_data, formula = paste0("Incoming.Spillovers ~ ", paste(instruments.s, collapse = " + ")))
df_data$Incoming.Spillovers.Hat1 = predict.lm(object = mod1, newdata = df_data)
mod1 = lm(data = df_data, formula = paste0("Incoming.Spillovers ~ Appropriability + ", paste(instruments.s, collapse = " + ")))
df_data$Incoming.Spillovers.Hat2 = predict.lm(object = mod1, newdata = df_data)
mod1 = lm(data = df_data, formula = paste0("Appropriability ~ ", paste(instruments.a, collapse = " + ")))
df_data$Appropriability.Hat = predict.lm(object = mod1, newdata = df_data)

# Regression (1) Logit
# Cooperation.Bin on all explanatory variables
formula = paste("Cooperation.Bin ~ Incoming.Spillovers + Appropriability +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R1"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R1"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R1"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R1"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R1"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (2) Logit without Appropriability
# Cooperation.Bin on all explanatory variables EXCEPT Appropriability
formula = paste("Cooperation.Bin ~ Incoming.Spillovers +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R2"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R2"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R2"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R2"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R2"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)
# Marginal effects
b0b = df_coeff[, "Estimate"] # Coefficients including intercept
b = b0b[names(b0b) != "(Intercept)"] # Coefficients without intercept
z = sum(b0b * means.intercept) # Multiply coefficients x means
df_marginal[, "R2"] = round(exp(-z)/(1+exp(-z))^2 * b, 4)
df_marginal$R2.10 = df_marginal$R2 * 10

# Regression (3) 2-Step with Appropriability as Explanatory
# Same as (1) but two-stage
formula = paste("Cooperation.Bin ~ Incoming.Spillovers.Hat1 + Appropriability.Hat +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat1", "", row.names(df_coeff)) # Remove ".Hat1" suffix from row names.
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R3"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R3"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R3"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R3"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R3"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (4) 2-Step with Appropriability as Instrument
# Same as (2) but two-stage
formula = paste("Cooperation.Bin ~ Incoming.Spillovers.Hat2 +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat2", "", row.names(df_coeff)) # Remove ".Hat2" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R4"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R4"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R4"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R4"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R4"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)
# Marginal effects
b0b = df_coeff[, "Estimate"] # Coefficients including intercept
b = b0b[names(b0b) != "(Intercept)"] # Coefficients without intercept
z = sum(b0b * means.intercept) # Multiply coefficients x means
df_marginal[, "R4"] = round(exp(-z)/(1+exp(-z))^2 * b, 4)
df_marginal$R4.10 = df_marginal$R4 * 10

# Regression (5) Cooperation with Suppliers and Customers (2-Step)
# Dependent variable is Cooperation.Firms.Bin. Appropriability is excluded from the model.
formula = paste("Cooperation.Firms.Bin ~ Incoming.Spillovers.Hat2 +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat2", "", row.names(df_coeff)) # Remove ".Hat2" suffix from row names.
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R5"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R5"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R5"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R5"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R5"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Regression (6) Cooperation with Research Institutions (2-Step)
# Dependent variable is Cooperation.Universities.Bin. Appropriability is excluded from the model.
formula = paste("Cooperation.Universities.Bin ~ Incoming.Spillovers.Hat2 +", paste(explanatory.c, collapse = " + "))
mod = glm(data = df_data, formula = formula, family = binomial(link = "logit"))
df_coeff = summary(mod)$coefficients
row.names(df_coeff) = gsub(".Hat2", "", row.names(df_coeff)) # Remove ".Hat2" suffix from row names.
row.names(df_coeff) = gsub(".Hat", "", row.names(df_coeff)) # Remove ".Hat" suffix from row names.
var = setdiff(row.names(df_coeff), "(Intercept)")
for (v in var){
  print(v)
  p = df_coeff[v, 4]
  df_table2[paste0(v, ".Estimate"), "R6"] = paste(format(df_coeff[v, "Estimate"], digits = 4), asterisk(p))
  df_table2[paste0(v, ".Std.Error"), "R6"] = paste0("(", format(df_coeff[v, "Std. Error"], digits = 4), ")")
}
df_data$Cooperation.Prob = predict.glm(object = mod, newdata = df_data, type = "response")
df_data$Cooperation.Hat = ifelse(df_data$Cooperation.Prob > prob.threshold, 1, 0)
mat_confusion = table(df_data[, c("Cooperation.Bin", "Cooperation.Hat")])
df_table2["Accuracy", "R6"] = round((mat_confusion[1,1]+mat_confusion[2,2]) / sum(mat_confusion), 2)
df_table2["Specificity", "R6"] = round(mat_confusion[1,1]/sum(mat_confusion[1,]), 2)
df_table2["Sensitivity", "R6"] = round(mat_confusion[2,2]/sum(mat_confusion[2,]), 2)

# Format and save to results folder
names(df_table2) = c("Variable", "(1) Logit", "(2) Logit without Appropriability", "(3) 2-Step with Appropriability as Explanatory", "(4) 2-Step with Appropriability as Instrument",  "(5) Cooperation with Suppliers and Customers (2-Step)", "(6) Cooperation with Research Institutions (2-Step)")
names(df_marginal) = c("Variable", "Mean", "(2) Logit without Appropriability", "(4) (2-Step) with Appropriability as Explanatory", "(2) x10", "(4) (2-Step) x10")
df_table2$Variable = gsub(".Scaled", "", df_table2$Variable)
df_table2$Variable = gsub(".", " ", df_table2$Variable, fixed = T)
df_table2$Variable = gsub("RD", "R&D", df_table2$Variable, fixed = T)
names(df_table2) = gsub(".", " ", names(df_table2), fixed = T)
df_marginal$Variable = gsub(".", " ", df_marginal$Variable, fixed = T)
df_marginal$Variable = gsub("RD", "R&D", df_marginal$Variable, fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_table2, file = paste0(path.results, file.table2), firstRow = T, colWidths = 15, headerStyle = hs)
write.xlsx(df_marginal, file = paste0(path.results, file.marginal), firstRow = T, colWidths = 15, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)