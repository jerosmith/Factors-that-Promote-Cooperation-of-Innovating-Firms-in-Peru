# REGRESSIONS - OLS & IV

# Packages
library(openxlsx)
library(AER)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.database = "../Database/"
file.metadata = "Metadata P09.xlsx"
file.database = "Cooperation Peru.xlsx"

# Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Calculated Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 1. OLS Cooperation ~ Appropriability
mod = lm(data = df_data, formula = Cooperation ~ Innovation + Incentives + Basicness.RD + Appropriability)
summary(mod)

# 2. OLS Cooperation ~ All explanatory variables
explanatory = df_variables[df_variables$Role.in.Dataset=="Explanatory" & !df_variables$Data.Type %in% c("Dummy", "Categorical"), "Variable.R"]
explanatory = paste(explanatory, collapse = " + ")
formula = paste0("Cooperation ~ ", explanatory)
mod = lm(data = df_data, formula = formula)
summary(mod)

# 3. OLS Cooperation ~ Significant variables
mod = lm(data = df_data, formula = Cooperation ~ Innovation + Intensity.Innovation + Incentives + Finance.Private + Public.Aid.Applied + Workers + Appropriability + Brand + Patent + Copyright + Obstacle.Environment + Obstacle.Market + Obstacle.Cost + Basicness.RD)
summary(mod)
