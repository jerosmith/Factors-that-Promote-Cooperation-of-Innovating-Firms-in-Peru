# SCATTERPLOTS
# 50 sec

# Packages
library(openxlsx)
library(sqldf)
library(ggplot2)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.database = "../Database/"
path.plots = "../Results/Scatterplots/"
file.metadata = "Metadata P09.xlsx"
file.database = "Cooperation Peru.xlsx"
alpha = 0.05
z = -qnorm(alpha/2)

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Calculated Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 2. Make plots
variable = df_variables[df_variables$Role.in.Dataset %in% c("Explanatory", "Instrument") & df_variables$Data.Type=="Numeric", "Variable.R"]
for (v in variable){
  print(v)
  df = df_data[, c("Cooperation", v)]
  names(df)[2] = "x"
  n = nrow(df)
  df$Cooperation = df$Cooperation + runif(n, -0.5, 0.5) # Create jitter for greater visibility
  df$x = df$x + runif(n, -0.5, 0.5) # Create jitter for greater visibility
  g = ggplot(data = df, mapping = aes(y=Cooperation, x=x)) + xlab(v) +
    geom_point(size=0.5) +
    geom_smooth(method = "lm")
  ggsave(plot = g, filename = paste0(path.plots, "Cooperation vs ", v, ".png"))
}

# Show time taken
print(Sys.time() - t0)
