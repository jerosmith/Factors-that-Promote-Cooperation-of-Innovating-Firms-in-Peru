# PROBABILITY PLOTS WITH BINARY VARIABLES
# 31 sec

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
path.plots = "../Results/Probability Plots/"
file.metadata = "Metadata P09.xlsx"
file.database = "Cooperation Peru.xlsx"
alpha = 0.05
z = -qnorm(alpha/2)
nudge.up = 0.02
line.wd = 0.45
y.axis.ub = 0.6

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Calculated Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 2. Make plots
dummy = df_variables[df_variables$Role.in.Dataset %in% c("Explanatory", "Instrument") & df_variables$Data.Type=="Dummy", "Variable.R"]
for (d in dummy){
  print(d)
  df = df_data[, c("Cooperation.Bin", d)]
  names(df)[2] = "x"
  df = sqldf("select avg([Cooperation.Bin]) as [Cooperation.Prob], x, count(*) as n from df group by x")
  df$Standard.Error = sqrt(df$Cooperation.Prob*(1 - df$Cooperation.Prob)/df$n)
  df$Margin.Error = df$Standard.Error*z
  df$lb = ifelse(df$Cooperation.Prob - df$Margin.Error >= 0, df$Cooperation.Prob - df$Margin.Error, 0)
  df$ub = ifelse(df$Cooperation.Prob + df$Margin.Error <= 1, df$Cooperation.Prob + df$Margin.Error, 1)
  g = ggplot(data = df, mapping = aes(x=x)) + xlab(d) +
      ylab("Cooperation") +
      scale_y_continuous(breaks = seq(0, y.axis.ub, 0.1), limits = c(0, y.axis.ub)) +
      scale_x_continuous(breaks = c(0, 1)) +
      geom_col(mapping = aes(y=ub), fill="blue", alpha=0.5) +
      geom_col(mapping = aes(y=lb), fill="blue") +
      geom_segment(mapping = aes(x = -line.wd, xend = line.wd, y=df$Cooperation.Prob[1]), color="red") +
      geom_segment(mapping = aes(x = 1-line.wd, xend = 1+line.wd, y=df$Cooperation.Prob[2]), color="red") +
      annotate("text", x=0, y=df$Cooperation.Prob[1]+nudge.up, label="Point Estimate", color="red") +
      annotate("text", x=1, y=df$Cooperation.Prob[2]+nudge.up, label="Point Estimate", color="red") +
      annotate("text", x=0, y=df$ub[1]+2*nudge.up, label="95% Conf.Interval", color="blue", alpha=0.5) +
      annotate("text", x=1, y=df$ub[2]+nudge.up, label="95% Conf.Interval", color="blue", alpha=0.5)
  ggsave(plot = g, filename = paste0(path.plots, "Cooperation.Bin vs ", d, ".png"), units = "px", height = 1000, width = 1200)
}

# Show time taken
print(Sys.time() - t0)
