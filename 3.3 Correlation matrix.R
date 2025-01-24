# CREATE CORRELATION MATRIX AND STORE IN EXCEL
# Helps to understand the variables
# Multicollinearity
# Instrumental variables
# 3 sec

# Packages
library(openxlsx)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.database = "../Database/"
path.result = "../Results/"
file.metadata = "Metadata P09.xlsx"
file.database = "Cooperation Peru.xlsx"
file.result = "Correlation Matrix.xlsx"

# 1. Load metadata and data
df_variables = read.xlsx(paste0(path.metadata, file.metadata), sheet = "Calculated Variables")
df_data = read.xlsx(paste0(path.database, file.database))

# 2. Select variables for correlation table
select.cols = df_variables[!is.na(df_variables$Table.1) & df_variables$Data.Type=="Continuous", "Variable.R"]
for (c in select.cols){ # Remove columns with zero standard deviation
  if (sd(df_data[, c], na.rm = T) == 0){
    select.cols = setdiff(select.cols, c)
  }
}
df_data = df_data[, select.cols]

# 3. Create correlation table
df_corr = as.data.frame(cor(df_data, use = "complete.obs"))
df_corr = round(abs(df_corr), 2)
n = nrow(df_corr)
for (i in 1:n){
  df_corr[i, i] = NA
}
names(df_corr) = gsub(".", " ", names(df_corr), fixed = T)
row.names(df_corr) = gsub(".", " ", row.names(df_corr), fixed = T)

# Save
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_corr, file = paste0(path.result, file.result), firstRow = T, rowNames = T, colWidths = 12, headerStyle = hs)

# Show time taken
print(Sys.time() - t0)

