# CONVERT ORIGINAL SPSS DATA FILES TO CSV
# 3 sec

# Libraries
library(haven)
library(data.table)

# Clear memory
rm(list = ls())

# Parameters
path_source = "../Data/2 Original SPSS/"
path_target = "../Data/3 Original csv/"
file_source = 'INNOVACION_2018_II.sav'
file_target = 'INNOVACION_2018_II.csv'

# Start stopwatch
t0 = Sys.time()

# Read and write
df_data = read_spss(paste0(path_source, file_source))
fwrite(df_data, file = paste0(path_target, file_target), sep = ";")

# Display elapsed time
print(Sys.time()-t0)
