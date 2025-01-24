# ANALYSE ORIGINAL DATA BEFORE GENERATING DATABASE

# Clear memory
rm(list = objects())

# Parameters
path.source = "../Data/3 Original csv/"
csv.file1 = "INNOVACION_2018_I.csv"
csv.file2 = "INNOVACION_2018_II.csv"

# 1. Load data
df_data1 = read.csv2(paste0(path.source, csv.file1))
df_data2 = read.csv2(paste0(path.source, csv.file2))

# Analyse data

# Rows
nrow(df_data1) # 2084
nrow(df_data2) # 1268

# Primary key
length(unique(df_data1$C2P2_1_1)) # 2084 firms
length(unique(df_data1$NCUEST)) # 2084 surveys
length(unique(df_data2$C2P2_1_1)) # 1156 firms
length(unique(df_data2$NCUEST)) # 1156 surveys
# In file1, the firm ID, C2P2_1_1, and the survey ID, NCUEST, are unique keys.
# In file2, there are duplicates of the firm ID.

# Inspect duplicates
df = as.data.frame(table(df_data2$NCUEST))
df = df[df$Freq >= 2, ]
df_data2[df_data2$NCUEST==101, c("NCUEST", "C9P2", "C9P3_1")]

# Is file 2 a subset of file 1?
length(intersect(df_data1$C2P2_1_1, df_data2$C2P2_1_1)) # 1145
# file 2 is almost a subset of file 1, except for 11 firms.

df_intersect = merge(df_data1, df_data2) # 1257
# The variable values of equivalent firms in both files are almost all the same.
length(setdiff(df_data1$C2P2_1_1, df_data2$C2P2_1_1)) # 939 firms are only in file1.
length(setdiff(df_data2$C2P2_1_1, df_data1$C2P2_1_1)) # 11 firms are only in file2.

# Verify that all firms in file2 are those in which C9P2=1

df = merge(df_data1, df_data2, by = "NCUEST", all.x = T)

# Conclusion: Use mainly file1 and just add the 11 additional firms from file 2 into file 1.

ncol(df_data1) # 530
ncol(df_data2) # 89

setdiff(names(df_data2), names(df_data1))
