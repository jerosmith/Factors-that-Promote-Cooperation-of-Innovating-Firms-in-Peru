# GENERATE DATABASE OBS TABLE
# 7 sec

# Packages
library(openxlsx)
library(sqldf)

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
path.source = "../Data/3 Original csv/"
path.intermediate = "../Data/5 Intermediate/"
path.database = "../Database/"
metadata.xlsx = "Metadata P09.xlsx"
data1.csv = "INNOVACION_2018_I.csv"
data2.csv = "INNOVACION_2018_II.csv"
master.sector.csv = "master_sector.csv"
obs.table.xlsx = "Cooperation Peru.xlsx"

# start stopwatch
t0 = Sys.time()

# 1. Load metadata and data, select columns
df_metadata_data = read.xlsx(paste0(path.metadata, metadata.xlsx), sheet = "Data Variables")
df_metadata_formula = read.xlsx(paste0(path.metadata, metadata.xlsx), sheet = "Calculated Variables")
df_metadata_formula$R.Code = gsub("&lt;", "<", df_metadata_formula$R.Code, fixed = T) # Correct formulae not read correctly from Excel formulae
df_metadata_formula$R.Code = gsub("&gt;", ">", df_metadata_formula$R.Code, fixed = T) # Correct formulae not read correctly from Excel formulae
df_metadata_formula$SQL.Code = gsub("&lt;", "<", df_metadata_formula$SQL.Code, fixed = T) # Correct formulae not read correctly from Excel formulae
df_metadata_formula$SQL.Code = gsub("&gt;", ">", df_metadata_formula$SQL.Code, fixed = T) # Correct formulae not read correctly from Excel formulae
df_sector = read.table(paste0(path.intermediate, master.sector.csv), head=T, sep = ";")
df_data1 = read.csv2(paste0(path.source, data1.csv))
df_data2 = read.csv2(paste0(path.source, data2.csv))

# 2. Ensure that numeric variables are actually numeric
numeric = df_metadata_data[df_metadata_data$Data.Type == "N", "Variable"]
numeric1 = intersect(numeric, names(df_data1))
numeric2 = intersect(numeric, names(df_data2))
for (v in numeric1){
  print(v)
  df_data1[, v] = as.numeric(df_data1[, v])
}
for (v in numeric2){
  print(v)
  df_data2[, v] = as.numeric(df_data2[, v])
}

# 3. In 3-value variables, convert 2, 3 and NA to 0
var = df_metadata_data[  !is.na(df_metadata_data$Data.Type) 
                  & !is.na(df_metadata_data$Length) 
                  & !is.na(df_metadata_data$Range) 
                  & df_metadata_data$Data.Type == "N"
                  & df_metadata_data$Length == 1
                  & df_metadata_data$Range %in% c("(0:1)", "(1:2)", "(1:3)")
                  & df_metadata_data$Label != "1.La empresa | 2.Mercado Nacional | 3.Mercado Internacional"
                  , "Variable"
                  ]
var1 = intersect(var, names(df_data1))
var2 = intersect(var, names(df_data2))
for (v in var1){
  print(v)
  df_data1[, v] = ifelse(df_data1[, v] > 1 | is.na(df_data1[, v]) | df_data1[, v] == 0, 0, 1)
}
for (v in var2){
  print(v)
  df_data2[, v] = ifelse(df_data2[, v] > 1 | is.na(df_data2[, v]) | df_data2[, v] == 0, 0, 1)
}
# View(df_data1[, var])

# 4. In other variables, convert NA to appropriate value.
var = df_metadata_data[df_metadata_data$Label == "1.Alta | 2.Media | 3.Baja | 4.Ninguna", "Variable"]
var1 = intersect(var, names(df_data1))
var2 = intersect(var, names(df_data2))
for (v in var1){
  print(v)
  df_data1[, v] = ifelse(is.na(df_data1[, v]), 4, df_data1[, v])
}
for (v in var2){
  print(v)
  df_data2[, v] = ifelse(is.na(df_data2[, v]), 4, df_data2[, v])
}

# 5. In numeric variables for quantities, convert NA to 0
var = df_metadata_data[df_metadata_data$Data.Type == "N" & df_metadata_data$Length >= 5, "Variable"]
var1 = intersect(var, names(df_data1))
var2 = intersect(var, names(df_data2))
for (v in var1){
  print(v)
  df_data1[, v] = ifelse(is.na(df_data1[, v]), 0, df_data1[, v])
}
for (v in var2){
  print(v)
  df_data2[, v] = ifelse(is.na(df_data2[, v]), 0, df_data2[, v])
}

# 6. Likewise for percentage variables, convert NA to 0
var = df_metadata_data[df_metadata_data$Data.Type == "N" & df_metadata_data$Length == 3, "Variable"]
var1 = intersect(var, names(df_data1))
var2 = intersect(var, names(df_data2))
for (v in var1){
  print(v)
  df_data1[, v] = ifelse(is.na(df_data1[, v]), 0, df_data1[, v])
}
for (v in var2){
  print(v)
  df_data2[, v] = ifelse(is.na(df_data2[, v]), 0, df_data2[, v])
}

# 7. Get Sector division
df_data1$Code = substr(df_data1$C2P2_1_9_COD, 1, 2)
df_data1 = merge(df_data1, df_sector, all.x = T)

# 8. Calculate all variables
df_db = data.frame(NCUEST = df_data1[, "NCUEST"])
nphase = max(df_metadata_formula$Calculation.Phase, na.rm = T)
for (ph in 1:nphase){
  # R Code
  r.code = df_metadata_formula[df_metadata_formula$Calculation.Phase == ph & df_metadata_formula$Language=="R" & !is.na(df_metadata_formula$R.Code), "R.Code"]
  for (r in r.code){
    print(paste("Phase", ph, r))
    eval(parse(text = r))
  }
  # SQL Code
  sql.code = df_metadata_formula[df_metadata_formula$Calculation.Phase == ph & df_metadata_formula$Language=="SQL" & !is.na(df_metadata_formula$SQL.Code), "SQL.Code"]
  if (length(sql.code > 0)){
    #sql.code = gsub("&lt;=", "<=", sql.code, fixed = T)
    #sql.code = gsub("&gt;=", ">=", sql.code, fixed = T)
    sql.code = paste("select NCUEST,", sql.code, "group by NCUEST")
    nc0 = ncol(df_db)
    for (s in sql.code){
      print(paste("Phase", ph, s))
      df_data2_groupby = sqldf(s)
      df_db = merge(df_db, df_data2_groupby, all.x = T)
    }
    nc1 = ncol(df_db)
    for (j in (nc0+1):nc1){
      df_db[, j][is.na(df_db[, j])] = 0
    }
  }
}

# 9. Keep only columns that have been defined to include in DB and sort rows
column.id = df_metadata_formula[df_metadata_formula$Role.in.Dataset != "Intermediate" , "Variable.R"]
df_db = df_db[, column.id]
df_db = df_db[order(df_db$NCUEST), ] # Rows

# 10. Save to database
names(df_db) = gsub(".", " ", names(df_db), fixed = T)
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_db, file = paste0(path.database, obs.table.xlsx), headerStyle = hs, firstRow = T, colWidths = 15)

# Show time taken
print(Sys.time() - t0)
