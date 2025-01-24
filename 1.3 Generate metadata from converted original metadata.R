# EXTRACT METADATA FROM EXCEL Diccionario_Innovacion_2018_I.xlsx, Diccionario_Innovacion_2018_II.xlsx
# converted from PDF FILE Diccionario_Innovacion_2018_I.pdf, Diccionario_Innovacion_2018_II.pdf
# using PDF converter at https://www.ilovepdf.com/
# 1 sec

# Packages
library(openxlsx)

# Clear memory and start stpwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.metadata = "../Data/1 Metadata/"
file.original1 = "Diccionario_Innovacion_2018_I.xlsx"
file.original2 = "Diccionario_Innovacion_2018_II.xlsx"
file.metadata = "Metadata_Data.xlsx"
columns = c("Num", "Variable", "Description", "Label", "Data Type", "Length", "Range", "Omission", "Compulsory", "Double Digit")

# Read original files into respective dataframes
df_metadata1 = read.xlsx(paste0(path.metadata, file.original1), startRow = 3, cols = 1:10)
df_metadata2 = read.xlsx(paste0(path.metadata, file.original2), startRow = 3, cols = 1:10)

# Clean and format both dataframes
names(df_metadata1) = columns
names(df_metadata2) = columns
df_metadata1 = df_metadata1[!is.na(df_metadata1$Variable), ]
df_metadata2 = df_metadata2[!is.na(df_metadata2$Variable), ]
df_metadata1$Num = as.integer(df_metadata1$Num)
df_metadata2$Num = as.integer(df_metadata2$Num)
col.desc = c(  "¿Cuánto fue el monto invertido en el año 2015? Incluye horas – hombre dedicadas a la actividad (S/.)"
             , "¿Cuánto fue el monto invertido en el año 2016? Incluye horas – hombre dedicadas a la actividad (S/.)"
             , "¿Cuánto fue el monto invertido en el año 2017? Incluye horas – hombre dedicadas a la actividad (S/.)"
            )
col.desc = rep(col.desc, 8)
df_metadata1[df_metadata1$Description=="Equivalencia", "Description"] = col.desc

# Add source file column to each dataframe
df_metadata1$`Source File` = "I"
df_metadata2$`Source File` = "II"
nc = length(columns)
columns = c(columns[1], "Source File", columns[2:nc])
df_metadata1 = df_metadata1[, columns]
df_metadata2 = df_metadata2[, columns]

# Append (union) both dataframes into one
var2 = setdiff(df_metadata2$Variable, df_metadata1$Variable)
df_metadata2 = df_metadata2[df_metadata2$Variable %in% var2, ]
nr1 = nrow(df_metadata1)
nr2 = nrow(df_metadata2)
df_metadata2$Num = (nr1+1):(nr1+nr2)
df_metadata = rbind(df_metadata1, df_metadata2)

# Save
hs = createStyle(textDecoration = "bold", fgFill = "#DDEBF7", wrapText = T)
write.xlsx(df_metadata, file = paste0(path.metadata, file.metadata), headerStyle = hs, firstRow = T)

# Show time taken
print(Sys.time()-t0)
