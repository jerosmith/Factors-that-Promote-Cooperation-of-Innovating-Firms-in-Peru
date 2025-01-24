# GENERAR SECTOR MASTER TABLE
# 0 sec

# Clear memory and start stopwatch
rm(list = objects())
t0 = Sys.time()

# Parameters
path.source = "../Data/3 Original csv/"
path.intermediate = "../Data/5 Intermediate/"
sector.csv = "ISIC_Rev_4_english_structure.csv"
master.sector.csv = "master_sector.csv"

# start stopwatch
t0 = Sys.time()

# 1. Load source master table, and only keep codes <= length 2
df_sector = read.csv(paste0(path.source, sector.csv), sep = ",", stringsAsFactors = F, fileEncoding = "Latin1")
df_sector = df_sector[nchar(df_sector$Code)<=2, ]

# 2. Create and populate Group column
df_sector$Group = NA
nr = nrow(df_sector)
if (toupper(df_sector$Code[1]) %in% LETTERS){
  g = paste(df_sector$Code[1], df_sector$Title[1])
}
i = 2
while (i <= nr){
  while (i <= nr & !df_sector$Code[i] %in% LETTERS){
    print(paste0(round(i/nr*100, 0), "%"))
    df_sector$Group[i] = g
    i = i +1
  }
  if (i <= nr){
    g = paste(df_sector$Code[i], df_sector$Title[i])
    i = i +1
  }
}
df_sector = df_sector[!is.na(df_sector$Group), ]

# Save
write.table(df_sector, paste0(path.intermediate, master.sector.csv), sep = ";", row.names = F)

# Time taken
print(Sys.time() - t0)
