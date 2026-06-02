# 1.3.1 Memahami Dataset
data_pembangunan <- read.csv("../dataset/pembangunan_wilayah_missing_outlier.csv")
("--- STRUKTUR DATA ---")
str(data_pembangunan)

("--- 6 BARIS PERTAMA DATASET ---")
head(data_pembangunan)

("--- SUMMARY ---")
summary(data_pembangunan)

dimensi <- dim(data_pembangunan)
cat("Jumlah Observasi (Baris) :", dimensi[1])
cat("Jumlah Variabel (Kolom)  :", dimensi[2])

# 1.3.2 Statistika Deskriptif
# Mengambil variabel numerik
data_numerik <- data_pembangunan[sapply(data_pembangunan, is.numeric)]

# Ringkasan statistik deskriptif
summary(data_numerik)

# Mean
sapply(data_numerik, mean, na.rm = TRUE)

# Median
sapply(data_numerik, median, na.rm = TRUE)

# Nilai minimum
sapply(data_numerik, min, na.rm = TRUE)

# Nilai maksimum
sapply(data_numerik, max, na.rm = TRUE)

# Standar deviasi
sapply(data_numerik, sd, na.rm = TRUE)

# Varians
sapply(data_numerik, var, na.rm = TRUE)

# Kuartil
sapply(data_numerik, quantile, na.rm = TRUE)
