# ======================
# 1.3.1 Memahami Dataset
# ======================
data_pembangunan <- read.csv("probstat/dataset/pembangunan_wilayah_missing_outlier.csv")
("--- STRUKTUR DATA ---")
str(data_pembangunan)

("--- 6 BARIS PERTAMA DATASET ---")
head(data_pembangunan)

("--- SUMMARY ---")
summary(data_pembangunan)

dimensi <- dim(data_pembangunan)
cat("Jumlah Observasi (Baris) :", dimensi[1])
cat("Jumlah Variabel (Kolom)  :", dimensi[2])

# ===========================
# 1.3.2 Statistika Deskriptif
# ===========================
("--- Mengambil variabel numerik ---")
data_numerik <- data_pembangunan[sapply(data_pembangunan, is.numeric)]

("--- Ringkasan statistik deskriptif ---")
summary(data_numerik)

("--- Mean ---")
sapply(data_numerik, mean, na.rm = TRUE)

("--- Median ---")
sapply(data_numerik, median, na.rm = TRUE)

("--- Nilai minimum ---")
sapply(data_numerik, min, na.rm = TRUE)

("--- Nilai maksimum ---")
sapply(data_numerik, max, na.rm = TRUE)

("--- Standar deviasi ---")
sapply(data_numerik, sd, na.rm = TRUE)

("--- Varians ---")
sapply(data_numerik, var, na.rm = TRUE)

("--- Kuartil ---")
sapply(data_numerik, quantile, na.rm = TRUE)

# ============================
# 1.3.3 Analisis Missing Value
# ============================
("--- Memeriksa jumlah missing value di tiap variabel ---")
missing_value <- colSums(is.na(data_pembangunan))

("--- Merepresentasikan jumlah missing value tiap variabel ke dalam persentase ---")
percentage_missing_value <- missing_value / nrow(data_pembangunan) * 100

("--- Menghitung rata-rata persentase missing value dataframe (secara keseluruhan) ---")
percentage_missing_value_df <- mean(is.na(data_pembangunan)) * 100

("--- Melihat hasil perhitungan persentase ---")
print(percentage_missing_value_df)

("--- Karena persentase 0.4% < 30%, maka metode yang diambil adalah metode penghapusan ---")
data_pembangunan <- na.omit(data_pembangunan)

("--- Memeriksa kembali apakah jumlah baris dataset sudah diperbarui setelah penghapusan ---")
print(nrow(data_pembangunan))

# ======================
# 1.3.4 Analisis Outlier
# ======================
("--- Mengambil kolom variabel yang bertipe data numerik ---")
numeric_columns_name <- names(data_pembangunan)[sapply(data_pembangunan, is.numeric)]

("--- Menghitung IQR tiap kolom numerik ---")
for(column_name in numeric_columns_name) {
  Q1 <- quantile(data_pembangunan[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data_pembangunan[[column_name]], 0.75, na.rm = TRUE)
  
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  total_outlier <- sum(data_pembangunan[[column_name]] < lower_bound | data_pembangunan[[column_name]] > upper_bound, na.rm = TRUE)
  
  cat(column_name, ":", total_outlier, "outlier\n")
}

("--- Membuat beberapa boxplot untuk visualisasi outlier ---")
par(mfrow = c(2, 3))

for(col in numeric_columns_name) {
  boxplot(data_pembangunan[[col]], main = col)
}


# ============================
# 1.3.7 Analisis Korelasi
# ============================
data_bersih <- data_pembangunan[complete.cases(data_pembangunan), ]
kolom_numerik_korelasi <- c("pdrb_perkapita", "kemiskinan", "pengangguran", "ipm", 
                            "harapan_hidup", "rata_lama_sekolah", "akses_internet", 
                            "jalan_baik", "air_bersih")

matriks_numerik <- data_bersih[, kolom_numerik_korelasi]

cat("--- Menghitung korelasi antar variabel ---")
matriks_korelasi <- cor(matriks_numerik, method = "pearson")
print(round(matriks_korelasi, 3))

cat("--- UJI KORELASI: IPM vs KEMISKINAN ---")
uji_ipm_kemiskinan <- cor.test(data_bersih$ipm, data_bersih$kemiskinan)
print(uji_ipm_kemiskinan)

cat("--- UJI KORELASI: AKSES INTERNET vs RATA-LAMA SEKOLAH ---")
uji_internet_sekolah <- cor.test(data_bersih$akses_internet, data_bersih$rata_lama_sekolah)
print(uji_internet_sekolah)

cat("--- UJI KORELASI: PDRB PERKAPITA vs IPM (KESEJAHTERAAN) ---")
uji_pdrb_ipm <- cor.test(data_bersih$pdrb_perkapita, data_bersih$ipm)
print(uji_pdrb_ipm)

