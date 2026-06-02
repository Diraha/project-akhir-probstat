# ======================
# 1.3.1 Memahami Dataset
# ======================
data_pembangunan <- read.csv("./probstat/dataset/pembangunan_wilayah_missing_outlier.csv")
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
data_pembangunan <- data_pembangunan[complete.cases(data_pembangunan[, c("pdrb_perkapita", "kemiskinan", "pengangguran", "ipm", "akses_internet", "jalan_baik")]), ]

("--- Memeriksa kembali apakah jumlah baris dataset sudah diperbarui setelah penghapusan ---")
print(nrow(data_pembangunan))
