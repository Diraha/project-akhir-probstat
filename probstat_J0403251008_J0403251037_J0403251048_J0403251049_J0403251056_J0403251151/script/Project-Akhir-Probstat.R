library(naniar)

# ======================
# 1.3.1 Memahami Dataset
# ======================
data_pembangunan <- read.csv("probstat_J0403251008_J0403251037_J0403251048_J0403251049_J0403251056_J0403251151/dataset/pembangunan_wilayah_missing_outlier.csv")
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
round(sapply(data_numerik, mean, na.rm = TRUE), 2)

("--- Median ---")
round(sapply(data_numerik, median, na.rm = TRUE), 2)

("--- Nilai minimum ---")
round(sapply(data_numerik, min, na.rm = TRUE), 2)

("--- Nilai maksimum ---")
round(sapply(data_numerik, max, na.rm = TRUE), 2)

("--- Standar deviasi ---")
round(sapply(data_numerik, sd, na.rm = TRUE), 2)

("--- Varians ---")
round(sapply(data_numerik, var, na.rm = TRUE), 2)

("--- Kuartil ---")
round(sapply(data_numerik, quantile, na.rm = TRUE), 2)

# ============================
# 1.3.3 Analisis Missing Value
# ============================
("--- Memeriksa jumlah missing value di tiap variabel ---")
missing_value <- colSums(is.na(data_pembangunan))

("--- Merepresentasikan jumlah missing value tiap variabel ke dalam persentase ---")
percentage_missing_value <- missing_value / nrow(data_pembangunan) * 100

("--- Melihat hasil perhitungan persentase ---")
print(percentage_missing_value)

("-- Mengambil seluruh variabel numerik --")
data_num <- data_pembangunan[, sapply(data_pembangunan, is.numeric)]

("-- Menguji variabel missing value, apakah MCAR atau bukan --")
hasil_uji_mcar <- mcar_test(data_num)

("-- Melihat hasil uji MCAR --")
print(hasil_uji_mcar)

("--- Karena nilai missing value terbukti MCAR (0.515 > 0.05), maka digunakan metode penghapusan ---")
data_pembangunan <- na.omit(data_pembangunan)

("--- Memeriksa kembali apakah jumlah baris dataset sudah diperbarui setelah penghapusan ---")
print(nrow(data_pembangunan))

# ======================
# 1.3.4 Analisis Outlier
# ======================
("--- Mengambil kolom variabel yang bertipe data numerik ---")
numeric_columns_name <- names(data_pembangunan)[sapply(data_pembangunan, is.numeric)]

outlier_columns <- c()

("--- Menghitung IQR tiap kolom numerik ---")
for(column_name in numeric_columns_name) {
  ("--- Mengambil kolom variabel yang bertipe data numerik ---")
  numeric_columns_name <- names(data_pembangunan)[sapply(data_pembangunan, is.numeric)]
  
  outlier_columns <- c()
  
  ("--- Menghitung IQR tiap kolom numerik ---")
  for(column_name in numeric_columns_name) {
  
    Q1 <- quantile(data_pembangunan[[column_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data_pembangunan[[column_name]], 0.75, na.rm = TRUE)
  
    IQR <- Q3 - Q1
  
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
  
    outlier_values <- data_pembangunan[[column_name]][
      data_pembangunan[[column_name]] < lower_bound |
      data_pembangunan[[column_name]] > upper_bound
    ]
  
    total_outlier <- length(outlier_values)
  
    cat("\n====================================\n")
    cat("Variabel :", column_name, "\n")
    cat("Lower Bound:", lower_bound, "\n")
    cat("Upper Bound:", upper_bound, "\n")
    cat("Jumlah Outlier :", total_outlier, "\n")
  
    if(total_outlier > 0) {
  
      outlier_columns <- c(outlier_columns, column_name)
  
      cat("\nNilai Outlier:\n")
      print(outlier_values)
  
      mean_sebelum <- mean(
        data_pembangunan[[column_name]],
        na.rm = TRUE
      )
  
      data_tanpa_outlier <- data_pembangunan[[column_name]][
        data_pembangunan[[column_name]] >= lower_bound &
        data_pembangunan[[column_name]] <= upper_bound
      ]
  
      mean_setelah <- mean(
        data_tanpa_outlier,
        na.rm = TRUE
      )
  
      cat("\nRata-rata Outlier :", mean(outlier_values), "\n")
      cat("Mean Sebelum :", mean_sebelum, "\n")
      cat("Mean Setelah :", mean_setelah, "\n")
      cat("Selisih Mean :", abs(mean_sebelum - mean_setelah), "\n")
    }
  }
}

("--- Membuat boxplot hanya untuk variabel yang memiliki outlier ---")
par(mfrow = c(1, length(outlier_columns)))

for(col in outlier_columns) {
  boxplot(
    data_pembangunan[[col]],
    main = col,
    ylab = col
  )
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

# Data hasil korelasi
data_korelasi <- data.frame(
  pasangan = c(
    "PDRB - Kemiskinan",
    "Pengangguran - PDRB",
    "IPM - PDRB",
    "Harapan Hidup - PDRB",
    "Akses Internet - Kemiskinan",
    "Akses Internet - IPM"
  ),
  korelasi = c(
    -0.0048,
    -0.0323,
    0.0111,
    0.0125,
    -0.0309,
    0.0020
  )
)

ggplot(data_korelasi,
       aes(x = reorder(pasangan, korelasi),
           y = korelasi)) +
  
  geom_col(width = 0.7, fill = "steelblue") +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             linewidth = 1,
             color = "red") +
  
  coord_flip() +
  
  ylim(-0.04, 0.02) +
  
  labs(
    title = "Visualisasi Nilai Korelasi Antar Variabel",
    x = "Pasangan Variabel",
    y = "Koefisien Korelasi Pearson"
  ) +
  
  theme_minimal(base_size = 12)

