# Bagian : 1.3.5 Visualisasi Data
#          1.3.6 Analisis Probabilitas dan Distribusi Data


library(ggplot2)
data <- read.csv(
  "data/pembangunan_wilayah_missing_outlier.csv"
)

#bersihin Missing Value

data_clean <- na.omit(data)

cat("Jumlah data sebelum pembersihan :", nrow(data), "\n")
cat("Jumlah data setelah pembersihan :", nrow(data_clean), "\n")

# =====================================================
# 1.3.5 VISUALISASI DATA
# =====================================================

# Grafik 1 : Histogram IPM

ggplot(data_clean, aes(x = ipm)) +
  geom_histogram(binwidth = 3) +
  labs(
    title = "Distribusi Nilai IPM",
    x = "IPM",
    y = "Frekuensi"
  )

# Grafik 2 : Histogram Kemiskinan

ggplot(data_clean, aes(x = kemiskinan)) +
  geom_histogram(binwidth = 2) +
  labs(
    title = "Distribusi Tingkat Kemiskinan",
    x = "Kemiskinan (%)",
    y = "Frekuensi"
  )

# Grafik 3 : Boxplot PDRB Per Kapita

ggplot(data_clean, aes(y = pdrb_perkapita)) +
  geom_boxplot() +
  labs(
    title = "Boxplot PDRB Per Kapita",
    y = "PDRB Per Kapita"
  )

# Grafik 4 : Scatter Plot Kemiskinan vs IPM

ggplot(data_clean,
       aes(
         x = kemiskinan,
         y = ipm
       )) +
  geom_point() +
  labs(
    title = "Hubungan Kemiskinan dan IPM",
    x = "Kemiskinan (%)",
    y = "IPM"
  )

# Grafik 5 : Scatter Plot Akses Internet
#             vs Rata Lama Sekolah

ggplot(data_clean,
       aes(
         x = akses_internet,
         y = rata_lama_sekolah
       )) +
  geom_point() +
  labs(
    title = "Hubungan Akses Internet dan Rata Lama Sekolah",
    x = "Akses Internet (%)",
    y = "Rata Lama Sekolah (Tahun)"
  )

# =====================================================
# 1.3.6 ANALISIS DISTRIBUSI DATA
# =====================================================

# Uji Normalitas Shapiro-Wilk

shapiro_ipm <- shapiro.test(data_clean$ipm)
shapiro_kemiskinan <- shapiro.test(data_clean$kemiskinan)
shapiro_internet <- shapiro.test(data_clean$akses_internet)

cat("\n")
cat("===================================\n")
cat("HASIL UJI NORMALITAS\n")
cat("===================================\n")

print(shapiro_ipm)
print(shapiro_kemiskinan)
print(shapiro_internet)

# QQ Plot IPM

qqnorm(data_clean$ipm,
       main = "QQ Plot IPM")
qqline(data_clean$ipm)

# QQ Plot Kemiskinan

qqnorm(data_clean$kemiskinan,
       main = "QQ Plot Kemiskinan")
qqline(data_clean$kemiskinan)

# =====================================================
# ANALISIS PROBABILITAS
# =====================================================

# Probabilitas Kemiskinan
# di atas rata-rata

mean_kemiskinan <- mean(data_clean$kemiskinan)

prob_kemiskinan_atas_rata <- mean(
  data_clean$kemiskinan > mean_kemiskinan
)

cat("\n")
cat("===================================\n")
cat("PROBABILITAS KEMISKINAN\n")
cat("===================================\n")

cat(
  "Probabilitas wilayah memiliki kemiskinan di atas rata-rata = ",
  round(prob_kemiskinan_atas_rata,4),
  "\n"
)

# Probabilitas IPM > 75

prob_ipm_tinggi <- mean(
  data_clean$ipm > 75
)

cat(
  "Probabilitas wilayah memiliki IPM > 75 = ",
  round(prob_ipm_tinggi,4),
  "\n"
)

# Probabilitas Normal pakai pnorm()
# P(IPM < 75)

prob_normal_ipm <- pnorm(
  75,
  mean(data_clean$ipm),
  sd(data_clean$ipm)
)

cat(
  "Probabilitas IPM kurang dari 75 (Distribusi Normal) = ",
  round(prob_normal_ipm,4),
  "\n"
)

# Nilai IPM pada persentil ke-95
# pakai qnorm()

ipm_p95 <- qnorm(
  0.95,
  mean(data_clean$ipm),
  sd(data_clean$ipm)
)

cat(
  "Nilai IPM pada persentil ke-95 = ",
  round(ipm_p95,2),
  "\n"
)

# =====================================================
# TABEL RINGKAS HASIL UJI NORMALITAS
# =====================================================

hasil_normalitas <- data.frame( 
  Variabel = c( 
    "IPM", "Kemiskinan" 
  ), 
  P_Value = c( 
    shapiro_ipm$p.value, 
    shapiro_kemiskinan$p.value 
  ) 
)

hasil_normalitas$Kesimpulan <-
  ifelse(
    hasil_normalitas$P_Value > 0.05,
    "Berdistribusi Normal",
    "Tidak Berdistribusi Normal"
  )

cat("\n")
cat("===================================\n")
cat("TABEL HASIL UJI NORMALITAS\n")
cat("===================================\n")

print(hasil_normalitas)
