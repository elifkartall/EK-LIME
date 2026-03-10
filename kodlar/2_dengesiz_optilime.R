# Veriseti dengesiz, optilime uygulandı
library(caret)
library(randomForest)
library(dplyr)

#--------------------------------------------------
# 1. Veri Standardizasyonu
#--------------------------------------------------
standardize_Class <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  
  if (nlevels(y) != 2) {
    stop("Target değişkeni binary değil.")
  }
  
  freq <- table(y)
  minority_class <- names(freq)[which.min(freq)]
  
  data[[target_col]] <- factor(
    ifelse(y == minority_class, "1", "0"),
    levels = c("0", "1")
  )
  
  return(data)
}

#--------------------------------------------------
# 2. OptiLIME v3 (Sınıf Başına 100 Gözlem)
#--------------------------------------------------
run_optilime_v3 <- function(data, dataset_name, 
                            target_col = "Class", 
                            target_r2 = 0.9, 
                            K = 5) {
  
  set.seed(123)
  
  # Imbalance Ratio Hesaplama
  class_freq <- table(data[[target_col]])
  n_majority <- as.numeric(class_freq["0"])
  n_minority <- as.numeric(class_freq["1"])
  IR <- n_majority / n_minority
  
  # Train/Test Split
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_raw <- data[train_idx, ]
  test_raw  <- data[-train_idx, ]
  
  # Ölçekleme
  numeric_cols <- sapply(train_raw, is.numeric)
  numeric_cols[target_col] <- FALSE
  
  preproc <- preProcess(train_raw[, numeric_cols], 
                        method = c("center", "scale"))
  
  train_scaled <- predict(preproc, train_raw)
  test_scaled  <- predict(preproc, test_raw)
  
  # Model Matrix ve RF Eğitimi
  formula_str <- as.formula(paste(target_col, "~ ."))
  X_train <- model.matrix(formula_str, data = train_scaled)[,-1]
  Y_train <- train_scaled[[target_col]]
  
  rf_model <- randomForest(x = X_train, y = Y_train, ntree = 100)
  
  X_test_all <- model.matrix(formula_str, data = test_scaled)[,-1]
  Y_test_all <- test_scaled[[target_col]]
  
  #--------------------------------------------------
  # GÜNCELLEME: Her sınıftan 100 gözlem seçimi
  #--------------------------------------------------
  idx0 <- which(Y_test_all == "0")
  idx1 <- which(Y_test_all == "1")
  
  # Eğer sınıfta 100'den az veri varsa hepsini alır, yoksa 100 tanesini örnekler
  selected_indices <- c(
    sample(idx0, min(100, length(idx0))),
    sample(idx1, min(100, length(idx1)))
  )
  
  results_list <- list()
  coef_list <- list()
  var_list  <- list()
  class_list <- c()
  
  #-----------------------------
  # LIME Döngüsü
  #-----------------------------
  for (k in seq_along(selected_indices)) {
    
    i <- selected_indices[k]
    ref_point <- X_test_all[i, , drop = FALSE]
    true_label <- as.character(Y_test_all[i])
    
    # Yerel örnekleme
    n_samples <- 500
    sampled_points <- as.data.frame(
      matrix(rnorm(n_samples * ncol(X_train)), nrow = n_samples)
    )
    colnames(sampled_points) <- colnames(X_train)
    
    y_probs <- predict(rf_model, sampled_points, type = "prob")[,"1"]
    
    # OptiLIME Loss
    calculate_loss <- function(kw) {
      dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
      weights <- exp(-(dists^2) / kw)
      
      if (sd(y_probs) < 1e-10) return(target_r2)
      
      fit <- lm(y ~ ., data = cbind(sampled_points, y = y_probs), weights = weights)
      r2 <- summary(fit)$r.squared
      
      if (is.na(r2)) return(0)
      if (r2 <= target_r2) return(r2) else return(2 * target_r2 - r2)
    }
    
    opt_res <- optimize(calculate_loss, interval = c(0.01, 10), maximum = TRUE)
    best_kw <- opt_res$maximum
    
    # Final Model
    dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
    weights <- exp(-(dists^2) / best_kw)
    final_fit <- lm(y ~ ., data = cbind(sampled_points, y = y_probs), weights = weights)
    
    # Feature Selection (K=5)
    betas <- coef(final_fit)[-1]
    betas[is.na(betas)] <- 0
    top_features <- names(sort(abs(betas), decreasing = TRUE)[1:min(K, length(betas))])
    
    final_betas <- betas
    final_betas[!(names(final_betas) %in% top_features)] <- 0
    
    coef_list[[k]] <- final_betas
    var_list[[k]]  <- top_features
    class_list[k]  <- true_label
    
    results_list[[k]] <- data.frame(
      Sınıf = true_label,
      Fidelity = opt_res$objective,
      KW = best_kw,
      stringsAsFactors = FALSE
    )
  }
  
  # Stability Hesaplama (VSI & CSI)
  compute_stability <- function(indices) {
    if (length(indices) < 2) return(list(VSI = NA, CSI = NA))
    jaccard_vals <- c(); cosine_vals  <- c()
    
    for (i in 1:(length(indices)-1)) {
      for (j in (i+1):length(indices)) {
        A <- var_list[[indices[i]]]; B <- var_list[[indices[j]]]
        union_len <- length(union(A, B))
        jaccard <- if(union_len > 0) length(intersect(A, B)) / union_len else 0
        jaccard_vals <- c(jaccard_vals, jaccard)
        
        b1 <- coef_list[[indices[i]]]; b2 <- coef_list[[indices[j]]]
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        cos_sim <- if(denom > 0) sum(b1*b2)/denom else 0
        cosine_vals <- c(cosine_vals, cos_sim)
      }
    }
    return(list(VSI = mean(jaccard_vals, na.rm = TRUE), CSI = mean(cosine_vals, na.rm = TRUE)))
  }
  
  final_df <- do.call(rbind, results_list)
  summary_stats <- final_df %>%
    group_by(Sınıf) %>%
    summarise(
      Ortalama_Fidelity = mean(Fidelity),
      Ortalama_KW = mean(KW),
      Gozlem_Sayisi = n(),
      .groups = "drop"
    )
  
  stab0 <- compute_stability(which(class_list == "0"))
  stab1 <- compute_stability(which(class_list == "1"))
  
  summary_stats$VSI <- NA; summary_stats$CSI <- NA
  if ("0" %in% summary_stats$Sınıf) {
    summary_stats$VSI[summary_stats$Sınıf == "0"] <- stab0$VSI
    summary_stats$CSI[summary_stats$Sınıf == "0"] <- stab0$CSI
  }
  if ("1" %in% summary_stats$Sınıf) {
    summary_stats$VSI[summary_stats$Sınıf == "1"] <- stab1$VSI
    summary_stats$CSI[summary_stats$Sınıf == "1"] <- stab1$CSI
  }
  
  summary_stats$Imbalance_Ratio <- IR
  summary_stats <- summary_stats %>% select(Imbalance_Ratio, everything())
  
  cat("\n---", dataset_name, "(Max 100 Gözlem/Sınıf | IR =", round(IR,3), "| K =", K, ") ---")
  print(summary_stats)
  
  return(summary_stats)
}

#--------------------------------------------------
# 3. Çalıştırma Döngüsü
#--------------------------------------------------
dataset_names <- c(
  "abalone", "abalone_19", "churn", "jm1", "kc1",
  "MagicTelescope", "mammography", "ozone_level_8hr",
  "pc1", "pc3", "pc4", "phoneme", "spambase",
  "SpeedDating", "steel_plates_fault", "us_crime",
  "wilt", "wine_quality", "yeast_me2", "yeast_ml8")

# Sonuçları biriktirmek için boş bir liste oluştur
all_results_list <- list()

for (name in dataset_names) {
  if (exists(name)) {
    tryCatch({
      data_prepared <- standardize_Class(get(name), "Class")
      # Fonksiyonun döndürdüğü summary_stats'ı listeye kaydet
      all_results_list[[name]] <- run_optilime_v3(data_prepared, name, K = 5)
    }, error = function(e) {
      cat("\nHata (", name, "): ", e$message)
    })
  }
}

# Tüm listeyi tek bir veri çerçevesine birleştir
final_comparison_optilime <- do.call(rbind, all_results_list)

library(ggplot2)

library(ggplot2)
library(gridExtra)

library(ggplot2)
library(gridExtra)

# --- BİRİNCİ GRAFİK (Fidelity vs CSI) ---
ggplot(final_comparison_df, aes(x = Ortalama_Fidelity, y = CSI, color = Sınıf)) +
  geom_point(size = 4, alpha = 0.7) +
  theme_minimal() +
  labs(title = "OptiLIME: Fidelity vs Katsayı Kararlılığı (CSI)",
       x = "Ortalama Fidelity (Bağlılık)",
       y = "CSI (Kararlılık)")

# --- YAN YANA GRAFİKLER İÇİN HAZIRLIK ---

# Ortak Görsel Ayarlar
base_theme <- theme_minimal() + 
  theme(legend.position = "none", 
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9))

# Fonksiyon: Sınıf bazlı scatter plot oluşturucu (Etiketler ve Trend Line kaldırıldı)
create_class_plot <- function(df, target_class, y_var, title_prefix, color_hex) {
  subset_df <- df[df$Sınıf == target_class, ]
  
  ggplot(subset_df, aes_string(x = "Imbalance_Ratio", y = y_var)) +
    geom_point(size = 3, color = color_hex, alpha = 0.7) +
    # geom_smooth ve geom_text satırları tamamen kaldırıldı
    labs(title = paste0(title_prefix, " (Sınıf ", target_class, ")"),
         x = "Imbalance Ratio", y = y_var) +
    base_theme
}

# 1. Imbalance Ratio vs Kernel Width
p1_0 <- create_class_plot(final_comparison_df, "0", "Ortalama_KW", "1. IR vs KW", "#00BFC4")
p1_1 <- create_class_plot(final_comparison_df, "1", "Ortalama_KW", "1. IR vs KW", "#F8766D")

# 2. Imbalance Ratio vs VSI
p2_0 <- create_class_plot(final_comparison_df, "0", "VSI", "2. IR vs VSI", "#00BFC4")
p2_1 <- create_class_plot(final_comparison_df, "1", "VSI", "2. IR vs VSI", "#F8766D")

# 3. Imbalance Ratio vs CSI
p3_0 <- create_class_plot(final_comparison_df, "0", "CSI", "3. IR vs CSI", "#00BFC4")
p3_1 <- create_class_plot(final_comparison_df, "1", "CSI", "3. IR vs CSI", "#F8766D")

# 4. Imbalance Ratio vs Ortalama Fidelity (Sınıf 0)
p4_0 <- create_class_plot(final_comparison_df, "0", "Ortalama_Fidelity", "4. IR vs Fidelity", "#00BFC4")

# 5. Imbalance Ratio vs Ortalama Fidelity (Sınıf 1)
p5_1 <- create_class_plot(final_comparison_df, "1", "Ortalama_Fidelity", "5. IR vs Fidelity", "#F8766D")

# --- GRAFİKLERİ YAN YANA GÖRÜNTÜLEME ---

# Grafik 1: KW Kıyaslaması
grid.arrange(p1_0, p1_1, ncol = 2)

# Grafik 2: VSI Kıyaslaması
grid.arrange(p2_0, p2_1, ncol = 2)

# Grafik 3: CSI Kıyaslaması
grid.arrange(p3_0, p3_1, ncol = 2)

# Grafik 4 & 5: Fidelity Kıyaslaması
grid.arrange(p4_0, p5_1, ncol = 2)



install.packages("ggcorrplot")
install.packages("gridExtra")
library(ggcorrplot)
library(gridExtra)
library(dplyr)

# 1. Korelasyon hesaplanacak sayısal değişkenleri belirle
# (image_323c87.png dosyasındaki sütun isimlerine göre)
metrics <- c("Imbalance_Ratio", "Ortalama_Fidelity", "Ortalama_KW", "VSI", "CSI")

# 2. Sınıf 0 (Çoğunluk Sınıfı) için veri hazırlığı ve korelasyon
df_class0 <- final_comparison_df %>%
  filter(Sınıf == "0") %>%
  select(all_of(metrics))
corr_matrix0 <- cor(df_class0, use = "complete.obs")

# 3. Sınıf 1 (Azınlık Sınıfı) için veri hazırlığı ve korelasyon
df_class1 <- final_comparison_df %>%
  filter(Sınıf == "1") %>%
  select(all_of(metrics))
corr_matrix1 <- cor(df_class1, use = "complete.obs")

# 4. Sınıf 0 Grafiği
plot_0 <- ggcorrplot(corr_matrix0, 
                     hc.order = TRUE, 
                     type = "lower",
                     lab = TRUE, 
                     lab_size = 3.5,
                     method = "circle", 
                     colors = c("#E46726", "white", "#6D9EC1"),
                     title = "Sınıf 0 Korelasyon Matrisi",
                     ggtheme = theme_minimal()) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# 5. Sınıf 1 Grafiği
plot_1 <- ggcorrplot(corr_matrix1, 
                     hc.order = TRUE, 
                     type = "lower",
                     lab = TRUE, 
                     lab_size = 3.5,
                     method = "circle", 
                     colors = c("#E46726", "white", "#6D9EC1"),
                     title = "Sınıf 1 Korelasyon Matrisi",
                     ggtheme = theme_minimal()) +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5))

# 6. İki grafiği yan yana bastır
grid.arrange(plot_0, plot_1, ncol = 2)