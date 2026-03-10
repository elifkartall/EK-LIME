#--------------------------------------------------
# Ensemble Kernel LIME (EK-LIME) sb=3
#--------------------------------------------------

library(caret)
library(randomForest)
library(dplyr)
library(smotefamily) 
library(ggplot2)

# 1. Veri Standardizasyonu 
standardize_Class_smote_EK <- function(data, target_col = "Class") {
  data <- na.omit(data)
  
  # Target kontrolü
  if (!(target_col %in% colnames(data))) stop(paste("Hedef sütun bulunamadı:", target_col))
  
  y <- as.factor(data[[target_col]])
  if (nlevels(y) != 2) stop("Target değişkeni binary (2 sınıflı) değil.")
  
  freq <- table(y)
  minority_class <- names(freq)[which.min(freq)]
  
  # 0 ve 1 olarak normalize et
  data[[target_col]] <- factor(ifelse(y == minority_class, "1", "0"), levels = c("0", "1"))
  return(data)
}

# 2. Ana Fonksiyon
run_ensemble_kernel_v3_EK <- function(data, dataset_name, 
                                      target_col = "Class", 
                                      target_r2 = 0.9, 
                                      K = 5) {
  set.seed(123)
  
  # Imbalance Ratio
  class_freq_orig <- table(data[[target_col]])
  IR_original <- as.numeric(class_freq_orig["0"]) / as.numeric(class_freq_orig["1"])
  
  # Train/Test Split
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_raw <- data[train_idx, ]
  test_raw  <- data[-train_idx, ]
  
  # Sadece numerik sütunları al (SMOTE ve RF için güvenli yol)
  numeric_cols <- names(which(sapply(train_raw, is.numeric)))
  feature_cols <- setdiff(numeric_cols, target_col)
  
  # Ölçekleme
  preproc <- preProcess(train_raw[, feature_cols], method = c("center", "scale"))
  train_sc <- predict(preproc, train_raw)
  test_sc  <- predict(preproc, test_raw)
  
  # SMOTE Uygulama (Hata payını azaltmak için sadece numeriklerle)
  smote_output <- SMOTE(X = train_sc[, feature_cols], target = train_sc[[target_col]], K = 5)
  train_smote <- smote_output$data
  colnames(train_smote)[ncol(train_smote)] <- target_col
  train_smote[[target_col]] <- factor(train_smote[[target_col]], levels = c("0", "1"))
  
  # RF Model
  rf_formula <- as.formula(paste(target_col, "~ ."))
  rf_model_EK <- randomForest(rf_formula, data = train_smote, ntree = 100)
  
  # Test Hazırlığı
  X_test_all <- test_sc[, feature_cols]
  Y_test_all <- test_sc[[target_col]]
  
  # Güvenli Örnekleme (Eğer sınıfta yeterli örnek yoksa hepsini al)
  idx0 <- which(Y_test_all == "0")
  idx1 <- which(Y_test_all == "1")
  s_size0 <- min(100, length(idx0))
  s_size1 <- min(100, length(idx1))
  selected_indices <- c(sample(idx0, s_size0), sample(idx1, s_size1))
  
  results_list_EK <- list(); coef_list_EK <- list(); var_list_EK <- list(); class_list_EK <- c()
  
  for (k in seq_along(selected_indices)) {
    i <- selected_indices[k]
    ref_point <- X_test_all[i, , drop = FALSE]
    true_label <- as.character(Y_test_all[i])
    
    # Yerel Örnekleme (Normal dağılım)
    n_samples <- 1000 
    sampled_points <- as.data.frame(matrix(rnorm(n_samples * ncol(X_test_all)), nrow = n_samples))
    colnames(sampled_points) <- colnames(X_test_all)
    
    # RF'den olasılık al
    y_probs <- predict(rf_model_EK, sampled_points, type = "prob")[,"1"]
    
    # Mesafe ve Kernel hesaplama
    dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
    
    # Optimizasyon (Küçük Kernel)
    calculate_fidelity <- function(kw) {
      weights <- exp(-(dists^2) / kw)
      fit_temp <- lm(y_probs ~ ., data = sampled_points, weights = weights)
      r2 <- summary(fit_temp)$r.squared
      if(is.na(r2)) return(0)
      return(if(r2 <= target_r2) r2 else 2*target_r2 - r2)
    }
    opt_small <- optimize(calculate_fidelity, interval = c(0.01, 2.0), maximum = TRUE)
    kw_small <- opt_small$maximum
    
    # Modelleri Kur
    w_small <- exp(-(dists^2) / kw_small)
    w_medium <- exp(-(dists^2) / 15)
    
    fit_small <- lm(y_probs ~ ., data = sampled_points, weights = w_small)
    fit_medium <- lm(y_probs ~ ., data = sampled_points, weights = w_medium)
    
    r2_small <- summary(fit_small)$r.squared
    r2_medium <- summary(fit_medium)$r.squared
    
    # Ağırlıklandırma
    stability_bonus <- 3
    w1 <- r2_small / (r2_small + (r2_medium * stability_bonus) + 1e-6)
    w2 <- (r2_medium * stability_bonus) / (r2_small + (r2_medium * stability_bonus) + 1e-6)
    
    # Katsayılar (Intercept hariç)
    betas_small <- coef(fit_small)[-1]; betas_small[is.na(betas_small)] <- 0
    betas_medium <- coef(fit_medium)[-1]; betas_medium[is.na(betas_medium)] <- 0
    
    # Önemli özellik seçimi (Geniş kernel)
    top_features_EK <- names(sort(abs(betas_medium), decreasing = TRUE)[1:min(K, length(betas_medium))])
    
    # Kombinasyon
    betas_combined <- (w1 * betas_small) + (w2 * betas_medium)
    final_betas_EK <- betas_combined
    final_betas_EK[!(names(final_betas_EK) %in% top_features_EK)] <- 0
    
    coef_list_EK[[k]] <- final_betas_EK
    var_list_EK[[k]]  <- top_features_EK
    class_list_EK[k]  <- true_label
    results_list_EK[[k]] <- data.frame(Sınıf = true_label, Fidelity = r2_small, W1 = w1, W2 = w2)
  }
  
  # Stabilite Fonksiyonu
  compute_stability_EK <- function(indices) {
    if (length(indices) < 2) return(list(VSI = 0, CSI = 0))
    jaccard_vals <- c(); cosine_vals <- c()
    for (m in 1:(length(indices)-1)) {
      for (n in (m+1):length(indices)) {
        A <- var_list_EK[[indices[m]]]; B <- var_list_EK[[indices[n]]]
        jaccard_vals <- c(jaccard_vals, length(intersect(A, B)) / length(union(A, B)))
        
        b1 <- coef_list_EK[[indices[m]]]; b2 <- coef_list_EK[[indices[n]]]
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        cosine_vals <- c(cosine_vals, if(denom > 0) sum(b1*b2)/denom else 0)
      }
    }
    return(list(VSI = mean(jaccard_vals, na.rm=T), CSI = mean(cosine_vals, na.rm=T)))
  }
  
  final_df_EK <- do.call(rbind, results_list_EK)
  summary_stats_EK <- final_df_EK %>% group_by(Sınıf) %>%
    summarise(Ort_Fidelity = mean(Fidelity), Ort_W2 = mean(W2), .groups = "drop")
  
  # Sınıf bazlı stabilite
  s0_idx <- which(class_list_EK == "0")
  s1_idx <- which(class_list_EK == "1")
  
  res0 <- compute_stability_EK(s0_idx)
  res1 <- compute_stability_EK(s1_idx)
  
  # Sonuçları eşleştir (Sınıf 0 ve 1'in varlığını kontrol et)
  summary_stats_EK$VSI <- ifelse(summary_stats_EK$Sınıf == "0", res0$VSI, res1$VSI)
  summary_stats_EK$CSI <- ifelse(summary_stats_EK$Sınıf == "0", res0$CSI, res1$CSI)
  summary_stats_EK$Imbalance_Ratio <- IR_original
  summary_stats_EK$Dataset <- dataset_name
  
  return(summary_stats_EK)
}

dataset_names <- c("abalone", "abalone_19", "churn", "jm1", "kc1", "MagicTelescope", 
                   "mammography", "ozone_level_8hr", "pc1", "pc3", "pc4", "phoneme", 
                   "spambase", "SpeedDating", "steel_plates_fault", "us_crime", 
                   "wilt", "wine_quality", "yeast_me2", "yeast_ml8")

all_results_EK <- list()

for (name in dataset_names) {
  if (exists(name)) {
    message("İşleniyor: ", name)
    tryCatch({
      data_prepared <- standardize_Class_smote_EK(get(name), "Class")
      all_results_EK[[name]] <- run_ensemble_kernel_v3_EK(data_prepared, name, K = 5)
    }, error = function(e) message("Hata (", name, "): ", e$message))
  }
}

final_results <- do.call(rbind, all_results_EK)
print(final_results)






# 1. Verileri en basit haliyle, ekstra müdahale etmeden hazırla
df_opti <- as.data.frame(final_df_smote_collected) %>%
  mutate(Method = "OptiLIME") %>%
  rename(Fidelity = matches("Ort.*Fidelity")) 

df_ek <- as.data.frame(final_results) %>%
  mutate(Method = "EK-LIME") %>%
  rename(Fidelity = matches("Ort.*Fidelity"))

# 2. Verileri alt alta birleştir (df_final3 ismini veriyoruz)
df_final3 <- bind_rows(df_opti, df_ek)

# 3. Grafik (Eski 75 değerini görmek için Parametrik yapıyoruz)
library(ggstatsplot)

plot_csi <- ggbetweenstats(
  data = df_final3,
  x = Method,
  y = CSI,
  type = "parametric", # Önceki kodunuzdaki gibi parametrik yaparsak Mean (75) görünür
  title = "Stability Improvement: OptiLIME vs. EK-LIME",
  ylab = "Constant Stability Index (CSI)",
  xlab = "Method",
  messages = FALSE,
  plot.type = "boxvioline" # Hem kutu hem keman grafiği
)

print(plot_csi);


mean(final_results$CSI)