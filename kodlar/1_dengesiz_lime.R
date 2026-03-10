#DENGESİZ VERİ + LİME (DEFAULT)
library(caret)
library(randomForest)
library(dplyr)

#--------------------------------------------------
# 1. Veri Standardizasyonu Fonksiyonu
#--------------------------------------------------
standardize_Class <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  
  if (nlevels(y) != 2) {
    stop("Target değişkeni binary değil.")
  }
  
  freq <- table(y)
  minority_class <- names(freq)[which.min(freq)]
  
  # Azınlık sınıfı her zaman "1", çoğunluk "0" yapılır
  data[[target_col]] <- factor(
    ifelse(y == minority_class, "1", "0"),
    levels = c("0", "1")
  )
  
  return(data)
}

#--------------------------------------------------
# 2. Dinamik Standart LIME Fonksiyonu
#--------------------------------------------------
run_standard_lime_dynamic <- function(data, dataset_name, 
                                      target_col = "Class", 
                                      K = 5) {
  set.seed(123)
  
  # Imbalance Ratio (IR) Hesaplama
  class_freq <- table(data[[target_col]])
  n_majority <- as.numeric(class_freq["0"])
  n_minority <- as.numeric(class_freq["1"])
  IR <- n_majority / n_minority
  
  # Train/Test Split (%70 - %30)
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_raw <- data[train_idx, ]
  test_raw  <- data[-train_idx, ]
  
  # Sayısal Değişkenleri Ölçekleme
  numeric_cols <- sapply(train_raw, is.numeric)
  numeric_cols[target_col] <- FALSE
  
  preproc <- preProcess(train_raw[, numeric_cols], method = c("center", "scale"))
  train_scaled <- predict(preproc, train_raw)
  test_scaled  <- predict(preproc, test_raw)
  
  # Model Matrix ve Random Forest Eğitimi
  formula_str <- as.formula(paste(target_col, "~ ."))
  X_train <- model.matrix(formula_str, data = train_scaled)[,-1]
  Y_train <- train_scaled[[target_col]]
  
  rf_model <- randomForest(x = X_train, y = Y_train, ntree = 100)
  
  #--- DİNAMİK KW HESAPLAMA ---
  # Formül: sqrt(Özellik Sayısı) * 0.75
  p_features <- ncol(X_train)
  dynamic_kw <- sqrt(p_features) * 0.75
  
  X_test_all <- model.matrix(formula_str, data = test_scaled)[,-1]
  Y_test_all <- test_scaled[[target_col]]
  
  # Her sınıftan en fazla 100 gözlem seçimi
  idx0 <- which(Y_test_all == "0")
  idx1 <- which(Y_test_all == "1")
  selected_indices <- c(
    sample(idx0, min(100, length(idx0))),
    sample(idx1, min(100, length(idx1)))
  )
  
  results_list <- list()
  coef_list <- list()
  var_list  <- list()
  class_list <- c()
  
  # LIME Döngüsü
  for (k in seq_along(selected_indices)) {
    i <- selected_indices[k]
    ref_point <- X_test_all[i, , drop = FALSE]
    true_label <- as.character(Y_test_all[i])
    
    # Yerel örnekleme (500 örnek)
    n_samples <- 500
    sampled_points <- as.data.frame(matrix(rnorm(n_samples * p_features), nrow = n_samples))
    colnames(sampled_points) <- colnames(X_train)
    
    # Modelden olasılık tahminleri ("1" sınıfı için)
    y_probs <- predict(rf_model, sampled_points, type = "prob")[,"1"]
    
    # Mesafe bazlı ağırlıklandırma (Dinamik KW ile)
    dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
    weights <- exp(-(dists^2) / dynamic_kw)
    
    # Yerel Model (WLS)
    final_fit <- lm(y ~ ., data = cbind(sampled_points, y = y_probs), weights = weights)
    
    # Feature Selection (K=5) - Katsayıları sıfırlamadan en iyileri seçer
    betas <- coef(final_fit)[-1]
    betas[is.na(betas)] <- 0
    top_features <- names(sort(abs(betas), decreasing = TRUE)[1:min(K, length(betas))])
    
    coef_list[[k]] <- betas
    var_list[[k]]  <- top_features
    class_list[k]  <- true_label
    
    # R-squared (Fidelity)
    fidelity <- summary(final_fit)$r.squared
    
    results_list[[k]] <- data.frame(
      Dataset = dataset_name,
      Sınıf = true_label,
      Fidelity = ifelse(is.na(fidelity), 0, fidelity),
      KW = dynamic_kw,
      stringsAsFactors = FALSE
    )
  }
  
  # Stability Hesaplama (VSI & CSI)
  compute_stability <- function(indices) {
    if (length(indices) < 2) return(list(VSI = NA, CSI = NA))
    jaccard_vals <- c(); cosine_vals  <- c()
    
    for (m in 1:(length(indices)-1)) {
      for (n in (m+1):length(indices)) {
        # VSI: Jaccard Similarity
        A <- var_list[[indices[m]]]; B <- var_list[[indices[n]]]
        jaccard <- length(intersect(A, B)) / length(union(A, B))
        jaccard_vals <- c(jaccard_vals, jaccard)
        
        # CSI: Cosine Similarity
        b1 <- coef_list[[indices[m]]]; b2 <- coef_list[[indices[n]]]
        common_names <- union(names(b1), names(b2))
        v1 <- setNames(rep(0, length(common_names)), common_names); v1[names(b1)] <- b1
        v2 <- setNames(rep(0, length(common_names)), common_names); v2[names(b2)] <- b2
        
        denom <- sqrt(sum(v1^2)) * sqrt(sum(v2^2))
        cos_sim <- if(denom > 0) sum(v1*v2)/denom else 0
        cosine_vals <- c(cosine_vals, cos_sim)
      }
    }
    return(list(VSI = mean(jaccard_vals, na.rm = TRUE), CSI = mean(cosine_vals, na.rm = TRUE)))
  }
  
  final_res_df <- do.call(rbind, results_list)
  summary_stats <- final_res_df %>%
    group_by(Sınıf) %>%
    summarise(
      Dataset = first(Dataset),
      Ortalama_Fidelity = mean(Fidelity),
      Ortalama_KW = mean(KW),
      Gozlem_Sayisi = n(),
      .groups = "drop"
    )
  
  # Sınıf bazlı kararlılık değerlerini ekle
  summary_stats$VSI <- NA; summary_stats$CSI <- NA
  s0_idx <- which(class_list == "0")
  s1_idx <- which(class_list == "1")
  
  if (length(s0_idx) > 1) {
    s0_stab <- compute_stability(s0_idx)
    summary_stats$VSI[summary_stats$Sınıf == "0"] <- s0_stab$VSI
    summary_stats$CSI[summary_stats$Sınıf == "0"] <- s0_stab$CSI
  }
  if (length(s1_idx) > 1) {
    s1_stab <- compute_stability(s1_idx)
    summary_stats$VSI[summary_stats$Sınıf == "1"] <- s1_stab$VSI
    summary_stats$CSI[summary_stats$Sınıf == "1"] <- s1_stab$CSI
  }
  
  summary_stats$Imbalance_Ratio <- IR
  summary_stats$Features_Count <- p_features
  
  cat("\n---", dataset_name, "Tamamlandı (KW:", round(dynamic_kw, 3), ") ---")
  return(summary_stats)
}

#--------------------------------------------------
# 3. Ana Döngü: 20 Veriseti Üzerinde Çalıştırma
#--------------------------------------------------
dataset_names <- c(
  "abalone", "abalone_19", "churn", "jm1", "kc1",
  "MagicTelescope", "mammography", "ozone_level_8hr",
  "pc1", "pc3", "pc4", "phoneme", "spambase",
  "SpeedDating", "steel_plates_fault", "us_crime",
  "wilt", "wine_quality", "yeast_me2", "yeast_ml8")

all_results__default <- list()

for (name in dataset_names) {
  if (exists(name)) {
    tryCatch({
      data_prepared <- standardize_Class(get(name), "Class")
      all_results__default[[name]] <- run_standard_lime_dynamic(data_prepared, name, K = 5)
    }, error = function(e) {
      cat("\nHata (", name, "): ", e$message)
    })
  } else {
    cat("\nUyarı: ", name, " veri seti global ortamda bulunamadı.")
  }
}

# Tüm sonuçları birleştir
final_comparison_df <- do.call(rbind, all_results__default)

# Sonucu görüntüle
print(final_comparison_df)