# smote ile veriler dengelendi ve oplime uygulandı

library(caret)
library(randomForest)
library(dplyr)
library(smotefamily) 


#--------------------------------------------------
# 1. Veri Standardizasyonu
#--------------------------------------------------
#Veri setleri farklı hedef değişken yapılarına sahip olduğu için 0 çoğunluk ,1 azınlık sınıfı olacak şekilde veri standardize edildi.

standardize_Class_smote <- function(data, target_col = "Class") {
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
# 2. OptiLIME v3 (SMOTE Uygulamalı - Orijinal IR Takipli)
#--------------------------------------------------
run_optilime_v3_smote <- function(data, dataset_name, 
                                  target_col = "Class", 
                                  target_r2 = 0.9, 
                                  K = 5) {
  
  set.seed(123)
  
  # --- Orijinal Imbalance Ratio Hesaplama ---
  class_freq_orig <- table(data[[target_col]])
  IR_original <- as.numeric(class_freq_orig["0"]) / as.numeric(class_freq_orig["1"])
  
  # Train/Test Split
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_raw <- data[train_idx, ]
  test_raw  <- data[-train_idx, ]
  
  # Ölçekleme ve Temizlik
  numeric_cols <- sapply(train_raw, is.numeric)
  numeric_cols[target_col] <- FALSE
  var_check <- sapply(train_raw[, numeric_cols], sd)
  valid_cols <- names(var_check[var_check > 0])
  
  preproc <- preProcess(train_raw[, valid_cols], method = c("center", "scale"))
  train_scaled <- predict(preproc, train_raw)
  test_scaled  <- predict(preproc, test_raw)
  
  # SMOTE Uygulama (Sadece Train)
  smote_output <- SMOTE(X = train_scaled[, valid_cols], target = train_scaled[[target_col]], K = 5)
  train_smote <- smote_output$data
  colnames(train_smote)[ncol(train_smote)] <- target_col
  train_smote[[target_col]] <- factor(train_smote[[target_col]], levels = c("0", "1"))
  train_smote <- train_smote[sample(nrow(train_smote)), ]
  
  # Kara Kutu Model (RF)
  formula_str <- as.formula(paste(target_col, "~ ."))
  X_train_final <- model.matrix(formula_str, data = train_smote)[,-1]
  Y_train_final <- train_smote[[target_col]]
  rf_model <- randomForest(x = X_train_final, y = Y_train_final, ntree = 100)
  
  # Test Verisi Hazırlığı
  X_test_all <- model.matrix(formula_str, data = test_scaled)[,-1]
  Y_test_all <- test_scaled[[target_col]]
  
  # Örnek Seçimi (Her Sınıftan 100)
  idx0 <- which(Y_test_all == "0")
  idx1 <- which(Y_test_all == "1")
  selected_indices <- c(sample(idx0, min(100, length(idx0))), 
                        sample(idx1, min(100, length(idx1))))
  
  results_list <- list(); coef_list <- list(); var_list <- list(); class_list <- c()
  
  # LIME Döngüsü
  for (k in seq_along(selected_indices)) {
    i <- selected_indices[k]
    ref_point <- X_test_all[i, , drop = FALSE]
    true_label <- as.character(Y_test_all[i])
    
    n_samples <- 1000 # Stabilite için 1000 örnek
    sampled_points <- as.data.frame(matrix(rnorm(n_samples * ncol(X_train_final)), nrow = n_samples))
    colnames(sampled_points) <- colnames(X_train_final)
    y_probs <- predict(rf_model, sampled_points, type = "prob")[,"1"]
    
    calculate_loss <- function(kw) {
      if(kw <= 0) return(0)
      dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
      weights <- exp(-(dists^2) / kw) # RBF Kernel
      
      if (sd(y_probs) < 1e-10 || sum(weights) < 1) return(0)
      res <- try({
        fit <- lm(y ~ ., data = cbind(sampled_points, y = y_probs), weights = weights)
        summary(fit)$r.squared
      }, silent = TRUE)
      
      if (inherits(res, "try-error") || is.na(res)) return(0)
      if (res <= target_r2) return(res) else return(2 * target_r2 - res)
    }
    
    opt_res <- optimize(calculate_loss, interval = c(0.1, 20), maximum = TRUE)
    best_kw <- opt_res$maximum
    
    # Final Model
    dists <- sqrt(rowSums(sweep(sampled_points, 2, as.numeric(ref_point), "-")^2))
    weights <- exp(-(dists^2) / best_kw)
    final_fit <- lm(y ~ ., data = cbind(sampled_points, y = y_probs), weights = weights)
    
    betas <- coef(final_fit)[-1]
    betas[is.na(betas)] <- 0
    top_features <- names(sort(abs(betas), decreasing = TRUE)[1:min(K, length(betas))])
    
    final_betas <- betas
    final_betas[!(names(final_betas) %in% top_features)] <- 0
    coef_list[[k]] <- final_betas
    var_list[[k]]  <- top_features
    class_list[k]  <- true_label
    
    results_list[[k]] <- data.frame(Sınıf = true_label, Fidelity = opt_res$objective, 
                                    KW = best_kw, stringsAsFactors = FALSE)
  }
  
  # Stability Hesaplama (VSI & CSI)
  compute_stability <- function(indices) {
    if (length(indices) < 2) return(list(VSI = NA, CSI = NA))
    jaccard_vals <- c(); cosine_vals <- c()
    for (i in 1:(length(indices)-1)) {
      for (j in (i+1):length(indices)) {
        A <- var_list[[indices[i]]]; B <- var_list[[indices[j]]]
        union_len <- length(union(A, B))
        jaccard_vals <- c(jaccard_vals, if(union_len > 0) length(intersect(A, B)) / union_len else 0)
        b1 <- coef_list[[indices[i]]]; b2 <- coef_list[[indices[j]]]
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        cosine_vals <- c(cosine_vals, if(denom > 0) sum(b1*b2)/denom else 0)
      }
    }
    return(list(VSI = mean(jaccard_vals, na.rm = TRUE), CSI = mean(cosine_vals, na.rm = TRUE)))
  }
  
  final_df <- do.call(rbind, results_list)
  summary_stats <- final_df %>% group_by(Sınıf) %>%
    summarise(Ortalama_Fidelity = mean(Fidelity), Ortalama_KW = mean(KW), Gozlem_Sayisi = n(), .groups = "drop")
  
  summary_stats$VSI <- c(compute_stability(which(class_list == "0"))$VSI, compute_stability(which(class_list == "1"))$VSI)
  summary_stats$CSI <- c(compute_stability(which(class_list == "0"))$CSI, compute_stability(which(class_list == "1"))$CSI)
  
  summary_stats$Imbalance_Ratio <- IR_original
  summary_stats <- summary_stats %>% select(Imbalance_Ratio, everything())
  
  cat("\n---", dataset_name, "(OptiLIME v3 + SMOTE | Orijinal IR =", round(IR_original,3), ") ---")
  print(summary_stats)
  return(summary_stats)
}

#--------------------------------------------------
# 3. Çalıştırma Döngüsü ve Veri Toplama
#--------------------------------------------------
dataset_names <- c("abalone", "abalone_19", "churn", "jm1", "kc1", "MagicTelescope", 
                   "mammography", "ozone_level_8hr", "pc1", "pc3", "pc4", "phoneme", 
                   "spambase", "SpeedDating", "steel_plates_fault", "us_crime", 
                   "wilt", "wine_quality", "yeast_me2", "yeast_ml8")

all_results_smote <- list()

for (name in dataset_names) {
  if (exists(name)) {
    tryCatch({
      data_prepared <- standardize_Class_smote(get(name), "Class")
      all_results_smote[[name]] <- run_optilime_v3_smote(data_prepared, name, K = 5)
    }, error = function(e) cat("\nHata (", name, "): ", e$message))
  }
}

final_df_smote_collected <- do.call(rbind, all_results_smote)

write.csv(final_comparison_df, "final_df_smote_collected.csv")

