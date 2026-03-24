#glime

library(caret)
library(randomForest)
library(dplyr)

# 1. Veri Hazırlama Fonksiyonu
prepare_data <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  minority_class <- names(table(y))[which.min(table(y))]
  
  data[[target_col]] <- factor(ifelse(y == minority_class, "1", "0"),
                               levels = c("0", "1"))
  
  dummies <- dummyVars(as.formula(paste(target_col, "~ .")), data = data)
  data_numeric <- as.data.frame(predict(dummies, newdata = data))
  data_numeric[[target_col]] <- data[[target_col]]
  
  preproc <- preProcess(data_numeric[, -ncol(data_numeric)],
                        method = c("center", "scale"))
  
  return(predict(preproc, data_numeric))
}

# 2. GLIME-Binomial Ana Fonksiyonu
run_glime <- function(data, dataset_name,
                      target_col = "Class",
                      K = 5,
                      sigma = 0.5,       # Yerelliği belirleyen çekirdek genişliği [cite: 108]
                      lambda_ridge = 0.01, # Regülarizasyon katsayısı [cite: 106]
                      n_samples = 1000) {
  
  set.seed(123)
  
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_df  <- data[train_idx, ]
  test_df   <- data[-train_idx, ]
  
  feature_cols <- setdiff(colnames(data), target_col)
  d <- length(feature_cols)
  
  rf_model <- randomForest(as.formula(paste(target_col, "~ .")),
                           data = train_df, ntree = 100)
  
  X_test <- test_df[, feature_cols]
  Y_test <- test_df[[target_col]]
  
  idx0 <- which(Y_test == "0")
  idx1 <- which(Y_test == "1")
  
  # Her sınıftan dengeli örnek seçimi
  selected_indices <- c(sample(idx0, min(25, length(idx0))),
                        sample(idx1, min(25, length(idx1))))
  
  coef_list <- list(); var_list  <- list(); class_list <- c(); fidelity_results <- c()
  
  for (k in seq_along(selected_indices)) {
    i <- selected_indices[k]
    
    # MAKALE UYARLAMASI: Binomial Dağılım ile Örnekleme [cite: 164, 165]
    # Olasılık: P(zi=1) = 1 / (1 + exp(-1/sigma^2))
    prob_success <- 1 / (1 + exp(-1/(sigma^2)))
    sampled_binary <- matrix(rbinom(n_samples * d, 1, prob_success), 
                             nrow = n_samples, ncol = d)
    
    sampled_points <- as.data.frame(sampled_binary)
    colnames(sampled_points) <- feature_cols
    
    # Model Tahminleri (Ağırlıklandırma fonksiyonu olmadan [cite: 149, 153])
    y_probs <- predict(rf_model, sampled_points, type = "prob")[,"1"]
    
    # NaN Önleyici Varyans Kontrolü
    y_var <- sum((y_probs - mean(y_probs))^2)
    
    if (y_var < 1e-10) {
      fidelity <- 0
      betas <- rep(0, d)
      names(betas) <- feature_cols
    } else {
      # MAKALE UYARLAMASI: Z Normalizasyonu ve Ridge Çözümü [cite: 153, 1152]
      Z_val <- (exp(-d/(sigma^2)) / (2^d)) * ((1 + exp(1/(sigma^2)))^d)
      adj_lambda <- lambda_ridge / (Z_val * n_samples)
      
      X_mat <- as.matrix(sampled_points)
      XtX <- t(X_mat) %*% X_mat + adj_lambda * diag(d)
      Xty <- t(X_mat) %*% y_probs
      
      betas <- solve(XtX, Xty)
      names(betas) <- feature_cols
      
      # Özellik Seçimi (Top-K) ve Katsayıların Saklanması
      top_features <- names(sort(abs(betas), decreasing = TRUE)[1:K])
      final_betas <- rep(0, d); names(final_betas) <- feature_cols
      final_betas[top_features] <- betas[top_features]
      
      # Yerel Sadakat (Local Fidelity - R^2) [cite: 313, 316]
      intercept <- mean(y_probs - X_mat %*% final_betas)
      y_hat <- intercept + X_mat %*% final_betas
      fidelity <- 1 - (sum((y_probs - y_hat)^2) / y_var)
    }
    
    coef_list[[k]] <- if(exists("final_betas")) final_betas else betas
    var_list[[k]]  <- names(sort(abs(betas), decreasing = TRUE)[1:K])
    class_list[k]  <- as.character(Y_test[i])
    fidelity_results[k] <- fidelity
  }
  
  # Stabilite Hesaplama (VSI & CSI) [cite: 308, 311]
  compute_stability <- function(class_label) {
    indices <- which(class_list == class_label)
    if (length(indices) < 2) return(list(VSI = NA, CSI = NA))
    
    j_vals <- c(); c_vals <- c()
    for (m in 1:(length(indices)-1)) {
      for (n in (m+1):length(indices)) {
        A <- var_list[[indices[m]]]; B <- var_list[[indices[n]]]
        j_vals <- c(j_vals, length(intersect(A, B)) / length(union(A, B))) # VSI (Jaccard)
        
        b1 <- coef_list[[indices[m]]]; b2 <- coef_list[[indices[n]]]
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        c_vals <- c(c_vals, if (denom > 0) sum(b1*b2)/denom else 0) # CSI (Cosine Similarity)
      }
    }
    return(list(VSI = mean(j_vals), CSI = mean(c_vals)))
  }
  
  s0 <- compute_stability("0"); s1 <- compute_stability("1")
  
  return(data.frame(
    Dataset = dataset_name, Sınıf = c("0", "1"),
    Ort_Fidelity = c(mean(fidelity_results[class_list=="0"], na.rm=T),
                     mean(fidelity_results[class_list=="1"], na.rm=T)),
    VSI = c(s0$VSI, s1$VSI), CSI = c(s0$CSI, s1$CSI)
  ))
}

# 3. Döngü ve Sonuçların Saklanması
all_results <- list()

dataset_names <- c( "abalone" ,"abalone_19" ,"churn","jm1"  ,             
                     "kc1",  "MagicTelescope",   "mammography", "ozone_level_8hr" ,  
                     "pc1" , "pc3",   "pc4","phoneme"           ,
                     "spambase","SpeedDating","steel_plates_fault", "us_crime",          
                     "wilt","wine_quality", "yeast_me2","yeast_ml8"   )

for (name in dataset_names) {
  if (exists(name)) {
    message("İşleniyor: ", name)
    all_results[[name]] <- run_glime(data = prepare_data(get(name)), dataset_name = name)
  }
}

# --- SONUÇ TABLOSU ---
final_glime <- do.call(rbind, all_results)
print(final_glime)


# Sınıf bazlı genel özet
glime_summary <- final_glime %>%
  group_by(Sınıf) %>%
  summarise(
       Ort_Fidelity = mean(Ort_Fidelity, na.rm = TRUE),
       Ort_VSI = mean(VSI, na.rm = TRUE),
       Ort_CSI = mean(CSI, na.rm = TRUE)
  )

print(glime_summary)