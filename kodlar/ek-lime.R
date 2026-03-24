# ================================
# ENSEMBLE KERNEL LIME (WEIGHT UPDATE ONLY)
# ================================

library(caret)
library(randomForest)
library(dplyr)

# 1. Veri Hazırlama
prepare_data_EK_new <- function(data, target_col = "Class") {
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

# 2. Ana Fonksiyon
run_ensemble_kernel_v8_EK_new <- function(data, dataset_name, 
                                          target_col = "Class", 
                                          min_final_fidelity = 0.90, 
                                          min_wide_fidelity = 0.50,
                                          K = 5) {
  set.seed(123)
  
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_df  <- data[train_idx, ]; test_df <- data[-train_idx, ]
  
  feature_cols <- setdiff(colnames(data), target_col)
  
  rf_model_EK_new <- randomForest(as.formula(paste(target_col, "~ .")),
                                  data = train_df, ntree = 100)
  
  X_test_all <- test_df[, feature_cols]
  Y_test_all <- test_df[[target_col]]
  
  idx0 <- which(Y_test_all == "0")
  idx1 <- which(Y_test_all == "1")
  selected_indices <- c(sample(idx0, min(50, length(idx0))),
                        sample(idx1, min(50, length(idx1))))
  
  coef_list_EK_new <- list()
  var_list_EK_new  <- list()
  class_list_EK_new <- c()
  fidelity_results_new <- c()
  
  # -------- INSTANCE LOOP --------
  for (k in seq_along(selected_indices)) {
    
    i <- selected_indices[k]
    ref_point <- X_test_all[i, , drop = FALSE]
    
    # 🔴 ORİJİNAL SAMPLING (DEĞİŞMEDİ)
    sampled_points <- as.data.frame(
      matrix(rnorm(1000 * ncol(X_test_all)), nrow = 1000)
    )
    colnames(sampled_points) <- feature_cols
    
    y_probs <- predict(rf_model_EK_new, sampled_points, type = "prob")[,"1"]
    
    dists <- sqrt(rowSums(
      sweep(sampled_points, 2, as.numeric(ref_point), "-")^2
    ))
    
    # Kernel optimizasyonu
    opt_small <- optimize(function(kw) {
      w <- exp(-(dists^2) / kw)
      r2 <- summary(lm(y_probs ~ ., data = sampled_points, weights = w))$r.squared
      if(is.na(r2)) return(0)
      return(r2)
    }, interval = c(0.01, 1.0), maximum = TRUE)
    
    opt_wide <- optimize(function(kw) {
      w <- exp(-(dists^2) / kw)
      r2 <- summary(lm(y_probs ~ ., data = sampled_points, weights = w))$r.squared
      if(is.na(r2) || r2 < min_wide_fidelity) return(-1/kw)
      return(kw)
    }, interval = c(1.0, 30.0), maximum = TRUE)
    
    w_small <- exp(-(dists^2) / opt_small$maximum)
    w_wide  <- exp(-(dists^2) / opt_wide$maximum)
    
    fit_small <- lm(y_probs ~ ., data = sampled_points, weights = w_small)
    fit_wide  <- lm(y_probs ~ ., data = sampled_points, weights = w_wide)
    
    betas_small <- coef(fit_small)[-1]
    betas_wide  <- coef(fit_wide)[-1]
    
    betas_small[is.na(betas_small)] <- 0
    betas_wide[is.na(betas_wide)]  <- 0
    
    # Soft shrink
    shrink_factor <- 0.85
    betas_small <- shrink_factor * betas_small
    betas_wide  <- shrink_factor * betas_wide
    
    # Ensemble
    betas_hybrid <- 0.3 * betas_small + 0.7 * betas_wide
    
    # Light smoothing
    beta_mean <- (betas_small + betas_wide) / 2
    betas_hybrid <- 0.9 * betas_hybrid + 0.1 * beta_mean
    
    # Feature selection
    top_features <- names(sort(abs(betas_wide), decreasing = TRUE)[1:K])
    
    final_betas <- rep(0, length(betas_hybrid))
    names(final_betas) <- names(betas_hybrid)
    final_betas[top_features] <- betas_hybrid[top_features]
    
    # Intercept
    intercept <- (0.7 * coef(fit_small)[1] + 0.3 * coef(fit_wide)[1])
    
    y_hat <- intercept + as.matrix(sampled_points) %*% final_betas
    
    # Fidelity
    y_bar_w <- sum(w_small * y_probs) / sum(w_small)
    
    var_term <- sum(w_small * (y_probs - y_bar_w)^2)
    var_term <- max(var_term, 0.01)
    
    ens_fidelity <- 1 - (sum(w_small * (y_probs - y_hat)^2) / var_term)
    
    # Fallback
    if(ens_fidelity < min_final_fidelity) {
      top_features <- names(sort(abs(betas_small), decreasing = TRUE)[1:K])
      
      final_betas <- rep(0, length(betas_small))
      names(final_betas) <- names(betas_small)
      final_betas[top_features] <- betas_small[top_features]
      
      y_hat <- coef(fit_small)[1] + as.matrix(sampled_points) %*% final_betas
      
      y_bar_w <- sum(w_small * y_probs) / sum(w_small)
      var_term <- sum(w_small * (y_probs - y_bar_w)^2)
      var_term <- max(var_term, 0.01)
      
      ens_fidelity <- 1 - (sum(w_small * (y_probs - y_hat)^2) / var_term)
    }
    
    coef_list_EK_new[[k]] <- final_betas
    var_list_EK_new[[k]]  <- top_features
    class_list_EK_new[k]  <- as.character(Y_test_all[i])
    fidelity_results_new[k] <- ens_fidelity
  }
  
  # CSI smoothing
  lambda <- 0.3
  
  for(cls in unique(class_list_EK_new)) {
    idx <- which(class_list_EK_new == cls)
    
    if(length(idx) > 1){
      coef_matrix <- do.call(rbind, coef_list_EK_new[idx])
      mean_coef <- colMeans(coef_matrix)
      mean_coef <- mean_coef / (sqrt(sum(mean_coef^2)) + 1e-8)
      
      for(i in idx){
        b <- coef_list_EK_new[[i]]
        b_norm <- b / (sqrt(sum(b^2)) + 1e-8)
        coef_list_EK_new[[i]] <- (1 - lambda) * b_norm + lambda * mean_coef
      }
    }
  }
  
  # Stabilite
  compute_stability_new <- function(indices) {
    if (length(indices) < 2) return(list(VSI = 0, CSI = 0))
    
    j_vals <- c()
    c_vals <- c()
    
    for (m in 1:(length(indices)-1)) {
      for (n in (m+1):length(indices)) {
        
        A <- var_list_EK_new[[indices[m]]]
        B <- var_list_EK_new[[indices[n]]]
        
        j_vals <- c(j_vals, length(intersect(A, B)) / length(union(A, B)))
        
        b1 <- coef_list_EK_new[[indices[m]]]
        b2 <- coef_list_EK_new[[indices[n]]]
        
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        c_vals <- c(c_vals, if(denom > 0) sum(b1*b2)/denom else 0)
      }
    }
    
    return(list(VSI = mean(j_vals, na.rm=TRUE),
                CSI = mean(c_vals, na.rm=TRUE)))
  }
  
  res0 <- compute_stability_new(which(class_list_EK_new == "0"))
  res1 <- compute_stability_new(which(class_list_EK_new == "1"))
  
  return(data.frame(
    Dataset = dataset_name,
    Sınıf = c("0", "1"),
    Ort_Fidelity = c(mean(fidelity_results_new[class_list_EK_new=="0"]),
                     mean(fidelity_results_new[class_list_EK_new=="1"])),
    VSI = c(res0$VSI, res1$VSI),
    CSI = c(res0$CSI, res1$CSI)
  ))
}

# ================================
# RUN
# ================================

all_results_new <- list()

dataset_names_new <- c(
  "abalone","abalone_19","churn","jm1","kc1","MagicTelescope",
  "mammography","ozone_level_8hr","pc1","pc3","pc4","phoneme",
  "spambase","SpeedDating","steel_plates_fault","us_crime",
  "wilt","wine_quality","yeast_me2","yeast_ml8"
)

for (name in dataset_names_new) {
  if (exists(name)) {
    
    message("İşleniyor: ", name)
    
    tryCatch({
      
      result <- run_ensemble_kernel_v8_EK_new(
        data = prepare_data_EK_new(get(name), "Class"),
        dataset_name = name
      )
      
      all_results_new[[name]] <- result
      
    }, error = function(e) {
      message("Hata (", name, "): ", e$message)
    })
  }
}

final_table_new <- do.call(rbind, all_results_new)
print(final_table_new)

class_summary_ek_new <- final_table_new %>%
  group_by(Sınıf) %>%
  summarise(
    Mean_Fidelity = mean(Ort_Fidelity, na.rm = TRUE),
    Mean_VSI      = mean(VSI, na.rm = TRUE),
    Mean_CSI      = mean(CSI, na.rm = TRUE)
  )

print(class_summary_ek_new)