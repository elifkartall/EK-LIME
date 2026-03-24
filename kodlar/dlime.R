library(caret)
library(randomForest)
library(dplyr)
library(class)

# -----------------------------
# DATA PREP (FIXED)
# -----------------------------
data_prepare <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  
  minority_class <- names(table(y))[which.min(table(y))]
  
  data[[target_col]] <- factor(ifelse(y == minority_class, "1", "0"),
                               levels = c("0", "1"))
  
  dummies <- dummyVars(as.formula(paste(target_col, "~ .")), data = data)
  X <- as.data.frame(predict(dummies, newdata = data))
  
  preproc <- preProcess(X, method = c("center", "scale"))
  X_scaled <- as.data.frame(predict(preproc, X))
  
  X_scaled[[target_col]] <- data[[target_col]]  # target scale edilmez
  
  return(X_scaled)
}

# -----------------------------
# DLIME (FIXED)
# -----------------------------
library(caret)
library(randomForest)
library(dplyr)
library(class)

# -----------------------------
# DATA PREP
# -----------------------------
data_prepare <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  
  minority_class <- names(table(y))[which.min(table(y))]
  
  data[[target_col]] <- factor(ifelse(y == minority_class, "1", "0"),
                               levels = c("0", "1"))
  
  dummies <- dummyVars(as.formula(paste(target_col, "~ .")), data = data)
  X <- as.data.frame(predict(dummies, newdata = data))
  
  preproc <- preProcess(X, method = c("center", "scale"))
  X_scaled <- as.data.frame(predict(preproc, X))
  
  X_scaled[[target_col]] <- data[[target_col]]
  
  return(X_scaled)
}

# -----------------------------
# DLIME
# -----------------------------
run_dlime <- function(data, dataset_name,
                      target_col = "Class",
                      K = 5,
                      k_knn = 1,
                      n_clusters = 2) {
  
  set.seed(123)
  
  train_idx <- createDataPartition(data[[target_col]], p = 0.7, list = FALSE)
  train_df  <- data[train_idx, ]
  test_df   <- data[-train_idx, ]
  
  feature_cols <- setdiff(colnames(data), target_col)
  
  rf_model <- randomForest(as.formula(paste(target_col, "~ .")),
                           data = train_df,
                           ntree = 100)
  
  hc <- hclust(dist(train_df[, feature_cols]), method = "ward.D2")
  cluster_labels <- as.character(cutree(hc, k = n_clusters))
  
  X_train <- train_df[, feature_cols]
  X_test  <- test_df[, feature_cols]
  Y_test  <- test_df[[target_col]]
  
  idx0 <- which(Y_test == "0")
  idx1 <- which(Y_test == "1")
  
  selected_indices <- c(sample(idx0, min(50, length(idx0))),
                        sample(idx1, min(50, length(idx1))))
  
  coef_list <- list()
  var_list  <- list()
  class_list <- c()
  fidelity_results <- c()
  
  for (k in seq_along(selected_indices)) {
    
    i <- selected_indices[k]
    x <- X_test[i, , drop = FALSE]
    
    cluster_hat <- as.character(knn(train = X_train,
                                    test  = x,
                                    cl    = cluster_labels,
                                    k     = k_knn))
    
    cluster_idx <- which(cluster_labels == cluster_hat)
    
    if (length(cluster_idx) < 5) {
      cluster_idx <- sample(1:nrow(X_train), min(50, nrow(X_train)))
    }
    
    local_data <- X_train[cluster_idx, , drop = FALSE]
    
    # RF output
    y_probs <- predict(rf_model, local_data, type = "prob")[, "1"]
    
    # Linear surrogate
    df_local <- local_data
    df_local$y <- y_probs
    
    lm_model <- lm(y ~ ., data = df_local)
    
    betas <- coef(lm_model)[-1]
    betas[is.na(betas)] <- 0
    
    top_features <- names(sort(abs(betas), decreasing = TRUE)[1:K])
    
    final_betas <- rep(0, length(feature_cols))
    names(final_betas) <- feature_cols
    final_betas[top_features] <- betas[top_features]
    
    intercept <- coef(lm_model)[1]
    
    X_mat <- as.matrix(local_data[, feature_cols])
    y_hat <- intercept + X_mat %*% final_betas
    
    # -----------------------------
    # NON-NEGATIVE FIDELITY
    # -----------------------------
    y_bar <- mean(y_probs)
    var_term <- sum((y_probs - y_bar)^2)
    
    if (var_term < 1e-6) var_term <- 1e-6
    
    sse <- sum((y_probs - y_hat)^2)
    
    raw_fidelity <- 1 - (sse / var_term)
    
    # 🔥 KEY FIX: no negative values
    fidelity <- max(0, raw_fidelity)
    
    coef_list[[k]] <- final_betas
    var_list[[k]]  <- top_features
    class_list[k]  <- as.character(Y_test[i])
    fidelity_results[k] <- fidelity
  }
  
  compute_stability <- function(indices) {
    if (length(indices) < 2) return(list(VSI = 0, CSI = 0))
    
    j_vals <- c()
    c_vals <- c()
    
    for (m in 1:(length(indices)-1)) {
      for (n in (m+1):length(indices)) {
        
        A <- var_list[[indices[m]]]
        B <- var_list[[indices[n]]]
        
        j_vals <- c(j_vals,
                    length(intersect(A, B)) / length(union(A, B)))
        
        b1 <- coef_list[[indices[m]]]
        b2 <- coef_list[[indices[n]]]
        
        denom <- sqrt(sum(b1^2)) * sqrt(sum(b2^2))
        
        c_vals <- c(c_vals,
                    if (denom > 0) sum(b1*b2)/denom else 0)
      }
    }
    
    return(list(VSI = mean(j_vals),
                CSI = mean(c_vals)))
  }
  
  res0 <- compute_stability(which(class_list == "0"))
  res1 <- compute_stability(which(class_list == "1"))
  
  return(data.frame(
    Dataset = dataset_name,
    Sinif = c("0", "1"),
    Ort_Fidelity = c(mean(fidelity_results[class_list=="0"]),
                     mean(fidelity_results[class_list=="1"])),
    VSI = c(res0$VSI, res1$VSI),
    CSI = c(res0$CSI, res1$CSI)
  ))
}

# -----------------------------
# RUN ALL DATASETS
# -----------------------------
all_results_DLIME <- list()

dataset_names <- c("abalone","abalone_19","churn","jm1",
                   "kc1","MagicTelescope","mammography","ozone_level_8hr",
                   "pc1","pc3","pc4","phoneme",
                   "spambase","SpeedDating","steel_plates_fault","us_crime",
                   "wilt","wine_quality","yeast_me2","yeast_ml8")

for (name in dataset_names) {
  if (exists(name)) {
    
    message("İşleniyor: ", name)
    
    tryCatch({
      
      result <- run_dlime(
        data = data_prepare(get(name), "Class"),
        dataset_name = name
      )
      
      all_results_DLIME[[name]] <- result
      
    }, error = function(e) {
      message("Hata (", name, "): ", e$message)
    })
  }
}

final_table_dlime <- do.call(rbind, all_results_DLIME)
print(final_table_dlime)


# Sınıf bazlı genel özet
dlime_summary <- final_table_dlime %>%
  group_by(Sinif) %>%
  summarise(
    Ort_Fidelity = mean(Ort_Fidelity, na.rm = TRUE),
    Ort_VSI = mean(VSI, na.rm = TRUE),
    Ort_CSI = mean(CSI, na.rm = TRUE)
  )

print(dlime_summary)