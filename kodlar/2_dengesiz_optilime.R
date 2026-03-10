# Veriseti dengesiz, optilime uygulandı
library(caret)
library(randomForest)
library(dplyr)

#--------------------------------------------------
# 1. Veri Standardizasyonu
#--------------------------------------------------
#Veri setleri farklı hedef değişken yapılarına sahip olduğu için 0 çoğunluk ,1 azınlık sınıfı olacak şekilde veri standardize edildi.

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
# 2. OptiLIME 
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
  
  rf_model <- randomForest(x = X_train, y = Y_train)
  
  X_test_all <- model.matrix(formula_str, data = test_scaled)[,-1]
  Y_test_all <- test_scaled[[target_col]]
  
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

grid.arrange(plot_0, plot_1, ncol = 2)
