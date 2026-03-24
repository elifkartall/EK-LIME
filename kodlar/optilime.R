library(caret)
library(randomForest)
library(dplyr)
library(tidyr)

#--------------------------------------------------
# 1. Standardize Target
#--------------------------------------------------
standardize_Class_v2 <- function(data, target_col = "Class") {
  data <- na.omit(data)
  y <- as.factor(data[[target_col]])
  
  if (nlevels(y) != 2) stop("Target binary değil")
  
  freq <- table(y)
  minority_class <- names(freq)[which.min(freq)]
  
  data[[target_col]] <- factor(
    ifelse(y == minority_class, "1", "0"),
    levels = c("0","1")
  )
  
  return(data)
}

#--------------------------------------------------
# 2. Stability (VSI + CSI)
#--------------------------------------------------
compute_vsi_csi_v2 <- function(coef_list_v2, var_list_v2) {
  
  n_v2 <- length(coef_list_v2)
  if(n_v2 < 2) return(list(VSI=NA, CSI=NA))
  
  jaccard_vals_v2 <- c()
  cosine_vals_v2 <- c()
  
  for(i in 1:(n_v2-1)){
    for(j in (i+1):n_v2){
      
      # VSI
      A_v2 <- var_list_v2[[i]]
      B_v2 <- var_list_v2[[j]]
      union_len_v2 <- length(union(A_v2,B_v2))
      
      jacc_v2 <- if(union_len_v2 > 0){
        length(intersect(A_v2,B_v2)) / union_len_v2
      } else {0}
      
      jaccard_vals_v2 <- c(jaccard_vals_v2, jacc_v2)
      
      # CSI
      b1_v2 <- coef_list_v2[[i]]
      b2_v2 <- coef_list_v2[[j]]
      
      denom_v2 <- sqrt(sum(b1_v2^2)) * sqrt(sum(b2_v2^2))
      cos_sim_v2 <- if(denom_v2 > 0) sum(b1_v2*b2_v2)/denom_v2 else 0
      
      cosine_vals_v2 <- c(cosine_vals_v2, cos_sim_v2)
    }
  }
  
  return(list(
    VSI = mean(jaccard_vals_v2),
    CSI = mean(cosine_vals_v2)
  ))
}

#--------------------------------------------------
# 3. OptiLIME Pipeline (LOCAL FIXED)
#--------------------------------------------------
run_optilime_stable_v2 <- function(data_v2, dataset_name_v2,
                                   target_col="Class",
                                   target_r2=0.9,
                                   K=5,
                                   n_repeats_v2=20) {
  
  set.seed(123)
  
  # Split
  train_idx_v2 <- createDataPartition(data_v2[[target_col]], p=0.7, list=FALSE)
  train_v2 <- data_v2[train_idx_v2,]
  test_v2  <- data_v2[-train_idx_v2,]
  
  # Scale
  num_cols_v2 <- sapply(train_v2, is.numeric)
  num_cols_v2[target_col] <- FALSE
  
  preproc_v2 <- preProcess(train_v2[,num_cols_v2], method=c("center","scale"))
  train_v2 <- predict(preproc_v2, train_v2)
  test_v2  <- predict(preproc_v2, test_v2)
  
  # Model
  formula_str_v2 <- as.formula(paste(target_col,"~ ."))
  X_train_v2 <- model.matrix(formula_str_v2, train_v2)[,-1]
  y_train_v2 <- train_v2[[target_col]]
  
  rf_model_v2 <- randomForest(x=X_train_v2, y=y_train_v2)
  
  X_test_v2 <- model.matrix(formula_str_v2, test_v2)[,-1]
  y_test_v2 <- test_v2[[target_col]]
  
  sigma_v2 <- apply(X_train_v2, 2, sd)
  
  #--------------------------------------------------
  # Stratified selection
  #--------------------------------------------------
  idx0_v2 <- which(y_test_v2 == "0")
  idx1_v2 <- which(y_test_v2 == "1")
  
  selected_idx_v2 <- c(
    if(length(idx0_v2) > 0) sample(idx0_v2, min(25, length(idx0_v2))) else NULL,
    if(length(idx1_v2) > 0) sample(idx1_v2, min(25, length(idx1_v2))) else NULL
  )
  
  results_list_v2 <- list()
  
  for(i_v2 in selected_idx_v2) {
    
    ref_v2 <- X_test_v2[i_v2,,drop=FALSE]
    
    coef_list_v2 <- list()
    var_list_v2  <- list()
    
    fidelity_vec_v2 <- c()
    kw_vec_v2 <- c()
    
    #-----------------------------------
    # REPEAT LOOP (LOCAL SAMPLING)
    #-----------------------------------
    for(r_v2 in 1:n_repeats_v2){
      
      set.seed(123 + r_v2)
      
      n_samples_v2 <- 500
      sampled_v2 <- matrix(0, n_samples_v2, ncol(X_train_v2))
      
      # LOCAL sampling
      for(j_v2 in 1:ncol(X_train_v2)){
        local_sd_v2 <- sigma_v2[j_v2] * 0.5
        sampled_v2[,j_v2] <- rnorm(n_samples_v2, ref_v2[,j_v2], local_sd_v2)
      }
      
      colnames(sampled_v2) <- colnames(X_train_v2)
      sampled_v2 <- as.data.frame(sampled_v2)
      
      y_probs_v2 <- predict(rf_model_v2, sampled_v2, type="prob")[,"1"]
      
      # Kernel search
      kw_grid_v2 <- seq(0.1, 1.5, length.out=20)
      r2_vals_v2 <- c()
      
      for(kw_v2 in kw_grid_v2){
        dists_v2 <- sqrt(rowSums((sampled_v2 - ref_v2)^2))
        weights_v2 <- exp(-(dists_v2^2)/kw_v2)
        
        fit_v2 <- lm(y_probs_v2 ~ ., data=sampled_v2, weights=weights_v2)
        r2_vals_v2 <- c(r2_vals_v2, summary(fit_v2)$r.squared)
      }
      
      loss_v2 <- ifelse(r2_vals_v2 <= target_r2,
                        r2_vals_v2,
                        2*target_r2 - r2_vals_v2)
      
      best_idx_v2 <- which.max(loss_v2)
      best_kw_v2 <- kw_grid_v2[best_idx_v2]
      
      # Final model
      dists_v2 <- sqrt(rowSums((sampled_v2 - ref_v2)^2))
      weights_v2 <- exp(-(dists_v2^2)/best_kw_v2)
      
      final_fit_v2 <- lm(y_probs_v2 ~ ., data=sampled_v2, weights=weights_v2)
      
      betas_v2 <- coef(final_fit_v2)[-1]
      betas_v2[is.na(betas_v2)] <- 0
      
      top_features_v2 <- names(sort(abs(betas_v2), decreasing=TRUE)[1:min(K,length(betas_v2))])
      
      coef_list_v2[[r_v2]] <- betas_v2
      var_list_v2[[r_v2]]  <- top_features_v2
      
      fidelity_vec_v2 <- c(fidelity_vec_v2, r2_vals_v2[best_idx_v2])
      kw_vec_v2 <- c(kw_vec_v2, best_kw_v2)
    }
    
    # Stability
    stab_v2 <- compute_vsi_csi_v2(coef_list_v2, var_list_v2)
    
    results_list_v2[[length(results_list_v2)+1]] <- data.frame(
      Class = y_test_v2[i_v2],
      Fidelity = mean(fidelity_vec_v2),
      KW = mean(kw_vec_v2),
      VSI = stab_v2$VSI,
      CSI = stab_v2$CSI
    )
  }
  
  final_df_v2 <- do.call(rbind, results_list_v2)
  
  #--------------------------------------------------
  # Summary
  #--------------------------------------------------
  summary_stats_v2 <- final_df_v2 %>%
    group_by(Class) %>%
    summarise(
      Mean_Fidelity = mean(Fidelity),
      Mean_KW = mean(KW),
      Mean_VSI = mean(VSI, na.rm=TRUE),
      Mean_CSI = mean(CSI, na.rm=TRUE),
      N = n(),
      .groups="drop"
    )
  
  # Missing class fix
  all_classes_v2 <- c("0","1")
  
  summary_stats_v2 <- summary_stats_v2 %>%
    complete(
      Class = all_classes_v2,
      fill = list(
        Mean_Fidelity = 0,
        Mean_KW = 0,
        Mean_VSI = 0,
        Mean_CSI = 0,
        N = 0
      )
    )
  
  cat("\n---", dataset_name_v2,"---\n")
  print(summary_stats_v2)
  
  return(summary_stats_v2)
}

#--------------------------------------------------
# 4. Run
#--------------------------------------------------
dataset_names_v2 <- c( "abalone" ,"abalone_19" ,"churn","jm1",               
                    "kc1",  "MagicTelescope",   "mammography", "ozone_level_8hr",   
                    "pc1" , "pc3",   "pc4","phoneme"  ,         
                    "spambase","SpeedDating","steel_plates_fault", "us_crime" ,         
                    "wilt","wine_quality", "yeast_me2","yeast_ml8"   )

all_results_v2 <- list()

for(name_v2 in dataset_names_v2){
  if(exists(name_v2)){
    tryCatch({
      data_prep_v2 <- standardize_Class_v2(get(name_v2),"Class")
      all_results_v2[[name_v2]] <- run_optilime_stable_v2(data_prep_v2, name_v2)
    }, error=function(e){
      cat("\nHata:",name_v2,"-",e$message)
    })
  }
}

final_results_optilime_v2 <- do.call(rbind, all_results_v2)


class_summary_optilime <- final_results_optilime_v2 %>%
  group_by(Class) %>%
  summarise(
    Mean_Fidelity = mean(Mean_Fidelity, na.rm = TRUE),
    Mean_VSI      = mean(Mean_VSI, na.rm = TRUE),
    Mean_CSI      = mean(Mean_CSI, na.rm = TRUE)
  )

print(class_summary_optilime)