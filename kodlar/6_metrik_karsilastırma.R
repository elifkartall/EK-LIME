library(ggstatsplot)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Veri Hazırlama
safe_prepare <- function(df, method_name) {
  df %>%
    rename_with(tolower) %>%
    rename(
      Fidelity = matches("fidelity"),
      VSI = matches("vsi"),
      CSI = matches("csi"),
      Sınıf = matches("sınıf|sinif|class")
    ) %>%
    select(Fidelity, VSI, CSI, Sınıf) %>%
    mutate(Method = method_name)
}

combined_df_clean <- bind_rows(
  safe_prepare(final_comparison_df, "LIME"),
  safe_prepare(final_comparison_optilime, "Opti-LIME"),
  safe_prepare(final_df_smote_collected, "SMOTE+OptiLIME"),
  safe_prepare(final_df_EK_collected, "EK-LIME")
) %>%
  pivot_longer(cols = c(Fidelity, VSI, CSI), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(
    Method = factor(Method, levels = c("LIME", "Opti-LIME", "SMOTE+OptiLIME", "EK-LIME")),
    Metric = recode(Metric,
                    "Fidelity" = "Yerel Doğruluk",
                    "VSI" = "DKİ",
                    "CSI" = "KKİ")
  )

# 2. Grafik Fonksiyonu
generate_ultra_clean_plot <- function(class_label) {
  grouped_ggbetweenstats(
    data = filter(combined_df_clean, Sınıf == class_label),
    x = Method,
    y = Value,
    grouping.var = Metric,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "significant",
    sample.size.label = FALSE,
    bf.message = FALSE,
    mean.plotting = TRUE,
    package = "RColorBrewer",
    palette = "Set2",
    plotgrid.args = list(nrow = 1),
    annotation.args = list(
      title = NULL,
      subtitle = NULL
    ),
    ggplot.component = list(
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15)
      ),
      scale_x_discrete(labels = c("LIME", "Opti-LIME", "SMOTE+OptiLIME", "EK-LIME"))
    )
  )
}

# 3. Çalıştır
plot_0 <- generate_ultra_clean_plot("0")
plot_1 <- generate_ultra_clean_plot("1")

print(plot_0)
print(plot_1)



































library(ggstatsplot)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Veri Hazırlama
safe_prepare <- function(df, method_name) {
  df %>%
    rename_with(tolower) %>%
    rename(
      Fidelity = matches("fidelity"),
      VSI = matches("vsi"),
      CSI = matches("csi"),
      Sınıf = matches("sınıf|sinif|class")
    ) %>%
    select(Fidelity, VSI, CSI, Sınıf) %>%
    mutate(Method = method_name)
}

combined_df_clean <- bind_rows(
  safe_prepare(final_comparison_df, "LIME"),
  safe_prepare(final_comparison_optilime, "Opti-LIME"),
  safe_prepare(final_df_smote_collected, "SMOTE+OptiLIME"),
  safe_prepare(final_df_EK_collected, "EK-LIME")
) %>%
  pivot_longer(cols = c(Fidelity, VSI, CSI),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(
    Method = factor(Method, levels = c("LIME", "Opti-LIME", "SMOTE+OptiLIME", "EK-LIME")),
    Metric = recode(Metric,
                    "Fidelity" = "Yerel Doğruluk",
                    "VSI" = "Değişken Kararlılık İndeksi (DKİ)",
                    "CSI" = "Katsayı Kararlılık İndeksi (KKİ)")
  )

# 2. Grafik Fonksiyonu
generate_ultra_clean_plot <- function(class_label) {
  grouped_ggbetweenstats(
    data = filter(combined_df_clean, Sınıf == class_label),
    x = Method,
    y = Value,
    grouping.var = Metric,
    type = "parametric",
    pairwise.comparisons = TRUE,
    pairwise.display = "significant",
    sample.size.label = FALSE,
    bf.message = FALSE,
    mean.plotting = TRUE,
    package = "RColorBrewer",
    palette = "Set2",
    plotgrid.args = list(nrow = 1),
    annotation.args = list(
      title = NULL,
      subtitle = NULL
    ),
    ggplot.component = list(
      theme_bw(base_size = 11),   # base size burada kullanıldı
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 12),
        legend.position = "none"
      ),
      scale_x_discrete(labels = c("LIME", "Opti-LIME", "SMOTE+OptiLIME", "EK-LIME"))
    )
  )
}

# 3. Çalıştır
plot_0 <- generate_ultra_clean_plot("0")
plot_1 <- generate_ultra_clean_plot("1")

print(plot_0)
print(plot_1)