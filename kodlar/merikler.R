library(ggstatsplot)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Veri Hazırlama
safe_prepare <- function(df, method_name) {
  df %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename(
      Fidelity = matches("mean_fidelity|ortalama_fidelity|ort_fidelity"),
      VSI = matches("vsi|mean_vsi|ort_vsi"),
      CSI = matches("csi|mean_csi|ort_csi"),
      Sınıf = matches("sınıf|sinif|class")
    ) %>%
    dplyr::select(Fidelity, VSI, CSI, Sınıf) %>%
    dplyr::mutate(Method = method_name)
}

combined_df_clean <- bind_rows(
  safe_prepare(final_comparison_df, "LIME"),
  safe_prepare(final_table_dlime, "DLIME"),
  safe_prepare(final_glime, "GLIME"),
  safe_prepare(final_results_optilime_v2, "Opti-LIME"),
  safe_prepare(final_table_new, "EK-LIME")
) %>%
  pivot_longer(cols = c(Fidelity, VSI, CSI),
               names_to = "Metric",
               values_to = "Value") %>%
  mutate(
    Method = factor(Method, levels = c("LIME", "DLIME", "GLIME", "Opti-LIME","EK-LIME")),
    Metric = recode(Metric,
                    "Fidelity" = "Yerel Doğruluk",
                    "VSI" = "Değişken Kararlılık İndeksi (DKİ)",
                    "CSI" = "Katsayı Kararlılık İndeksi (KKİ)")
  )

# 2. Grafik Fonksiyonu
generate_plot <- function(class_label) {
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
    
  
    centrality.label.args = list(
      size = 4,
      fontface = "bold"
    ),
    
    package = "RColorBrewer",
    palette = "Set2",
    plotgrid.args = list(nrow = 1),
    annotation.args = list(
      title = NULL,
      subtitle = NULL
    ),
    
    ggplot.component = list(
      theme_bw(base_size = 11),
      theme(
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 12),
        legend.position = "none"
      ),
      scale_x_discrete(labels = c("LIME", "DLIME", "GLIME", "Opti-LIME","EK-LIME"))
    )
  )
}

plot_0 <- generate_plot("0")
plot_1 <- generate_plot("1")

print(plot_0)
print(plot_1)
