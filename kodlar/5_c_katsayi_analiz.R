library(dplyr)
library(tidyr)
library(ggplot2)

# Veri Hazırlama
katsayi_kararlilik_df <- final_df %>%
  mutate(sb_factor = factor(sb)) %>%
  pivot_longer(cols = c(VSI, CSI), names_to = "Metric", values_to = "Score")

# Görselleştirme
ggplot(katsayi_kararlilik_df, aes(x = sb_factor, y = Score)) +
  geom_boxplot(
    fill = "#3C8DBC", 
    alpha = 0.7, 
    outlier.shape = 21, 
    outlier.fill = "white",
    width = 0.6
  ) +
  geom_jitter(width = 0.1, alpha = 0.15, size = 0.7) +
  facet_wrap(~Metric, ncol = 1, scales = "free_y") +
  
  theme_bw(base_size = 14) +
  
  theme(
    axis.title.y = element_blank(),
    
    axis.title.x = element_text(
      size = 12, 
      face = "plain", 
      margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0)
    ),
    
    axis.text.x = element_text(size = 11, color = "black"),
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    
    plot.title = element_text(hjust = 0, face = "plain", size = 12)
  ) +
  
  labs(
    title = "Kararlılık Katsayısının Metrikler Üzerinde Etkisi",
    x = "Kararlılık Katsayısı"
  )