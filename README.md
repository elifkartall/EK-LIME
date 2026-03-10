# EK-LIME: Dengesiz Sınıflandırmada Yerel Doğruluk ve Kararlılık Dengelemesi

> **Özet:** Bu çalışma, LIME açıklamalarında yerel doğruluk ile kararlılık arasındaki ödünleşim (trade-off) sorununu incelemektedir. Dar ve geniş çekirdeği birlikte kullanan **Ensemble Kernel LIME (EK-LIME)** yaklaşımı ile özellikle dengesiz veri setlerinde daha güvenilir açıklamalar üretilmesi hedeflenmiştir.

---

##  Proje İçeriği
- 📂 [Kodlar](./kodlar) - Deneyleri tekrarlamak için gerekli R scriptleri.
- 📊 [Veri Setleri](./verisetleri) - Analizde kullanılan 20 farklı veri seti.

###  Kullanılan Teknolojiler
<p align="left">
  <img src="https://img.shields.io/badge/R_Language-276DC3?style=for-the-badge&logo=r&logoColor=white" />
  <br>
  <img src="https://img.shields.io/badge/dplyr-1a162d?style=for-the-badge&logo=tidyverse&logoColor=white" />
  <img src="https://img.shields.io/badge/caret-FFA500?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/tidyr-1a162d?style=for-the-badge&logo=tidyverse&logoColor=white" />
  <img src="https://img.shields.io/badge/smotefamily-FF6347?style=for-the-badge&logo=r&logoColor=white" />
  <img src="https://img.shields.io/badge/ggplot2-1a162d?style=for-the-badge&logo=tidyverse&logoColor=white" />
  <img src="https://img.shields.io/badge/ggstatsplot-008080?style=for-the-badge&logo=r&logoColor=white" />
</p>

---

##  Deney Sonuçları ve Bulgular

### 1. Çoğunluk Sınıfı Performansı
<img width="100%" alt="Majority Class Analysis" src="https://github.com/user-attachments/assets/58e0bc26-0e12-4755-b2f1-d9925b0ffc68" />

> Çoğunluk sınıfı için **EK-LIME** yöntemi $0.91$ ortalama yerel doğruluk değeri ile LIME, OptiLIME ve SMOTE+OptiLIME yöntemlerinden daha yüksek yerel doğruluk sağlamıştır. DKİ metriğinde ise EK-LIME $0.74$ ile en yüksek stabiliteye ulaşmıştır. İstatistiksel testler bu eğilimi desteklemektedir ($F_{\text{Welch}}(3,8.85)=43.92$, $p < 0.001$).

### 2. Azınlık Sınıfı Performansı
<img width="100%" alt="Minority Class Analysis" src="https://github.com/user-attachments/assets/abf93e07-15e5-445f-9d33-ef020c82d465" />

> Azınlık sınıfı sonuçları incelendiğinde **EK-LIME** yönteminin $0.92$ ortalama doğruluk değeri ile en yüksek doğruluğa ulaştığı görülmüştür. DKİ metriğinde ise $0.72$ ortalama değer ile en yüksek stabiliteyi sağlayarak daha tutarlı özellik seçimleri üretmiştir ($F_{\text{Welch}}(3,9.15)=40.76$, $p < 0.001$).

---

## 📝 Sonuç Özeti
Genel olarak sonuçlar, EK-LIME yönteminin LIME açıklamalarında özellikle azınlık sınıf için yerel doğruluk ve kararlılık arasındaki ödünleşim sorununu azaltarak daha kararlı ve yerel doğruluk oranı yüksek açıklamalar ürettiği gözlenmiştir..
