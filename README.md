# EK-LIME: Dengesiz Sınıflandırmada Yerel Doğruluk ve Kararlılık Dengelemesi

> **Özet:** Bu çalışma, LIME açıklamalarında yerel doğruluk ile kararlılık arasındaki ödünleşim sorununu, özellikle dengesiz veri koşullarında incelemektedir. Bu amaçla, yalnızca eğitim verisine uygulanan SMOTE dengelemesi ile dar ve geniş çekirdeği birlikte kullanan Ensemble Kernel LIME yaklaşımı önerilmiştir. Yöntem, LIME, OptiLIME ve SMOTE+OptiLIME ile farklı dengesizlik oranlarına sahip 20 verisetinde karşılaştırılmış; performans yerel doğruluk, değişken kararlılık indeksi ve katsayı kararlılık indeksi üzerinden değerlendirilmiştir. Bulgular, EK-LIME'ın her iki sınıfta da doğruluk-kararlılık dengesini iyileştirdiğini; özellikle çoğunluk sınıfında $0.91$, azınlık sınıfında $0.92$ ve değişken kararlılık indeksi değerlerinin sırasıyla $0.74$ ve $0.72$ olduğunu göstermektedir.

---

##  Proje İçeriği

- 📊 [Veri Setleri](./verisetleri) - Üzerinden analizde kullanılan farklı dengesizlik oranlarına sahip 20 veri setine ulaşılabilir.
- 📂 [Kodlar](./kodlar) - Üzerinden deneyleri tekrarlamak için gerekli R scriptlerine ulaşılabilir.
   Dosya içerisinde deney süreci boyunca takip edilen adımlar scriptler halinde eklenmiştir.

##  Proje Akışı
ek-lime-repo/
├── data/                         # Veri setlerini yükleyin ->  [Veri Setleri](./verisetleri) 
├── scripts/                      # Sıra ile [Kodlar](./kodlar) dosyasında bulunan aşağıdaki R scriptlerini çalıştırın 
│   ├── 1_dengesiz_lime.R         # Dengesiz veri setleri ile LIME 
│   ├── 2_dengesiz_optilime.R     # Dengesiz veri setleri ile OptiLIME
│   ├── 3_smote_optilime.R        # SMOTE sonrası OptiLIME
│   ├── 4_ek_lime.R               # önerilen yöntem EK-LIME 
│   ├── 5_c_katsayi_analiz.R      # c Katsayısının final açıklama metrikleri üzerinde etkisinin incelenmesi
│   └── 6_metrik_karsilastirma.R  # Yöntemler arası yerel doğruluk ve kararlılık kıyaslaması
├── outputs/                      # Beklenen Çıktılar 
│   ├── katsayi_kararlilik.plot   # c Katsayısının final açıklama metrikleri üzerinde etkisini gösteren grafik (5. adımda)
│   ├── yontem_karsilastirma.plot # Azınlık(0) sınıfı için yöntemler arası yerel doğruluk ve kararlılık kıyas grafiği
│   └── yontem_karsilastirma.plot # Çoğunluk(1) sınıfı için yöntemler arası yerel doğruluk ve kararlılık kıyas grafiği
└── README.md
  
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

## Bulgular

### 1. Çoğunluk Sınıfı Performansı
<img width="100%" alt="Majority Class Analysis" src="https://github.com/user-attachments/assets/58e0bc26-0e12-4755-b2f1-d9925b0ffc68" />

> Çoğunluk sınıfı için **EK-LIME** yöntemi $0.91$ ortalama yerel doğruluk değeri ile LIME, OptiLIME ve SMOTE+OptiLIME yöntemlerinden daha yüksek yerel doğruluk sağlamıştır. DKİ metriğinde ise EK-LIME $0.74$ ile en yüksek stabiliteye ulaşmıştır. İstatistiksel testler bu eğilimi desteklemektedir ($F_{\text{Welch}}(3,8.85)=43.92$, $p < 0.001$).

### 2. Azınlık Sınıfı Performansı
<img width="100%" alt="Minority Class Analysis" src="https://github.com/user-attachments/assets/abf93e07-15e5-445f-9d33-ef020c82d465" />

> Azınlık sınıfı sonuçları incelendiğinde **EK-LIME** yönteminin $0.92$ ortalama doğruluk değeri ile en yüksek doğruluğa ulaştığı görülmüştür. DKİ metriğinde ise $0.72$ ortalama değer ile en yüksek stabiliteyi sağlayarak daha tutarlı özellik seçimleri üretmiştir ($F_{\text{Welch}}(3,9.15)=40.76$, $p < 0.001$).

---

## Sonuç
Genel olarak sonuçlar, EK-LIME yönteminin LIME açıklamalarında özellikle azınlık sınıf için yerel doğruluk ve kararlılık arasındaki ödünleşim sorununu azaltarak daha kararlı ve yerel doğruluk oranı yüksek açıklamalar ürettiği gözlenmiştir..
