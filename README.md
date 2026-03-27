# EK-LIME: Dengesiz Sınıflandırmada Yerel Doğruluk ve Kararlılık Dengelemesi

> **Özet:** Bu çalışma, LIME açıklamalarında yerel doğruluk ile kararlılık arasındaki ödünleşim sorununu, özellikle dengesiz veri koşullarında incelemektedir. Yerel doğruluk ve kararlılık dengesini artırmak amacıyla dar ve geniş çekirdeği birlikte kullanan Ensemble Kernel LIME yaklaşımı önerilmiştir. Yöntem, LIME, DLIME, GLIME ve OptiLIME ile farklı dengesizlik oranlarına sahip 20 verisetinde karşılaştırılmış; performans yerel doğruluk, değişken kararlılık indeksi ve katsayı kararlılık indeksi üzerinden değerlendirilmiştir. Bulgular, EK-LIME'ın her iki sınıfta da doğruluk-kararlılık dengesini iyileştirdiğini; çoğunluk sınıfında $0.98$, azınlık sınıfında $0.99$ ve değişken kararlılık indeksi değerlerinin sırasıyla $0.82$ ve $0.80$ olduğunu göstermektedir.

---

##  Proje İçeriği

- 📊 [Veri Setleri](./verisetleri) - Üzerinden analizde kullanılan farklı dengesizlik oranlarına sahip 20 veri setine ulaşılabilir.
- 📂 [Kodlar](./kodlar) - Üzerinden deneyleri tekrarlamak için gerekli R scriptlerine ulaşılabilir.
   Dosya içerisinde deney süreci boyunca takip edilen adımlar scriptler halinde eklenmiştir.

  
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

##  Proje Akışı
```ek-lime-repo/
├── data/                         # Veri setlerini yükleyin 
├── scripts/                      # Sıra ile aşağıdaki R scriptlerini çalıştırın 
│   ├── lime.R                    # LIME uygulaması
│   ├── dlime.R                   # DLIME uygulaması
│   ├── glime.R                   # GLIME uygulaması
│   ├── optilime.R                # Opti-LIME uygulaması 
│   ├── ek-lime.R                 # Önerilen ensemble kernel yaklaşımı uygulaması
│   └── metrikler.R               # Yöntemler arası yerel doğruluk ve kararlılık kıyaslaması
├── outputs/                      # Beklenen Çıktılar 
│  
│   ├── yontem_karsilastirma.plot # Azınlık(0) sınıfı için yöntemler arası yerel doğruluk ve kararlılık kıyas grafiği
│   └── yontem_karsilastirma.plot # Çoğunluk(1) sınıfı için yöntemler arası yerel doğruluk ve kararlılık kıyas grafiği
└── README.md
```
## Bulgular

### 1. Çoğunluk Sınıfı Performansı

<img width="1920" height="947" alt="metrikler0" src="https://github.com/user-attachments/assets/371e1eee-9da5-466a-b5d5-18df6513e079" />


>Çoğunluk sınıfı için EK-LIME yöntemi $0.98$ ortalama yerel doğruluk değeri ile LIME, DLIME, GLIME ve OptiLIME  yöntemlerinden daha yüksek yerel doğruluk sağlamıştır. Kararlılık açısından DKİ ve KKİ metriklerinde en yüksek değerler DLIME ile elde ediliyor olsa da bu iyileşimin yerel doğruluk değerini düşürdüğü görülmektedir. 

İstatistiksel analizlere göre, yöntemler arasındaki performans farklarını doğrulamaktadır: Yerel doğruluk üzerinde yöntem etkisi istatistiksel olarak anlamlıdır ($F_{\text{Welch}}(4, 33.53) = 136.51$, $p = 2.98 \times 10^{-20}$, $\hat{\omega}_{p}^{2} = 0.93$). Holm düzeltmeli Games--Howell karşılaştırmalarında EK-LIME'ın ($\hat{\mu}_{\text{mean}} = 0.98$), hem LIME ($p = 1.88 \times 10^{-6}$) hem de Opti-LIME ($p = 0.02$) yöntemlerine kıyasla anlamlı düzeyde daha yüksek yerel doğruluğa ulaştığı saptanmıştır. Benzer şekilde, Değişken Kararlılık İndeksi (DKİ) metriğinde de yöntemler arası fark anlamlı bulunmuş ($F_{\text{Welch}}(4, 36.18) = 18.89$, $p = 1.83 \times 10^{-8}$, $\hat{\omega}_{p}^{2} = 0.63$) ve EK-LIME'ın LIME ile olan farkı ($p = 1.15 \times 10^{-4}$) istatistiksel olarak teyit edilmiştir. Katsayı Kararlılık İndeksi (KKİ) üzerinde de yöntem etkisinin anlamlı olduğu görülmekte ($F_{\text{Welch}}(4, 36.07) = 13.56$, $p = 7.70 \times 10^{-7}$, $\hat{\omega}_{p}^{2} = 0.55$), post-hoc testler EK-LIME'ın Opti-LIME ($p = 4.87 \times 10^{-5}$) ve GLIME ($p = 3.55 \times 10^{-5}$) karşısında daha yüksek kararlılık sergilediğini göstermektedir.

### 2. Azınlık Sınıfı Performansı

<img width="1920" height="947" alt="metrikler1" src="https://github.com/user-attachments/assets/1bd8b267-11d3-4a28-81cd-9550ec2401ac" />

> Azınlık sınıfı sonuçları incelendiğinde, EK-LIME yöntemi $0.99$ ortalama doğruluk değeri ile en yüksek doğruluğa ulaşmıştır. Kararlılık açısından, Değişken Kararlılık İndeksi (DKİ) ve Katsayı Kararlılık İndeksi (KKİ) metriklerinde en yüksek değerler DLIME ile elde edilse de, yerel doğruluğun düşük olduğu görülmüştür.  

Yerel doğruluk üzerinde yöntem etkisi istatistiksel olarak anlamlı bulunmuştur. F_Welch(4, 31.99) = 148.40, p = 3.59e-20, ω̂p² = 0.94.  
Holm düzeltmeli Games–Howell post-hoc karşılaştırmalarında, EK-LIME'ın (μ̂_mean = 0.99) hem LIME (p = 2.18e-9) hem de Opti-LIME (p = 4.46e-3) yöntemlerine kıyasla anlamlı düzeyde daha yüksek yerel doğruluğa ulaştığı saptanmıştır. Benzer şekilde, DKİ metriğinde de yöntemler arası fark anlamlı bulunmuş:F_Welch(4, 35.53) = 24.41, p = 8.96e-10, ω̂p² = 0.70,  
ve EK-LIME'ın LIME ile olan farkı (p = 9.95e-6) istatistiksel olarak teyit edilmiştir. KKİ üzerinde de yöntem etkisinin anlamlı olduğu görülmektedir:  
F_Welch(4, 35.99) = 10.85, p = 7.11e-6, ω̂p² = 0.49, post-hoc testler EK-LIME'ın Opti-LIME (p = 6.86e-4) ve DLIME (p = 1.64e-4) karşısında daha yüksek kararlılık sergilediğini göstermektedir.


---

## Sonuç
Genel olarak sonuçlar, EK-LIME yönteminin LIME açıklamalarında yerel doğruluk ve kararlılık arasındaki ödünleşim sorununu azaltarak daha kararlı ve yerel doğruluk oranı yüksek açıklamalar ürettiği gözlenmiştir..
