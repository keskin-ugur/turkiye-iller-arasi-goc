# 🗺️ Türkiye'de İller Arası İç Göç: Kim, Nereden Nereye, Neden Gidiyor?

Veri görselleştirme dersi kapsamında hazırlanan bu çalışma, Türkiye'de iller arası iç göç hareketlerini farklı boyutlarıyla analiz etmektedir. TÜİK'in 2008–2024 yıllarına ait il bazlı göç verileri kullanılarak göçün yönü, nedenleri ve demografik profili görselleştirilmiştir.

---

## 📊 Grafikler

| Dosya | İçerik |
|---|---|
| `grafik1_harita_net_goc.R` | Türkiye illeri net göç hızı — choropleth harita (2024) |
| `grafik2_ic_goc_sankey.R` | Türkiye'de hangi illerden hangi illere göçülmüş — sankey diyagramı |
| `grafik3_il_neden.R` | En fazla göç alan 15 ilin göç nedeni dağılımı (2024) |
| `grafik4_heatmap_egitim_neden.R` | Göç nedeni × eğitim durumu ısı haritası (2024) |
| `grafik5_slope_yon_degistiren.R` | Net göçte yön değiştiren iller — slope chart (2008 vs 2024) |
| `grafik6_dumbbell_alinan_verilen_fark.R` | Alınan ve verilen göç farkı en büyük 20 il — dumbbell chart (2024) |

---

## 📦 Kullanılan R Paketleri

```r
install.packages(c(
  "ggplot2", "dplyr", "tidyr", "readxl", "scales", "sysfonts", "purrr"
  "sf", "geodata", "terra", "rnaturalearth", "rnaturalearthdata",
  "stringi", "ggrepel", "ggtext", "forcats", "patchwork", "showtext"
))
```

---

## 🗂️ Veri Kaynağı

Tüm veriler **TÜİK (Türkiye İstatistik Kurumu)**'ten alınmıştır.

🔗 [https://veriportali.tuik.gov.tr/tr/press/54082](İç Göç İstatistikleri, 2024)
🔗 [https://nip.tuik.gov.tr/?value=IllerArasiGoc](İller Arası Göç)

Kullanılan veri setleri:

- İllerin aldığı göç, verdiği göç, net göç ve net göç hızı
- Cinsiyete göre iller arası göç eden nüfus ve oranı
- Göç etme nedenine göre illerin aldığı göç
- Göç etme nedenine göre illerin verdiği göç
- Göç etme nedeni ve bitirilen eğitim durumuna göre iller arası göç eden nüfus
- Yaş grubu ve cinsiyete göre iller arası göç eden nüfus
- İller arası göç (2008–2024)

---

## ⚙️ Nasıl Çalıştırılır?

1. Repoyu klonlayın
2. Gerekli paketleri kurun (yukarıdaki kod bloğu)
3. TÜİK'ten ilgili veri dosyalarını indirip proje klasörüne koyun
4. İlgili `.R` dosyasını RStudio'da açıp çalıştırın

---

## 👥 Hazırlayanlar

**Uğur Keskin** & **Mehmet Yiğit Çağlayan**
