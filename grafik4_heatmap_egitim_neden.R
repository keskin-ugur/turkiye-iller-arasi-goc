# ============================================================
# Grafik 4: Göç Nedeni × Eğitim Durumu Heatmap — Poster Nihai
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)

# ── 1. VERİ OKUMA VE TEMİZLİK ────────────────────────────────
raw4 <- read_excel(
  "goc_etme_nedeni_ve_bitirilen_egitim_durumuna_gore iller_arasi_goc_eden_nufus.xls",
  col_names = FALSE, skip = 3
)

df4_raw <- raw4 %>%
  select(neden = 1, toplam = 2,
         okuma_yok = 3, okuma_var = 4, ilkokul = 5,
         ortaokul = 6, lise = 7, yuksek = 8, bilinmez = 9) %>%
  filter(
    !is.na(neden),
    neden != "Toplam-Total",
    !grepl("TÜİK|TurkStat|sıfır|zero|Gerçek|Emeklilik|Diğer|Bilinmeyen|Retirement|Other|Unknown",
           neden, ignore.case = TRUE)
  ) %>%
  mutate(across(-neden, as.numeric)) %>%
  filter(!is.na(toplam))

# Etiket Düzenlemeleri
df4_raw <- df4_raw %>%
  mutate(neden_kisa = case_when(
    grepl("Tayin|Assignation",  neden) ~ "Tayin / İş Değişikliği",
    grepl("baslamak|finding",   neden) ~ "İş Bulmak",
    grepl("Egitim|Education",   neden) ~ "Eğitim",
    grepl("Medeni|marital",     neden) ~ "Evlilik / Aile",
    grepl("konut|housing",      neden) ~ "Daha İyi Konut",
    grepl("Hane|household",     neden) ~ "Hane Bağımlı Göç",
    grepl("memleket|hometown",  neden) ~ "Memlekete Dönüş",
    grepl("Saglik|Health",      neden) ~ "Sağlık / Bakım",
    grepl("Ev alinmasi|Buying", neden) ~ "Ev Alınması",
    TRUE ~ neden
  ))

neden_sira <- c(
  "Tayin / İş Değişikliği", "İş Bulmak", "Eğitim",
  "Evlilik / Aile", "Daha İyi Konut", "Hane Bağımlı Göç",
  "Memlekete Dönüş", "Sağlık / Bakım", "Ev Alınması"
)

egitim_labels <- c(
  "okuma_yok" = "Okuma Yazma\nBilmeyen",
  "okuma_var" = "Diplomasız\nOkuryazar",
  "ilkokul"   = "İlkokul",
  "ortaokul"  = "Ortaokul",
  "lise"      = "Lise",
  "yuksek"    = "Yükseköğretim"
)

# Long Format ve Yüzde Hesaplama
df4 <- df4_raw %>%
  select(neden_kisa, all_of(names(egitim_labels))) %>%
  pivot_longer(cols = -neden_kisa,
               names_to  = "egitim_key",
               values_to = "sayi") %>%
  mutate(egitim = egitim_labels[egitim_key]) %>%
  group_by(neden_kisa) %>%
  mutate(pct = sayi / sum(sayi) * 100) %>%
  ungroup() %>%
  mutate(
    neden_kisa = factor(neden_kisa, levels = neden_sira),
    egitim     = factor(egitim, levels = unname(egitim_labels))
  )

# ── 2. GRAFİK TASARIMI (TÜM YAZILAR #1E3A8A) ────────────────
p4 <- ggplot(df4, aes(x = egitim, y = fct_rev(neden_kisa), fill = pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  # Kutucuk içi yüzdeler: Yüksek değerler beyaz, düşük değerler #1e3a8a
  geom_text(aes(label = paste0(round(pct, 1), "%"),
                color  = ifelse(pct > 35, "white", "#1e3a8a")),
            size = 6, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#F7F4F0",
    mid      = "#6BAED6",
    high     = "#08306B",
    midpoint = 25,
    name     = "Pay (%)",
    guide = guide_colorbar(barwidth = 1, barheight = 15, title.position = "top", title.hjust = 0.5)
  ) +
  scale_color_identity() +
  labs(
    title    = "GÖÇ NEDENİ × EĞİTİM DURUMU",
    subtitle = "Her nedenin eğitim düzeyine göre dağılımı (Satır içi yüzde) · 2024",
    x        = NULL,
    y        = NULL,
    caption  = "Kaynak: TÜİK · Renk koyuluğu ilgili neden içindeki eğitim payını temsil eder."
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    # Başlık ve Alt Başlık (#1e3a8a)
    plot.title       = element_text(size = 28, face = "bold", color = "#1e3a8a"),
    plot.subtitle    = element_text(size = 16, color = "#1e3a8a", margin = margin(b = 20)),
    plot.caption     = element_text(size = 12, color = "#1e3a8a"),
    
    # Eksen Yazıları (#1e3a8a ve Büyük)
    axis.text.x      = element_text(size = 16, face = "bold", color = "#1e3a8a", margin = margin(t = 10)),
    axis.text.y      = element_text(size = 16, face = "bold", color = "#1e3a8a"),
    
    # Lejant (#1e3a8a)
    legend.title     = element_text(size = 14, face = "bold", color = "#1e3a8a"),
    legend.text      = element_text(size = 12, color = "#1e3a8a"),
    
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "#eef2fa", color = NA),
    panel.background = element_rect(fill = "#eef2fa", color = NA),
    plot.margin      = margin(30, 30, 30, 30)
  )

# ── 3. KAYDET ────────────────────────────────────────────────
ggsave("grafik4_heatmap_poster.png",
       plot = p4, width = 16, height = 11, dpi = 300, bg = "#eef2fa")
