# ============================================================
# Grafik 4: Göç Nedeni × Eğitim Durumu Heatmap
# ============================================================
# install.packages(c("ggplot2", "dplyr", "tidyr", "readxl", "forcats"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)

# ── VERİ ─────────────────────────────────────────────────────
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

# ── NEDEN ETİKETLERİ ─────────────────────────────────────────
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

# ── EĞİTİM ETİKETLERİ ────────────────────────────────────────
egitim_labels <- c(
  "okuma_yok" = "Okuma Yazma\nBilmeyen",
  "okuma_var" = "Diplomasız\nOkuryazar",
  "ilkokul"   = "İlkokul",
  "ortaokul"  = "Ortaokul",
  "lise"      = "Lise",
  "yuksek"    = "Yükseköğretim"
)

# ── LONG FORMAT + SATIR İÇİ YÜZDE ────────────────────────────
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

# ── GRAFİK ───────────────────────────────────────────────────
p4 <- ggplot(df4, aes(x = egitim, y = fct_rev(neden_kisa), fill = pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), "%"),
                color  = ifelse(pct > 35, "white", "grey20")),
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(
    low      = "#F7F4F0",
    mid      = "#6BAED6",
    high     = "#08306B",
    midpoint = 25,
    name     = "Satır İçi\nPay (%)",
    guide = guide_colorbar(
      barwidth       = 10,
      barheight      = 0.6,
      title.position = "top",
      title.hjust    = 0.5,
      direction      = "horizontal"
    )
  ) +
  scale_color_identity() +
  labs(
    title    = "Göç Nedeni × Eğitim Durumu",
    subtitle = "Her nedenin eğitim dağılımı (satır içi yüzde) · 2024 · Kaynak: TÜİK",
    x        = NULL,
    y        = NULL,
    caption  = "Renkler her göç nedeni içindeki eğitim payını göstermektedir"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title       = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle    = element_text(size = 9,  color = "grey40", hjust = 0,
                                    margin = margin(b = 10)),
    plot.caption     = element_text(size = 8,  color = "grey55"),
    axis.text.x      = element_text(size = 12, face = "bold", hjust = 0.5, color = "grey10"),
    axis.text.y      = element_text(size = 12, face = "bold", hjust = 1,   color = "grey10"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin      = margin(15, 15, 10, 10)
  )

print(p4)

# ── KAYDET ───────────────────────────────────────────────────
ggsave("grafik4_heatmap_egitim_neden.png",
       plot = p4, width = 11, height = 7, dpi = 300, bg = "white")
message("grafik4_heatmap_egitim_neden.png kaydedildi")