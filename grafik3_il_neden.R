# ============================================================
# Grafik 3: İl Bazlı Göç Nedeni — Sadece Alınan Göç (Top 15 il)
# Kaynak: TÜİK İç Göç İstatistikleri, 2024
# ============================================================

# install.packages(c("ggplot2", "dplyr", "tidyr", "readxl", "forcats", "scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)
library(scales)

# ── NEDEN ETİKETLERİ ─────────────────────────────────────────
neden_labels <- c(
  "tayin"    = "Tayin / Is Degisikligi",
  "is"       = "Is Bulmak",
  "egitim"   = "Egitim",
  "medeni"   = "Evlilik / Aile",
  "konut"    = "Daha Iyi Konut",
  "hane"     = "Hane Bagimli Goc",
  "donus"    = "Memlekete Donus",
  "saglik"   = "Saglik / Bakim",
  "ev"       = "Ev Alinmasi",
  "emekli"   = "Emeklilik",
  "diger"    = "Diger",
  "bilinmez" = "Bilinmeyen"
)

# ── VERİ ─────────────────────────────────────────────────────
raw <- read_excel("goc_etme_nedenine_gore_illerin_aldigi_goc.xls",
                  col_names = FALSE, skip = 3)

alinan <- raw %>%
  select(il = 1, toplam = 2,
         tayin = 3, is = 4, egitim = 5, medeni = 6,
         konut = 7, hane = 8, donus = 9,
         saglik = 10, ev = 11, emekli = 12,
         diger = 13, bilinmez = 14) %>%
  filter(
    !is.na(il),
    il != "Toplam-Total",
    !grepl("TUIK|TurkStat|sifir|zero|Gercek", il, ignore.case = TRUE)
  ) %>%
  mutate(
    il    = trimws(il),
    across(-il, as.numeric)
  ) %>%
  filter(!is.na(toplam))

# ── EN FAZLA GÖÇ ALAN 15 İL ──────────────────────────────────
top15 <- alinan %>%
  arrange(desc(toplam)) %>%
  slice_head(n = 15) %>%
  pull(il)

# ── LONG FORMAT ──────────────────────────────────────────────
df3 <- alinan %>%
  filter(il %in% top15) %>%
  pivot_longer(cols = all_of(names(neden_labels)),
               names_to  = "neden_key",
               values_to = "sayi") %>%
  mutate(
    neden = neden_labels[neden_key],
    neden = factor(neden, levels = rev(unname(neden_labels))),
    il    = factor(il, levels = rev(top15))
  )

# Toplam sayıyı etiket için hazırla
toplam_df <- alinan %>%
  filter(il %in% top15) %>%
  mutate(il = factor(il, levels = rev(top15)))

# ── RENK PALETİ ──────────────────────────────────────────────
neden_renk <- c(
  "Tayin / Is Degisikligi" = "#1A6B9A",
  "Is Bulmak"              = "#2E9EC4",
  "Egitim"                 = "#6BAED6",
  "Evlilik / Aile"         = "#E07B54",
  "Daha Iyi Konut"         = "#C1392B",
  "Hane Bagimli Goc"       = "#E8A97E",
  "Memlekete Donus"        = "#7B9E3E",
  "Saglik / Bakim"         = "#A8C66C",
  "Ev Alinmasi"            = "#9B6B9E",
  "Emeklilik"              = "#C9A0DC",
  "Diger"                  = "#888888",
  "Bilinmeyen"             = "#CCCCCC"
)

# ── GRAFİK ───────────────────────────────────────────────────
p3 <- ggplot(df3, aes(x = sayi, y = il, fill = neden)) +
  geom_col(width = 0.7) +
  # Toplam sayı etiketi çubuğun sağına
  geom_text(data = toplam_df,
            aes(x = toplam, y = il,
                label = paste0(format(round(toplam / 1000), big.mark = "."), "K")),
            inherit.aes = FALSE,
            hjust = -0.15, size = 3.2, fontface = "bold", color = "grey20") +
  scale_fill_manual(values = neden_renk, name = "Goc Nedeni") +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K"),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Il Bazli Alinan Goc Nedenleri — En Fazla Goc Alan 15 Il",
    subtitle = "Iller arasi ic goc · 2024 · Kaynak: TUIK",
    x        = "Kisi Sayisi",
    y        = NULL,
    caption  = "Alinan gocun neden dagilimi il bazinda"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title         = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle      = element_text(size = 9, color = "grey10", hjust = 0,
                                      margin = margin(b = 10)),
    plot.caption       = element_text(size = 8, color = "grey20"),
    axis.text.y        = element_text(size = 12, face = "bold", color = "grey10"),
    axis.text.x        = element_text(size = 9),
    legend.position    = "bottom",
    legend.title       = element_text(size = 9, face = "bold"),
    legend.text        = element_text(size = 8),
    legend.key.size    = unit(0.4, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 15, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

print(p3)

# ── KAYDET ───────────────────────────────────────────────────
ggsave("grafik3_il_neden.png",
       plot = p3, width = 12, height = 9, dpi = 300, bg = "white")
message("grafik3_il_neden.png kaydedildi")