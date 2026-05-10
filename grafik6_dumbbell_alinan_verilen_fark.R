# ============================================================
# Grafik 6: İl Bazında Alınan ve Verilen Göç — Poster Nihai
# ============================================================

library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(ggtext)
library(stringr)

# ── 1. VERİ HAZIRLIĞI ────────────────────────────────────────
raw <- read_excel("iller_arasi_goc.xlsx", sheet = 1, skip = 1)
colnames(raw) <- c("yil", "goc_alan", "goc_veren",
                   "alan_nufus", "veren_nufus",
                   "aldigi_goc", "verdigi_goc", "net_goc")

df24_full <- raw %>%
  mutate(yil = as.integer(yil)) %>%
  filter(yil == 2024) %>%
  group_by(il = goc_alan) %>%
  summarise(
    aldigi  = sum(aldigi_goc,  na.rm = TRUE),
    verdigi = sum(verdigi_goc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    net = aldigi - verdigi,
    il  = str_to_title(il, locale = "tr") # İstanbul, Şanlıurfa vb. için Türkçe destek
  )

top_alan <- df24_full %>% filter(net > 0) %>% arrange(desc(net)) %>% slice_head(n = 10)
top_veren <- df24_full %>% filter(net < 0) %>% arrange(net) %>% slice_head(n = 10)

df24 <- bind_rows(top_alan, top_veren) %>%
  mutate(il = factor(il, levels = il[order(net)]))

# ── 2. GRAFİK TASARIMI ───────────────────────────────────────
p6 <- ggplot(df24) +
  # Referans çizgisi
  geom_vline(xintercept = 0, color = "#1e3a8a", 
             linewidth = 1, linetype = "dashed", alpha = 0.3) +
  
  # Dumbbell bağlantısı
  geom_segment(
    aes(x = aldigi, xend = verdigi, y = il, yend = il),
    color = "grey60", linewidth = 2.5
  ) +
  
  # Alınan ve Verilen Göç Noktaları
  geom_point(aes(x = aldigi,  y = il), color = "#1A6B9A", size = 7.5) +
  geom_point(aes(x = verdigi, y = il), color = "#C1392B", size = 7.5) +
  
  # Net Göç Rakamları
  geom_text(
    aes(
      x     = pmax(aldigi, verdigi),
      y     = il,
      label = ifelse(net >= 0,
                     paste0("+", round(net / 1000, 1), "K"),
                     paste0(round(net / 1000, 1), "K"))
    ),
    hjust    = -0.35,
    size     = 7.5, 
    color    = "#1e3a8a", 
    fontface = "bold"
  ) +
  
  scale_x_continuous(
    labels = label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0.05, 0.22)) # Sağdaki boşluk dengelendi
  ) +
  
  labs(
    title    = "ALINAN VE VERİLEN GÖÇ FARKI (2024)",
    subtitle = "<span style='color:#1A6B9A;'>&#9679; Alınan Göç</span>&nbsp;&nbsp;&nbsp;<span style='color:#C1392B;'>&#9679; Verilen Göç</span>",
    x        = "Göç miktarı (kişi)",
    y        = NULL,
    caption  = "Kaynak: TÜİK · 2024 yılı"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title         = element_text(size = 30, face = "bold", color = "#1e3a8a"),
    plot.subtitle      = element_markdown(size = 20, color = "#1e3a8a", margin = margin(b = 20)),
    plot.caption       = element_text(size = 12, color = "#1e3a8a"),
    
    # İl isimlerini ve eksenleri makul bir poster boyutuna çektik
    axis.text.y        = element_text(size = 21, face = "bold", color = "#1e3a8a"),
    axis.text.x        = element_text(size = 17, face = "bold", color = "#1e3a8a"),
    axis.title.x       = element_text(size = 18, face = "bold", color = "#1e3a8a", margin = margin(t = 15)),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "white", linewidth = 0.6),
    plot.background    = element_rect(fill = "#eef2fa", color = NA),
    panel.background   = element_rect(fill = "#eef2fa", color = NA),
    plot.margin        = margin(30, 50, 30, 40)
  )

# ── 3. KAYDET ────────────────────────────────────────────────
# 18x11 inç: Hem yataylığı hissettirir hem de posterde çok sırıtmaz.
ggsave("grafik6_dumbbell_poster_final.png",
       plot = p6, width = 18, height = 11, dpi = 300, bg = "#eef2fa")
