# ============================================================
# Grafik 5: Net Göç Yön Değiştiren İller — Poster İçin Maksimum Okunabilirlik
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(scales)
library(stringr) # Türkçe karakter düzeltmesi için eklendi

# ── VERİ HAZIRLIĞI ──────────────────────────────────────────
# Not: Dosya adının "iller_arasi_goc.xlsx" olduğundan emin olun.
raw <- read_excel("iller_arasi_goc.xlsx", sheet = 1, skip = 1)
colnames(raw) <- c("yil", "goc_alan", "goc_veren",
                   "alan_nufus", "veren_nufus",
                   "aldigi_goc", "verdigi_goc", "net_goc")

net_yil <- raw %>%
  mutate(yil = as.integer(yil)) %>%
  filter(yil >= 2008, yil <= 2024) %>%
  group_by(yil, il = goc_alan) %>%
  summarise(net_goc = sum(net_goc, na.rm = TRUE), .groups = "drop")

ucler <- net_yil %>%
  filter(yil %in% c(2008, 2024)) %>%
  pivot_wider(names_from = yil, values_from = net_goc,
              names_prefix = "y") %>%
  filter(!is.na(y2008), !is.na(y2024))

# Grupların Belirlenmesi
pozdan_neg <- ucler %>%
  filter(y2008 > 0, y2024 < 0) %>%
  mutate(grup = "Göç Alırken Göç Vermeye Başlayanlar") %>%
  arrange(desc(y2008)) %>%
  slice_head(n = 6)

negden_poz <- ucler %>%
  filter(y2008 < 0, y2024 > 0) %>%
  mutate(grup = "Göç Verirken Göç Almaya Başlayanlar") %>%
  arrange(desc(y2024)) %>%
  slice_head(n = 6)

df_slope <- bind_rows(pozdan_neg, negden_poz) %>%
  mutate(
    # tools::toTitleCase yerine Türkçe uyumlu str_to_title kullanıyoruz:
    il   = str_to_title(il, locale = "tr"),
    renk = ifelse(grup == "Göç Alırken Göç Vermeye Başlayanlar", "#C1392B", "#1A6B9A")
  )

df_long <- df_slope %>%
  select(il, grup, renk, y2008, y2024) %>%
  pivot_longer(cols = c(y2008, y2024),
               names_to  = "yil",
               values_to = "net_goc") %>%
  mutate(yil = as.integer(gsub("y", "", yil)))

# ── GRAFİK ───────────────────────────────────────────────────
p5 <- ggplot(df_long, aes(x = yil, y = net_goc, group = il, color = renk)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
  geom_line(linewidth = 2, alpha = 0.8) + 
  geom_point(size = 5) +                  
  
  # Şehir İsimleri ve Rakamlar (Boyut: 6)
  geom_text_repel(
    data             = df_long %>% filter(yil == 2008),
    aes(label        = paste0(il, "  ", round(net_goc / 1000, 1), "K")),
    hjust            = 1, nudge_x = -0.5, size = 6, fontface = "bold", direction = "y"
  ) +
  geom_text_repel(
    data             = df_long %>% filter(yil == 2024),
    aes(label        = paste0(round(net_goc / 1000, 1), "K  ", il)),
    hjust            = 0, nudge_x = 0.5, size = 6, fontface = "bold", direction = "y"
  ) +
  
  facet_wrap(~ grup, ncol = 2) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = c(2008, 2024),
    labels = c("2008", "2024"),
    expand = expansion(mult = c(0.45, 0.45))
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  
  labs(
    title    = "NET GÖÇTE YÖN DEĞİŞTİREN İLLER (2008 - 2024)",
    x        = "YILLAR", 
    y        = "NET GÖÇ (BİN KİŞİ)",
    caption  = "Kaynak: TÜİK Verileri | Kesik çizgi sıfır (denge) noktasını temsil eder."
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    # Başlıklar
    plot.title         = element_text(size = 26, face = "bold", color = "#1e3a8a", margin = margin(b = 20)),
    strip.text         = element_text(size = 20, face = "bold", color = "#1e3a8a"), # Grup başlıkları
    
    # Eksen Yazıları
    axis.title.x       = element_text(size = 18, face = "bold", color = "#1e3a8a", margin = margin(t = 15)),
    axis.title.y       = element_text(size = 18, face = "bold", color = "#1e3a8a"),
    axis.text.x        = element_text(size = 20, face = "bold", color = "#1e3a8a"),
    axis.text.y        = element_text(size = 16, color = "#1e3a8a"),
    
    # Arka Plan ve Izgara
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "#eef2fa", color = NA),
    panel.background   = element_rect(fill = "#eef2fa", color = NA),
    plot.margin        = margin(30, 40, 30, 40)
  )

# ── KAYDET ───────────────────────────────────────────────────
# Poster için 300 DPI ve büyük boyut şart
ggsave("poster_goc_final.png", plot = p5, width = 16, height = 10, dpi = 300)
