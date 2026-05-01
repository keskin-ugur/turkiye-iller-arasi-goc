# ============================================================
# Grafik 5: Net Göç Yön Değiştiren İller — Slope Chart
# ============================================================
# install.packages(c("ggplot2", "dplyr", "tidyr", "readxl", "ggrepel"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(ggrepel)
library(scales)

# ── VERİ ─────────────────────────────────────────────────────
raw <- read_excel("iller_arasi_goc.xlsx", sheet = 1, skip = 1)
colnames(raw) <- c("yil", "goc_alan", "goc_veren",
                   "alan_nufus", "veren_nufus",
                   "aldigi_goc", "verdigi_goc", "net_goc")

net_yil <- raw %>%
  mutate(yil = as.integer(yil)) %>%
  filter(yil >= 2008, yil <= 2024) %>%
  group_by(yil, il = goc_alan) %>%
  summarise(net_goc = sum(net_goc, na.rm = TRUE), .groups = "drop")

# ── YÖN DEĞİŞTİREN İLLER ─────────────────────────────────────
ucler <- net_yil %>%
  filter(yil %in% c(2008, 2024)) %>%
  pivot_wider(names_from = yil, values_from = net_goc,
              names_prefix = "y") %>%
  filter(!is.na(y2008), !is.na(y2024))

pozdan_neg <- ucler %>%
  filter(y2008 > 0, y2024 < 0) %>%
  mutate(grup = "Göç Alırken Göç Vermeye Başladı") %>%
  arrange(desc(y2008)) %>%
  slice_head(n = 6)

negden_poz <- ucler %>%
  filter(y2008 < 0, y2024 > 0) %>%
  mutate(grup = "Göç Verirken Göç Almaya Başladı") %>%
  arrange(desc(y2024)) %>%
  slice_head(n = 6)

df_slope <- bind_rows(pozdan_neg, negden_poz) %>%
  mutate(
    il   = tools::toTitleCase(tolower(il)),
    renk = ifelse(grup == "Göç Alırken Göç Vermeye Başladı", "#C1392B", "#1A6B9A")
  )

# Long format
df_long <- df_slope %>%
  select(il, grup, renk, y2008, y2024) %>%
  pivot_longer(cols = c(y2008, y2024),
               names_to  = "yil",
               values_to = "net_goc") %>%
  mutate(yil = as.integer(gsub("y", "", yil)))

# ── GRAFİK ───────────────────────────────────────────────────
p5 <- ggplot(df_long, aes(x = yil, y = net_goc, group = il, color = renk)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey50", linewidth = 0.6) +
  geom_line(linewidth = 1.2, alpha = 0.85) +
  geom_point(size = 3.5) +
  # Sol etiketler (2008) — box.padding artırıldı, üst üste binmeyi azaltır
  geom_text_repel(
    data             = df_long %>% filter(yil == 2008),
    aes(label        = paste0(il, "  ", round(net_goc / 1000, 1), "K")),
    hjust            = 1,
    nudge_x          = -0.4,
    size             = 3.8,
    fontface         = "bold",
    segment.size     = 0.3,
    direction        = "y",
    box.padding      = 0.3,    
    force            = 0.2,        
    min.segment.length = 0.1
  ) +
  # Sağ etiketler (2024)
  geom_text_repel(
    data             = df_long %>% filter(yil == 2024),
    aes(label        = paste0(round(net_goc / 1000, 1), "K  ", il)),
    hjust            = 0,
    nudge_x          = 0.4,
    size             = 3.8,
    fontface         = "bold",
    segment.size     = 0.3,
    direction        = "y",
    box.padding      = 0.6,
    force            = 0,
    min.segment.length = 0.1
  ) +
  facet_wrap(~ grup, ncol = 2) +
  scale_color_identity() +
  scale_x_continuous(
    breaks = c(2008, 2024),
    labels = c("2008", "2024"),
    expand = expansion(mult = c(0.35, 0.35))  # yanlar genişletildi
  ) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = "K")) +
  labs(
    title    = "Net Göçte Yön Değiştiren İller (2008 - 2024)",
    subtitle = "Kesik çizgi sıfır sınırını gösteriyor · Kırmızı: göç almaktan vermeye · Mavi: göç vermekten almaya",
    x        = NULL,
    y        = "Net Göç (Kişi)",
    caption  = "Kaynak: TÜİK · Net Göç = Alınan Göç - Verilen Göç"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title         = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle      = element_text(size = 9, color = "grey40", hjust = 0,
                                      margin = margin(b = 10)),
    plot.caption       = element_text(size = 8, color = "grey55"),
    strip.text         = element_text(size = 13, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 13, face = "bold"),
    axis.text.y        = element_text(size = 11, color = "grey20"),
    axis.title.y       = element_text(size = 11, face = "bold"),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 25, 10, 25)
  )

print(p5)

# ── KAYDET ───────────────────────────────────────────────────
ggsave("grafik5_slope.png",
       plot = p5, width = 14, height = 9, dpi = 300, bg = "white")
message("grafik5_slope.png kaydedildi")