# ============================================================
# Grafik 6: İl Bazında Alınan ve Verilen Göç — Dumbbell Chart
#           En fazla göç alan 10 + en fazla göç veren 10 il
# ============================================================
# install.packages(c("ggplot2", "dplyr", "readxl", "scales", "ggtext"))

library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(ggtext)

# ── VERİ ─────────────────────────────────────────────────────
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
    il  = tools::toTitleCase(tolower(il))
  )

# ── 10 ALAN + 10 VEREN ───────────────────────────────────────
top_alan <- df24_full %>%
  filter(net > 0) %>%
  arrange(desc(net)) %>%
  slice_head(n = 10)

top_veren <- df24_full %>%
  filter(net < 0) %>%
  arrange(net) %>%
  slice_head(n = 10)

df24 <- bind_rows(top_alan, top_veren) %>%
  mutate(il = factor(il, levels = il[order(net)]))

# ── GRAFİK ───────────────────────────────────────────────────
p6 <- ggplot(df24) +
  geom_vline(xintercept = 0, color = "grey40",
             linewidth = 0.4, linetype = "dashed") +
  geom_segment(
    aes(x = aldigi, xend = verdigi, y = il, yend = il),
    color = "grey70", linewidth = 1
  ) +
  geom_point(aes(x = aldigi,  y = il), color = "#1A6B9A", size = 3.5) +
  geom_point(aes(x = verdigi, y = il), color = "#C1392B", size = 3.5) +
  geom_text(
    aes(
      x     = pmax(aldigi, verdigi),
      y     = il,
      label = ifelse(net >= 0,
                     paste0("+", round(net / 1000, 1), "K"),
                     paste0(round(net / 1000, 1), "K"))
    ),
    hjust    = -0.25,
    size     = 3.2,
    color    = ifelse(df24$net >= 0, "#1A6B9A", "#C1392B"),
    fontface = "bold"
  ) +
  scale_x_continuous(
    labels = label_number(scale = 1e-3, suffix = "K"),
    expand = expansion(mult = c(0.03, 0.12))
  ) +
  labs(
    title    = "Alınan ve Verilen Göç Farkı — En Fazla Göç Alan ve Veren 10'ar İl (2024)",
    subtitle = "<span style='color:#1A6B9A;font-size:13pt'>&#9679; Alınan Göç</span>&nbsp;&nbsp;&nbsp;<span style='color:#C1392B;font-size:13pt'>&#9679; Verilen Göç</span>",
    x        = "Göç Miktarı (Kişi)",
    y        = NULL,
    caption  = "Kaynak: TÜİK · 2024 yılı iller arası iç göç"
  ) +
  theme_minimal(base_family = "sans", base_size = 11) +
  theme(
    plot.title         = element_text(size = 15, face = "bold", hjust = 0),
    plot.subtitle      = element_markdown(size = 12, hjust = 0,
                                          margin = margin(b = 10)),
    plot.caption       = element_text(size = 8,  color = "grey55"),
    axis.text.y        = element_text(size = 13, face = "bold", color = "grey10"),
    axis.text.x        = element_text(size = 10, color = "grey20"),
    axis.title.x       = element_text(size = 11, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 15, 10, 10)
  )

print(p6)

# ── KAYDET ───────────────────────────────────────────────────
ggsave("grafik6_dumbbell.png",
       plot = p6, width = 10, height = 9, dpi = 300, bg = "white")
message("grafik6_dumbbell.png kaydedildi")
