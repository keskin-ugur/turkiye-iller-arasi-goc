# ============================================================
# Grafik 2: Türkiye İç Göç Akışları — Sankey Diyagramı (2024)
# Poster İçin Optimize Edilmiş (#1e3a8a Metinler)
# ============================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(showtext)
library(sysfonts)
library(purrr)

# --- Font ---------------------------------------------------------------
font_add_google("Noto Sans", "noto")
showtext_auto()
showtext_opts(dpi = 300)

# --- Veri ---------------------------------------------------------------
duzelt_il <- function(x) {
  x <- tools::toTitleCase(tolower(x))
  x <- str_replace(x, "^Istanbul$", "İstanbul")
  x <- str_replace(x, "^Izmir$",    "İzmir")
  x <- str_replace(x, "^Izmit$",    "İzmit")
  x
}

dosya_yolu <- "iller_arasi_goc.xlsx"

df_raw <- read_excel(dosya_yolu, sheet = "İller Arası Göç", skip = 1)
colnames(df_raw) <- c("Yil","Goc_Alan","Goc_Veren","Alan_Nufus",
                      "Veren_Nufus","Aldigi_Goc","Verdigi_Goc","Net_Goc")

df2024 <- df_raw |> filter(Yil == 2024)

top8 <- df2024 |>
  group_by(Goc_Veren) |>
  summarise(toplam = sum(Verdigi_Goc, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(toplam)) |>
  slice_head(n = 8) |>
  pull(Goc_Veren)

flows <- df2024 |>
  filter(Goc_Veren %in% top8, Goc_Alan %in% top8, Goc_Veren != Goc_Alan) |>
  select(kaynak = Goc_Veren, hedef = Goc_Alan, miktar = Aldigi_Goc) |>
  arrange(desc(miktar)) |>
  slice_head(n = 20) |>
  mutate(
    kaynak = duzelt_il(kaynak),
    hedef  = duzelt_il(hedef)
  )

# --- Renk paleti --------------------------------------------------------
il_renk <- c(
  "İstanbul"  = "#C0392B",
  "Ankara"    = "#2471A3",
  "İzmir"     = "#27AE60",
  "Antalya"   = "#E67E22",
  "Kocaeli"   = "#8E44AD",
  "Bursa"     = "#16A085",
  "Mersin"    = "#D35400",
  "Gaziantep" = "#7F8C8D"
)

# --- Sankey geometrisi: düğüm konumları ---------------------------------
NODE_W  <- 0.032
GAP     <- 0.008
X_LEFT  <- 0.21
X_RIGHT <- 0.79

sol_toplam <- flows |>
  group_by(kaynak) |> summarise(h = sum(miktar), .groups = "drop") |>
  arrange(desc(h))

sag_toplam <- flows |>
  group_by(hedef) |> summarise(h = sum(miktar), .groups = "drop") |>
  arrange(desc(h))

toplam_yukseklik <- sum(sol_toplam$h)

hesapla_dugumler <- function(df_sum, x_pos) {
  df_sum <- arrange(df_sum, h)
  y_bas  <- 0
  result <- list()
  for (i in seq_len(nrow(df_sum))) {
    isim <- df_sum[[1]][i]
    h    <- df_sum$h[i]
    result[[i]] <- tibble(
      il    = isim,
      x     = x_pos,
      y_min = y_bas,
      y_max = y_bas + h,
      y_mid = y_bas + h / 2
    )
    y_bas <- y_bas + h + GAP * toplam_yukseklik
  }
  bind_rows(result)
}

sol_dugumler <- hesapla_dugumler(sol_toplam, X_LEFT)
sag_dugumler <- hesapla_dugumler(sag_toplam, X_RIGHT)

# --- Gaziantep etiketini her iki tarafta aşağı kaydır ------------------
sol_dugumler <- sol_dugumler |>
  mutate(y_mid = ifelse(il == "Gaziantep",
                        y_mid - toplam_yukseklik * 0.02,
                        y_mid))

sag_dugumler <- sag_dugumler |>
  mutate(y_mid = ifelse(il == "Gaziantep",
                        y_mid - toplam_yukseklik * 0.02,
                        y_mid))

# --- Bant geometrisi ----------------------------------------------------
bezier_y <- function(t, y0, y1) {
  (1-t)^3 * y0 +
    3*(1-t)^2*t * y0 +
    3*(1-t)*t^2 * y1 +
    t^3         * y1
}

bant_ciz <- function(x0, y0_bot, y0_top, x1, y1_bot, y1_top, fill, alpha = 0.7) {
  n  <- 150
  t  <- seq(0, 1, length.out = n)
  xs <- seq(x0, x1, length.out = n)
  y_top_vals <- bezier_y(t, y0_top, y1_top)
  y_bot_vals <- bezier_y(t, y0_bot, y1_bot)
  tibble(
    x     = c(xs, rev(xs)),
    y     = c(y_top_vals, rev(y_bot_vals)),
    fill  = fill,
    alpha = alpha
  )
}

sol_y_cursor <- setNames(sol_dugumler$y_min, sol_dugumler$il)
sag_y_cursor <- setNames(sag_dugumler$y_min, sag_dugumler$il)

bant_data <- list()
for (i in seq_len(nrow(flows))) {
  k  <- flows$kaynak[i]
  h  <- flows$hedef[i]
  m  <- flows$miktar[i]
  
  x0     <- X_LEFT  + NODE_W
  x1     <- X_RIGHT
  y0_bot <- sol_y_cursor[k]
  y0_top <- y0_bot + m
  y1_bot <- sag_y_cursor[h]
  y1_top <- y1_bot + m
  
  sol_y_cursor[k] <- y0_top
  sag_y_cursor[h] <- y1_top
  
  bant_data[[i]] <- bant_ciz(x0, y0_bot, y0_top, x1, y1_bot, y1_top,
                             fill = il_renk[k])
}

bant_df <- bind_rows(bant_data, .id = "id") |>
  mutate(id = as.integer(id))

# --- Düğüm dikdörtgenleri -----------------------------------------------
dugum_df <- bind_rows(
  sol_dugumler |> mutate(taraf = "kaynak", x_min = X_LEFT,           x_max = X_LEFT  + NODE_W),
  sag_dugumler |> mutate(taraf = "hedef",  x_min = X_RIGHT - NODE_W, x_max = X_RIGHT)
)

# --- Grafik -------------------------------------------------------------
p <- ggplot() +
  
  geom_polygon(
    data = bant_df,
    aes(x = x, y = y, group = id, fill = fill),
    alpha = 0.65, color = NA
  ) +
  scale_fill_identity() +
  
  geom_rect(
    data = dugum_df,
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = il_renk[il]),
    color = "white", linewidth = 0.3
  ) +
  
  # Sol Şehir İsimleri
  geom_text(
    data = sol_dugumler,
    aes(x = X_LEFT - 0.012, y = y_mid, label = il),
    hjust = 1, size = 8, fontface = "bold",
    family = "noto", color = "#1e3a8a"
  ) +
  
  # Sağ Şehir İsimleri
  geom_text(
    data = sag_dugumler,
    aes(x = X_RIGHT + 0.012, y = y_mid, label = il),
    hjust = 0, size = 8, fontface = "bold",
    family = "noto", color = "#1e3a8a"
  ) +
  
  # Üst Başlıklar (Göç Veren / Alan)
  annotate("text", x = X_LEFT  + NODE_W/2, y = max(sol_dugumler$y_max) * 1.06,
           label = "Göç Veren İl", hjust = 0.5, size = 9,
           fontface = "bold", family = "noto", color = "#1e3a8a") +
  annotate("text", x = X_RIGHT - NODE_W/2, y = max(sag_dugumler$y_max) * 1.06,
           label = "Göç Alan İl",  hjust = 0.5, size = 9,
           fontface = "bold", family = "noto", color = "#1e3a8a") +
  
  labs(
    title    = "Türkiye İç Göç Akışları · 2024",
    subtitle = "En yüksek göç hacmine sahip 8 il arasındaki 20 büyük akış",
    caption  = "Kaynak: TÜİK İller Arası Göç İstatistikleri, 2024  |  Bant kalınlığı göç eden kişi sayısıyla orantılıdır.",
    x = NULL, y = NULL
  ) +
  
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.05, 0)) +
  
  theme_void(base_family = "noto") +
  theme(
    plot.title       = element_text(size = 36, face = "bold", color = "#1e3a8a",
                                    margin = margin(b = 10), family = "noto"),
    plot.subtitle    = element_text(size = 22, color = "#1e3a8a",
                                    margin = margin(b = 25), family = "noto"),
    plot.caption     = element_text(size = 16, color = "#1e3a8a", hjust = 0,
                                    margin = margin(t = 20), family = "noto"),
    plot.margin      = margin(40, 40, 30, 40),
    plot.background  = element_rect(fill = "#eef2fa", color = NA),
    panel.background = element_rect(fill = "#eef2fa", color = NA),
    legend.position  = "none"
  )

print(p)

# --- Kaydet -------------------------------------------------------------
# Metinler büyüdüğü için boyutları biraz daha poster dostu (16x10) yaptık
ggsave(
  filename = "ic_goc_sankey_poster_2024_final.png",
  plot     = p,
  width    = 16,
  height   = 10,
  units    = "in",
  dpi      = 300, bg = "#eef2fa"
)
cat("✓ PNG kaydedildi: ic_goc_sankey_poster_2024_final.png  (300 dpi, 16×10 in)\n")
