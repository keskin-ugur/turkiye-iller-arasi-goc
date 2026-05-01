library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(showtext)
library(sysfonts)
library(purrr)

# --- 2. Font ---------------------------------------------------------------
font_add_google("Noto Sans", "noto")
showtext_auto()
showtext_opts(dpi = 300)

# --- 3. Veri ---------------------------------------------------------------
dosya_yolu <- "iller_arasi_goc.xlsx"

df_raw <- read_excel(dosya_yolu, sheet = "İller Arası Göç", skip = 1)
colnames(df_raw) <- c("Yil","Goc_Alan","Goc_Veren","Alan_Nufus",
                      "Veren_Nufus","Aldigi_Goc","Verdigi_Goc","Net_Goc")

df2025 <- df_raw |> filter(Yil == 2024)

top8 <- df2025 |>
  group_by(Goc_Veren) |>
  summarise(toplam = sum(Verdigi_Goc, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(toplam)) |>
  slice_head(n = 8) |>
  pull(Goc_Veren)

flows <- df2025 |>
  filter(Goc_Veren %in% top8, Goc_Alan %in% top8, Goc_Veren != Goc_Alan) |>
  select(kaynak = Goc_Veren, hedef = Goc_Alan, miktar = Aldigi_Goc) |>
  arrange(desc(miktar)) |>
  slice_head(n = 20)

# --- 4. Renk paleti --------------------------------------------------------
il_renk <- c(
  "İSTANBUL"   = "#C0392B",
  "ANKARA"     = "#2471A3",
  "İZMİR"      = "#27AE60",
  "ANTALYA"    = "#E67E22",
  "KOCAELİ"    = "#8E44AD",
  "BURSA"      = "#16A085",
  "MERSİN"     = "#D35400",
  "GAZİANTEP"  = "#7F8C8D"
)

# --- 5. Sankey geometrisi: düğüm konumları ---------------------------------
# Canvas: x ∈ [0,1], y toplam göç miktarına göre
NODE_W   <- 0.035  # düğüm genişliği
GAP      <- 0.008  # düğümler arası boşluk
X_LEFT   <- 0.22   # sol düğüm sütunu
X_RIGHT  <- 0.78   # sağ düğüm sütunu

# Toplam yükseklik hesabı
sol_toplam <- flows |>
  group_by(kaynak) |> summarise(h = sum(miktar), .groups = "drop") |>
  arrange(desc(h))

sag_toplam <- flows |>
  group_by(hedef) |> summarise(h = sum(miktar), .groups = "drop") |>
  arrange(desc(h))

toplam_yukseklik <- sum(sol_toplam$h)
bosluk_sayisi    <- nrow(sol_toplam) - 1
bosluk_toplam    <- bosluk_sayisi * GAP * toplam_yukseklik

# Düğüm y pozisyonlarını hesapla (aşağıdan yukarı)
hesapla_dugumler <- function(df_sum, x_pos) {
  df_sum <- arrange(df_sum, h)  # küçükten büyüğe (aşağıdan yukarı)
  y_bas <- 0
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

# --- 6. Bant geometrisi ----------------------------------------------------
# Her akış için cubic Bezier eğrisi oluştur
# x: lineer (soldan sağa düz gider)
# y: cubic Bezier — başta ve sonda yatay, ortada geçiş yumuşak
bezier_y <- function(t, y0, y1) {
  # Kontrol noktaları: başlangıç ve bitişte yatay tutmak için
  # P0=y0, P1=y0, P2=y1, P3=y1  →  klasik S-eğrisi ama x bağımsız
  (1-t)^3 * y0 +
    3*(1-t)^2*t * y0 +
    3*(1-t)*t^2 * y1 +
    t^3        * y1
}

bant_ciz <- function(x0, y0_bot, y0_top, x1, y1_bot, y1_top, fill, alpha = 0.7) {
  n  <- 150
  t  <- seq(0, 1, length.out = n)
  
  # x: lineer — eğri sola/sağa yayılsın, dikey sıkışma olmasın
  xs <- seq(x0, x1, length.out = n)
  
  # y: smooth Bezier
  y_top_vals <- bezier_y(t, y0_top, y1_top)
  y_bot_vals <- bezier_y(t, y0_bot, y1_bot)
  
  tibble(
    x     = c(xs, rev(xs)),
    y     = c(y_top_vals, rev(y_bot_vals)),
    fill  = fill,
    alpha = alpha
  )
}

# Her kaynak düğümdeki mevcut y pozisyonunu takip et
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

# --- 7. Düğüm dikdörtgenleri -----------------------------------------------
dugum_df <- bind_rows(
  sol_dugumler |> mutate(taraf = "kaynak", x_min = X_LEFT,          x_max = X_LEFT  + NODE_W),
  sag_dugumler |> mutate(taraf = "hedef",  x_min = X_RIGHT - NODE_W, x_max = X_RIGHT)
)

# --- 8. Grafik -------------------------------------------------------------
p <- ggplot() +
  
  # Bantlar
  geom_polygon(
    data = bant_df,
    aes(x = x, y = y, group = id, fill = fill),
    alpha = 0.65, color = NA
  ) +
  scale_fill_identity() +
  
  # Düğüm dikdörtgenleri
  geom_rect(
    data = dugum_df,
    aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = il_renk[il]),
    color = "white", linewidth = 0.3
  ) +
  
  # Sol etiketler — düğümlerin tamamen solunda
  geom_text(
    data = sol_dugumler,
    aes(x = X_LEFT - 0.012, y = y_mid, label = il),
    hjust = 1, size = 5.5, fontface = "bold",
    family = "noto", color = "grey15"
  ) +
  
  # Sağ etiketler — düğümlerin tamamen sağında
  geom_text(
    data = sag_dugumler,
    aes(x = X_RIGHT + 0.012, y = y_mid, label = il),
    hjust = 0, size = 5.5, fontface = "bold",
    family = "noto", color = "grey15"
  ) +
  
  # Eksen etiketleri (sütun başlıkları)
  annotate("text", x = X_LEFT  + NODE_W/2, y = max(sol_dugumler$y_max) * 1.06,
           label = "Göç Veren İl", hjust = 0.5, size = 5.5,
           fontface = "bold", family = "noto", color = "grey30") +
  annotate("text", x = X_RIGHT - NODE_W/2, y = max(sag_dugumler$y_max) * 1.06,
           label = "Göç Alan İl",  hjust = 0.5, size = 5.5,
           fontface = "bold", family = "noto", color = "grey30") +
  
  # Başlık ve notlar
  labs(
    title    = "Türkiye İç Göç Akışları · 2024",
    subtitle = "En yüksek göç hacmine sahip 8 il arasındaki 20 büyük akış",
    caption  = "Kaynak: TÜİK İller Arası Göç İstatistikleri, 2024  |  Bant kalınlığı göç eden kişi sayısıyla orantılıdır.",
    x = NULL, y = NULL
  ) +
  
  # Eksenleri gizle, temiz görünüm
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0.03, 0)) +
  
  theme_void(base_family = "noto") +
  theme(
    plot.title       = element_text(size = 30, face = "bold", color = "grey10",
                                    margin = margin(b = 6), family = "noto"),
    plot.subtitle    = element_text(size = 16, color = "grey45",
                                    margin = margin(b = 20), family = "noto"),
    plot.caption     = element_text(size = 11, color = "grey60", hjust = 0,
                                    margin = margin(t = 16), family = "noto"),
    plot.margin      = margin(28, 20, 20, 20),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "none"
  )

print(p)

# --- 9. Kaydet -------------------------------------------------------------
ggsave(
  filename = "ic_goc_sankey_poster_2024.png",
  plot     = p,
  width    = 42, height = 26, units = "cm",
  dpi      = 300, bg = "white"
)
cat("✓ PNG kaydedildi: ic_goc_sankey_poster_2024.png  (300 dpi, 42×26 cm)\n")