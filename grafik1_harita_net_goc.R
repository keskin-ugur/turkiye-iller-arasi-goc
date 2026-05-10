# ============================================================
# Grafik 1: Türkiye Net Göç Hızı Choropleth Haritası (2024)
# ============================================================

library(sf)
library(ggplot2)
library(dplyr)
library(readxl)
library(scales)
library(stringi)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

# ── 1. VERİ ──────────────────────────────────────────────────
raw <- read_excel("illerin_aldigi_goc_verdigi_goc_net_goc_ve_net_goc_hizi.xls",
                  col_names = FALSE, skip = 3)

goc <- raw %>%
  select(il = 1, nufus = 2, aldigi_goc = 3,
         verdigi_goc = 4, net_goc = 5, net_goc_hizi = 6) %>%
  filter(!is.na(il), il != "Toplam-Total",
         !grepl("TÜİK|TurkStat|sıfır|zero", il, ignore.case = TRUE)) %>%
  mutate(
    il           = trimws(il),
    net_goc_hizi = as.numeric(net_goc_hizi),
    net_goc      = as.numeric(net_goc),
    nufus        = as.numeric(nufus)
  ) %>%
  filter(!is.na(net_goc_hizi))

# ── 2. İL ADI KISALTMALARI ───────────────────────────────────
kisalt <- function(x) {
  case_when(
    x == "Afyonkarahisar"   ~ "Afyon",
    x == "Kahramanmaraş"    ~ "K.Maraş",
    x == "Karabük"          ~ "Karabük",
    x == "Kırıkkale"        ~ "Kırıkkale",
    x == "Kırklareli"       ~ "Kırklareli",
    x == "Kırşehir"         ~ "Kırşehir",
    x == "Nevşehir"         ~ "Nevşehir",
    x == "Osmaniye"         ~ "Osmaniye",
    x == "Şırnak"           ~ "Şırnak",
    TRUE ~ x
  )
}

# ── 3. TÜRKİYE İL SINIRLARI ──────────────────────────────────
turkey_sf <- ne_states(country = "Turkey", returnclass = "sf")
il_col    <- "name"

# ── 4. NORMALIZE ─────────────────────────────────────────────
normalize <- function(x) {
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x
}

turkey_sf$il_norm <- normalize(turkey_sf[[il_col]])
goc$il_norm       <- normalize(goc$il)

turkey_sf <- turkey_sf %>%
  mutate(il_norm = recode(il_norm,
                          "afyon"     = "afyonkarahisar",
                          "k. maras"  = "kahramanmaras",
                          "kinkkale"  = "kirikkale",
                          "zinguldak" = "zonguldak"
  ))

# ── 5. BİRLEŞTİRME ───────────────────────────────────────────
harita <- turkey_sf %>%
  left_join(goc, by = "il_norm")

# ── 6. CENTROID KOORDİNATLARI ────────────────────────────────
harita_label <- harita %>%
  mutate(
    cx      = st_coordinates(st_centroid(geometry))[, 1],
    cy      = st_coordinates(st_centroid(geometry))[, 2],
    il_kisa = kisalt(il)   # kısaltılmış il adı
  ) %>%
  filter(!is.na(net_goc_hizi))

# ── 7. RENK SKALASI ──────────────────────────────────────────
limit_val <- max(abs(harita$net_goc_hizi), na.rm = TRUE)

# ── 8. HARİTA (YENİ RENKLER VE BOYUTLARLA) ───────────────────
p <- ggplot(harita) +
  geom_sf(aes(fill = net_goc_hizi), color = "white", linewidth = 0.25) +
  
  # Tüm il isimleri (#1e3a8a renginde ve daha belirgin)
  geom_text(
    data     = harita_label,
    aes(x = cx, y = cy, label = il_kisa),
    size     = 3.5,            
    color    = "#1e3a8a",      
    fontface = "bold",         
    family   = "sans"
  ) +
  scale_fill_gradient2(
    low      = "#C1392B",
    mid      = "#F5F0E8",
    high     = "#1A6B9A",
    midpoint = 0,
    limits   = c(-limit_val, limit_val),
    breaks   = c(-40, -20, -10, 0, 10, 20),
    labels   = c("-40‰", "-20‰", "-10‰", "0", "+10‰", "+20‰"),
    name     = "Net göç hızı (‰)",
    guide    = guide_colorbar(
      barwidth       = 20,
      barheight      = 0.8,
      ticks          = TRUE,
      title.position = "top",
      title.hjust    = 0.5,
      direction      = "horizontal"
    )
  ) +
  labs(
    title    = "Türkiye İlleri Net Göç Hızı (2024)",
    subtitle = "İller arası iç göç",
    caption  = "Kaynak: TÜİK | Net göç hızı = (Aldığı göç − Verdiği göç) / İl nüfusu × 1000"
  ) +
  theme_void(base_family = "sans") +
  theme(
    # Tema metinleri de posterdeki gibi #1e3a8a yapıldı
    plot.title       = element_text(size = 24, face = "bold", color = "#1e3a8a", hjust = 0.5, margin = margin(b = 8)),
    plot.subtitle    = element_text(size = 16, color = "#1e3a8a", hjust = 0.5, margin = margin(b = 15)),
    plot.caption     = element_text(size = 12, color = "#1e3a8a", hjust = 0.5, margin = margin(t = 15)),
    
    legend.position  = "bottom",
    legend.margin    = margin(t = 10),
    legend.title     = element_text(size = 14, face = "bold", color = "#1e3a8a"),
    legend.text      = element_text(size = 12, color = "#1e3a8a", face = "bold"),
    
    plot.margin      = margin(20, 20, 20, 20),
    plot.background  = element_rect(fill = "#eef2fa", color = NA),
    panel.background = element_rect(fill = "#eef2fa", color = NA)
  )

print(p)

# ── 9. KAYDET ────────────────────────────────────────────────
ggsave("grafik1_net_goc_haritasi_poster.png",
       plot   = p,
       width  = 16,
       height = 9,
       dpi    = 300,
       bg     = "#eef2fa")

message("✓ Grafik kaydedildi: grafik1_net_goc_haritasi_poster.png")
