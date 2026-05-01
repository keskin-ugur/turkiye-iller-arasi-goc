# ============================================================
# Grafik 1: Türkiye Net Göç Hızı Choropleth Haritası (2024)
# ============================================================
# install.packages(c("sf", "ggplot2", "dplyr", "readxl", "scales",
#                    "rnaturalearth", "rnaturalearthdata", "stringi", "ggrepel"))

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

manual_fix <- c(
  "afyon"     ~ "afyonkarahisar",
  "k. maras"  ~ "kahramanmaras",
  "kinkkale"  ~ "kirikkale",
  "zinguldak" ~ "zonguldak"
)

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

eslesmeyen <- harita %>% filter(is.na(net_goc_hizi)) %>% pull(il_col)
if (length(eslesmeyen) > 0) {
  message("Eşleşmeyen iller: ", paste(eslesmeyen, collapse = ", "))
}

# ── 6. CENTROID KOORDİNATLARI ────────────────────────────────
harita_label <- harita %>%
  mutate(
    cx      = st_coordinates(st_centroid(geometry))[, 1],
    cy      = st_coordinates(st_centroid(geometry))[, 2],
    il_kisa = kisalt(il)   # kısaltılmış il adı
  ) %>%
  filter(!is.na(net_goc_hizi))

# ── 7. RENK SKALI ────────────────────────────────────────────
limit_val <- max(abs(harita$net_goc_hizi), na.rm = TRUE)

# ── 8. HARİTA ────────────────────────────────────────────────
p <- ggplot(harita) +
  geom_sf(aes(fill = net_goc_hizi), color = "white", linewidth = 0.25) +
  # Tüm il isimleri
  geom_text(
    data     = harita_label,
    aes(x = cx, y = cy, label = il_kisa),
    size     = 2.8,
    color    = "grey20",
    fontface = "plain",
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
      barwidth       = 18,
      barheight      = 0.6,
      ticks          = TRUE,
      title.position = "top",
      title.hjust    = 0.5,
      direction      = "horizontal"
    )
  ) +
  labs(
    title    = "Türkiye İlleri Net Göç Hızı",
    subtitle = "İller arası iç göç · 2024 · Kaynak: TÜİK",
    caption  = "Net göç hızı = (Aldığı göç − Verdiği göç) / İl nüfusu × 1000"
  ) +
  theme_void(base_family = "sans") +
  theme(
    plot.title       = element_text(size = 18, face = "bold",   hjust = 0.5,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 11, color = "grey40", hjust = 0.5,
                                    margin = margin(b = 12)),
    plot.caption     = element_text(size = 8,  color = "grey55", hjust = 0.5,
                                    margin = margin(t = 10)),
    legend.position  = "bottom",
    legend.margin    = margin(t = 6),
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    plot.margin      = margin(10, 20, 10, 10),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p)

# ── 9. KAYDET ────────────────────────────────────────────────
ggsave("grafik1_net_goc_haritasi.png",
       plot   = p,
       width  = 14,
       height = 7,
       dpi    = 300,
       bg     = "white")

message("✓ Grafik kaydedildi: grafik2_net_goc_haritasi.png")