# ============================================================
# Grafik 3: İl Bazlı Göç Nedeni — Sadece Alınan Göç (Top 15 il)
# ============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)
library(scales)
library(stringr)

# ── NEDEN ETİKETLERİ (Türkçe Karakterli) ─────────────────────
neden_labels <- c(
  "tayin"    = "Tayin / İş Değişikliği",
  "is"       = "İş Bulmak",
  "egitim"   = "Eğitim",
  "medeni"   = "Evlilik / Aile",
  "konut"    = "Daha İyi Konut",
  "hane"     = "Hane Bağımlı Göç",
  "donus"    = "Memlekete Dönüş",
  "saglik"   = "Sağlık / Bakım",
  "ev"       = "Ev Alınması",
  "emekli"   = "Emeklilik",
  "diger"    = "Diğer",
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
    il = str_to_title(trimws(il), locale = "tr"), # İstanbul, İzmir vb. için Türkçe düzeltme
    across(-il, ~as.numeric(.) %>% replace_na(0)) # Hata önlemek için boşlukları sıfır yap
  ) %>%
  filter(toplam > 0)

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

toplam_df <- alinan %>%
  filter(il %in% top15) %>%
  mutate(il = factor(il, levels = rev(top15)))

# ── RENK PALETİ (Türkçe Etiketlerle Eşleşti) ─────────────────
neden_renk <- c(
  "Tayin / İş Değişikliği" = "#1A6B9A",
  "İş Bulmak"              = "#2E9EC4",
  "Eğitim"                 = "#6BAED6",
  "Evlilik / Aile"         = "#E07B54",
  "Daha İyi Konut"         = "#C1392B",
  "Hane Bağımlı Göç"       = "#E8A97E",
  "Memlekete Dönüş"        = "#7B9E3E",
  "Sağlık / Bakım"         = "#A8C66C",
  "Ev Alınması"            = "#9B6B9E",
  "Emeklilik"              = "#C9A0DC",
  "Diğer"                  = "#888888",
  "Bilinmeyen"             = "#CCCCCC"
)

# ── GRAFİK ───────────────────────────────────────────────────
p3 <- ggplot(df3, aes(x = sayi, y = il, fill = neden)) +
  geom_col(width = 0.8) +
  geom_text(data = toplam_df,
            aes(x = toplam, y = il,
                label = paste0(format(round(toplam / 1000), big.mark = "."), "K")),
            inherit.aes = FALSE,
            hjust = -0.15, size = 8, fontface = "bold", color = "#1e3a8a") +
  scale_fill_manual(values = neden_renk, name = "Göç Nedenleri") +
  scale_x_continuous(labels = label_number(scale = 1e-3, suffix = "K"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "İL BAZLI ALINAN GÖÇ NEDENLERİ (2024)",
    subtitle = "En Fazla Göç Alan 15 İl - Neden Dağılımı",
    x        = "Göç Miktarı (Kişi)",
    y        = NULL,
    caption  = "Kaynak: TÜİK"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    # Tüm metin renkleri #1e3a8a yapıldı
    text               = element_text(color = "#1e3a8a"),
    plot.title         = element_text(size = 36, face = "bold", hjust = 0, color = "#1e3a8a"),
    plot.subtitle      = element_text(size = 22, color = "#1e3a8a", hjust = 0, margin = margin(b = 20)),
    plot.caption       = element_text(size = 16, color = "#1e3a8a"),
    
    # İl isimleri ve rakamlar dev boyuta çıkarıldı
    axis.text.y        = element_text(size = 26, face = "bold", color = "#1e3a8a"),
    axis.text.x        = element_text(size = 18, face = "bold", color = "#1e3a8a"),
    axis.title.x       = element_text(size = 20, color = "#1e3a8a", face = "bold", margin = margin(t = 15)),
    
    legend.position    = "bottom",
    # Gösterge başlığı ve metni büyütüldü
    legend.title       = element_text(size = 22, face = "bold", color = "#1e3a8a"),
    legend.text        = element_text(size = 18, color = "#1e3a8a"),
    
    legend.key.size    = unit(1.2, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "#eef2fa", color = NA),
    panel.background   = element_rect(fill = "#eef2fa", color = NA),
    plot.margin        = margin(30, 40, 30, 30)
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

print(p3)

# ── KAYDET ───────────────────────────────────────────────────
ggsave("grafik3_il_neden_dev_poster.png",
       plot = p3, width = 20, height = 14, dpi = 300, bg = "#eef2fa")
