library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
library(patchwork)

devtools::load_all()

.d <- `[`
### OECD actuals vs. IMF actuals

oecdbv <- fread(here("scores", "oecd_bvar_ci_scores_ho.csv"))|>
  .d(model == "bvar_const") |>
  .d(, model := "bvar_const-direct") |>
  .d(,truth := "oecd") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score"))
cntbv <- unique(oecdbv$country)

oecdimf <- fread(here("scores", "oecd_ci_scores_ho.csv"))|>
  .d(model == "IMF")|>
  .d(,truth := "oecd") |>
  .d(country %in% cntbv) |>
  .d(method == "rolling window") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score")) |>
  rbind(oecdbv) |>
  dcast(country + target + horizon + truth ~ model, value.var = "interval_score")

imfbv <- fread(here("scores", "bvar_ci_scores_ho.csv")) |>
  .d(model == "bvar_const") |>
  .d(, model := "bvar_const-direct") |>
  .d(,truth := "imf") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score"))
imfimf <- fread(here("scores", "ci_scores_ho.csv")) |>
  .d(model == "IMF") |>
  .d(,truth := "imf") |>
  .d(country %in% cntbv) |>
  .d(method == "rolling window") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score")) |>
  rbind(imfbv) |>
  dcast(country + target + horizon + truth ~ model, value.var = "interval_score") |>
  rbind(oecdimf)

allscores <- imfimf |>
  .d(, relwis := IMF - `bvar_const-direct`) |>
  dcast(country + target + horizon ~ truth, value.var = "relwis") |>
  .d(,horizon := factor(horizon, levels = c(0, 0.5, 1, 1.5), labels =
                          c("Fall, Current", "Spring, Current",
                            "Fall, Next", "Spring, Next")))

axislimits <- c(max(allscores$imf), max(allscores$oecd),
                min(allscores$imf), min(allscores$oecd)) |>
  abs() |>
  max()

valposx <- axislimits
valposy <- valposx -0.035

vsplot <- ggplot(aes(x = imf, y = oecd, color = as.factor(horizon)), data = allscores) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_color_met_d("Hokusai3") +
  theme_uqimf() %+replace%
  theme(
    text = element_text(family = "serif")) +
  annotate("text", x = -valposx, y = valposy,
           label = "I",
           family = "serif", size = 7) +
  annotate("text", x = valposx, y = valposy,
           label = "II",
           family = "serif", size = 7) +
  annotate("text", x = valposx, y = -valposy,
           label = "III",
           family = "serif", size = 7) +
  annotate("text", x = -valposx, y =- valposy,
           label = "IV",
           family = "serif", size = 7) +
  scale_x_continuous(limits =  c(-axislimits, axislimits)) +
  scale_y_continuous(limits =  c(-axislimits, axislimits)) +
  ylab("OECD truth") +
  xlab("IMF truth")

ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", "ho_oecdvsimf.pdf"),
                vsplot, width = 5, height = 4)
