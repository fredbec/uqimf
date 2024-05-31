library(here)
library(data.table)
library(ggplot2)
library(MetBrewer)
library(patchwork)

devtools::load_all()

.d <- `[`
### OECD actuals vs. IMF actuals

oecdbv <- fread(here("scores", "alt_scores", "oecd_bvar_ci_scores.csv"))|>
  .d(model == "bvar_qu")|>
  .d(,truth := "oecd") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score"))
oecdimf <- fread(here("scores", "alt_scores", "oecd_ci_scores_pava.csv"))|>
  .d(model == "IMF")|>
  .d(,truth := "oecd") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score")) |>
  rbind(oecdbv) |>
  dcast(country + target + horizon + truth ~ model, value.var = "interval_score")

imfbv <- fread(here("scores", "alt_scores", "imf_bvar_ci_scores.csv")) |>
  .d(model == "bvar_qu")|>
  .d(,truth := "imf") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score"))
imfimf <- fread(here("scores", "alt_scores", "imf_ci_scores_pava.csv")) |>
  .d(model == "IMF") |>
  .d(,truth := "imf") |>
  .d(, c("model", "country", "target", "horizon", "truth", "interval_score")) |>
  rbind(imfbv) |>
  dcast(country + target + horizon + truth ~ model, value.var = "interval_score") |>
  rbind(oecdimf)

allscores <- imfimf |>
  .d(, relwis := IMF - bvar_qu) |>
  dcast(country + target + horizon ~ truth, value.var = "relwis") |>
  .d(,horizon := factor(horizon, levels = c(0, 0.5, 1, 1.5), labels =
                          c("Fall, Current", "Spring, Current",
                            "Fall, Next", "Spring, Next")))


vsplot <- ggplot(aes(x = imf, y = oecd, color = as.factor(horizon)), data = allscores) +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  scale_color_met_d("Hokusai3") +
  theme_uqimf() +
  scale_x_continuous(limits =  c(-0.55, 0.55)) +
  scale_y_continuous(limits =  c(-0.55, 0.55)) +
  ylab("OECD truth") +
  xlab("IMF truth")


ggplot2::ggsave(here("..", "uqimf-manuscript", "figures", "oecdvsimf.pdf"),
                vsplot, width = 5, height = 4)
