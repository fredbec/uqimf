scoredat <- data.table::fread(here("morectry.csv"))

scoresctry_pool1 <- scoreempQu(scoredat, cvg_rg = c(50,80),
                               by = c("model", "error_method", "method", "target")) |>
  .d(, c("model", "target", "coverage_50", "coverage_80")) |>
  melt(id.vars = c("model", "target"), variable.name = "lvl", value.name = "cvg") |>
  .d(,lvl := as.numeric(substr(lvl, 10, 20)))

scoresctry_pool2 <- scoreempQu(scoredat, cvg_rg = c(50,80),
                               by = c("model", "error_method", "method", "country", "target", "horizon")) |>
  .d(, c("model", "target", "horizon", "country", "coverage_50", "coverage_80")) |>
  melt(id.vars = c("model", "target", "country", "horizon"), variable.name = "lvl", value.name = "cvg") |>
  .d(,lvl := as.numeric(substr(lvl, 10, 20)))


ggplot() +
  geom_point(aes(x = lvl, y = cvg), data = scoresctry_pool2, alpha = 0.1) +
  geom_point(aes(x = lvl, y = cvg), size = 3, data = scoresctry_pool1, alpha = 1) +
  theme_uqimf() +
  facet_wrap(~target)

