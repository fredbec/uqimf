library(here)
library(data.table)
library(sandwich)
.d <- `[`

# Diebold-Mariano test implementation, similar to function t_hac here:
# https://github.com/FK83/forecastcalibration/blob/main/R/R_procs.R
dm_test <- function(loss1, loss2){
  # make series of loss differences
  # loss1 is for model 1, loss2 is for model 2
  # smaller loss is better
  d <- loss1 - loss2
  # mean loss
  m <- mean(d)
  # variance estimated via Newey-West method
  # regression of d on intercept
  fit <- lm(d~1)
  # robust standard error
  s <- NeweyWest(fit, prewhite = TRUE) |> unname() |> sqrt() |> as.numeric()
  #variance for s3tilde
  std_var <- sum(fit$residuals^2) / (length(loss1)-1)
  s_var <- sqrt(std_var / (length(loss1)))
  # t statistic
  # t > 0 indicates that model 2 is better
  # t < 0 indicates that model 1 is better
  t <- m/s
  t_var <- m/s_var
  # p-value (two-sided)
  p <- 2*pnorm(-abs(t))
  p_2 <- 2*pt(-abs(t_var),10)
  # lag length used by estimator
  k <- bwNeweyWest(fit, prewhite = TRUE)
  # output
  list(t_stat = t, p_val = p, bandwidth = k,
       t_stat_s3tilde = t_var, p_val_s3tilde = p_2)
}

dm_dat <- vector(mode = "list", length = 24)

dm_acf <- vector(mode = "list", length = 24)
i <- 0

combs_methods <- data.table::CJ(hr = c(0, 0.5, 1, 1.5),
                                tgt = c("pcpi_pch", "ngdp_rpch"),
                                model1 = c("IMF"),
                                model2 = c("ar", "bvar_const", "bvar_const-direct"))

for(i in 1:nrow(combs_methods)){
  curr_comb <- combs_methods[i,]
  curr_hr <- curr_comb[,"hr"] |> unname() |> unlist()
  curr_tgt <- curr_comb[,"tgt"] |> unname() |> unlist()
  curr_mod1 <-  curr_comb[,"model1"] |> unname() |> unlist()
  curr_mod2 <-  curr_comb[,"model2"] |> unname() |> unlist()


  ####SCORE EINLESEN ÄNDERN
  #loss_mod1 <- fread(here("scores", "extcis_crps_values_ho_byyr.csv")) |>
  loss_mod1 <- fread(here("scores", "ci_scores_byyr_ho.csv")) |>
    .d(horizon == curr_hr) |>
    .d(target == curr_tgt) |>
    .d(model == curr_mod1) |>
    .d(, "interval_score") |>
    #.d(, "score") |>
    unname() |> unlist()

  #loss_mod2 <- fread(here("scores", "extcis_crps_values_ho_byyr.csv"))## |>
  if(curr_mod2 == "bvar_const-direct"){
    filename <- "bvar_ci_scores_byyr_ho.csv"
    curr_mod2 <- "bvar_const" #type of model (direct / not direct) is governed by file name
    curr_mod2_new <- "bvar_const-direct"
  } else {
    filename <- "ci_scores_byyr_ho.csv"
    curr_mod2_new <- "bvar_const"
  }

  loss_mod2 <- fread(here("scores", filename)) |>
    .d(horizon == curr_hr) |>
    .d(target == curr_tgt) |>
    .d(model == curr_mod2)|>
    .d(, "interval_score") |>
    #.d(, "score") |>
    unname() |> unlist()

  if(curr_mod2_new == "bvar_const-direct"){
    curr_mod2 <- curr_mod2_new
  }

  dm_res <- dm_test(loss1 = loss_mod1, loss2 = loss_mod2)

  dm_dat[[i]] <- data.table(model1 = curr_mod1,
                            model2 = curr_mod2,
                            horizon = curr_hr,
                            target = curr_tgt,
                            tstat = dm_res$t_stat,
                            p_val = dm_res$p_val,
                            bandwidth = dm_res$bandwidth,
                            tstat_s3tilde = dm_res$t_stat_s3tilde,
                            p_val_s3tilde = dm_res$p_val_s3tilde)

  #for additional visual diagnostics (investigate negative/positive autocorrelation)
  dm_acf[[i]] <- vector(mode = "list", length = 2)
  dm_acf[[i]][[1]] <- acf((loss_mod1-loss_mod2), plot = FALSE)
  plottgt <- ifelse(curr_tgt == "pcpi_pch", "Inf.", "GDP")
  dm_acf[[i]][[2]] <- paste0(plottgt,", ", curr_mod2, ", hor. ", curr_hr)
}

dm_dat <- rbindlist(dm_dat)
data.table::fwrite(dm_dat, here("manuscript_plots", "revision", "results", "dm_test_results_wis.csv"))


#Save acf plots
pdf(here("manuscript_plots", "revision", "results", "acf_lossdifferentials_ngdp_rpch.pdf"), width = 8, height = 6)
par(mfrow = c(3, 4), cex.main = 0.85)
for (k in c(1:3, 7:9, 13:15, 19:21)) {
  plot(dm_acf[[k]][[1]], main = dm_acf[[k]][[2]])
}
dev.off()
pdf(here("manuscript_plots", "revision", "results", "acf_lossdifferentials_pcpi_pch.pdf"), width = 8, height = 6)
par(mfrow = c(3, 4), cex.main = 0.85)
for (k in c(4:6, 10:12, 16:18, 22:24)) {
  plot(dm_acf[[k]][[1]], main = dm_acf[[k]][[2]])
}
dev.off()
