library(patchwork)

weodatcpi <- weodat |> .d(target == "pcpi_pch") |> .d(!is.na(tv_1))
weodatgdp <- weodat |> .d(target == "ngdp_rpch")|> .d(!is.na(tv_1))


val1 <- cor(weodatcpi$tv_0.5, weodatcpi$tv_1)
val2 <- mean(abs(weodatcpi$tv_0.5 - weodatcpi$tv_1))
val3 <- mean(abs(weodatcpi$tv_1))
plot1 <- ggplot(aes(x = tv_0.5, y = tv_1), data = weodatcpi) + geom_point() + geom_abline(aes(intercept = 0, slope = 1)) +
  theme_uqimf() +
  xlab("True Value Spring, Inflation") +
  ylab("True Value Fall, Inflation") +
  ggtitle("Inflation") +
  annotate(geom="text", x=2, y=9, label=paste0("correlation: ", round(val1,4), "\n mean absolute difference: ", round(val2,4), "\n mean value of vintage: ", round(val3,4)))

val1 <- cor(weodatgdp$tv_0.5, weodatgdp$tv_1)
val2 <- mean(abs(weodatgdp$tv_0.5 - weodatgdp$tv_1))
val3 <- mean(abs(weodatgdp$tv_1))
plot2 <- ggplot(aes(x = tv_0.5, y = tv_1), data = weodatgdp) + geom_point() + geom_abline(aes(intercept = 0, slope = 1)) +
  theme_uqimf() +
  xlab("True Value Spring, GDP Growth") +
  ylab("True Value Fall, GDP Growth") +
  ggtitle("GDP Growth") +
  annotate(geom="text", x=-5, y=7, label=paste0("correlation: ", round(val1,4), "\n mean absolute difference: ", round(val2,4), "\n mean value of vintage: ", round(val3,4)))

ovr_plot <- (plot1 + plot2)
