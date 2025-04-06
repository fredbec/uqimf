library(data.table)
library(knitr)
library(kableExtra)
library(ggplot2)
library(MetBrewer)
library(patchwork)
devtools::load_all()

prefix <- "extcis_"

ciscores <- fread(here("scores", paste0(prefix, "cvg_pooled_ho.csv"))) |>
  .d(model != "bvar") |> #filter out SV BVAR model (not used anymore)
  .d(, c("model", "target", paste0("qucoverage_", c(seq(5,45, by = 5), seq(55,95, by = 5))))) |>
  .d(, target := ifelse(target == "ngdp_rpch", "real GDP Growth", "Inflation")) |>
  .d(, model := gsub("_", "", model)) |>
  melt(id.vars = c("model", "target"), variable.name = "lvl", value.name = "cvg") |>
  .d(,lvl := as.numeric(substr(lvl, 12, 20)))

ciscores2 <- fread(here("scores", paste0(prefix, "bvar_cvg_pooled_ho.csv"))) |>
  .d(model != "bvar_qu") |> #filter out SV BVAR model (not used anymore)
  .d(, c("model", "target", paste0("qucoverage_", c(seq(5,45, by = 5), seq(55,95, by = 5))))) |>
  .d(, target := ifelse(target == "ngdp_rpch", "real GDP Growth", "Inflation")) |>
  .d(, model := gsub("_", "", model)) |>
  .d(, model := paste0(model, "-direct")) |>
  melt(id.vars = c("model", "target"), variable.name = "lvl", value.name = "cvg") |>
  .d(,lvl := as.numeric(substr(lvl, 12, 20)))

ciscores_perfect <- data.table(model = rep("nominal", 18),
                               target = rep("real GDP Growth", 18),
                               lvl = c(seq(5,45, by = 5), seq(55,95, by = 5)),
                               cvg = c(seq(5,45, by = 5), seq(55,95, by = 5))/100)

ciscores_perfect <- rbind(ciscores_perfect,
                          ciscores_perfect |>
                            copy() |>
                            .d(,target := "Inflation"))

ciscores1 <- rbind(ciscores, ciscores_perfect) |>
  .d(, lvl := lvl/100) |>
  .d(, dev :=  cvg-lvl) |>
  .d(, model := fifelse(model == "ar-direct", "Direct: AR",
                        fifelse(model == "arannual-direct", "Direct: AR-annual",
                                fifelse(model == "arbic-direct", "Direct: AR(p)",
                                        fifelse(model == "bvarconst-direct", "Direct: BVAR-Const.",
                                                fifelse(model == "bvarqu-direct", "Direct: BVAR-SV",
                                                        fifelse(model == "ar", "AR",

                                                                fifelse(model == "bvarmix", "BVAR-Mix",

                                                                        fifelse(model == "bvarmix-direct", "Direct: BVAR-Mix",
                                                                                fifelse(model == "arbic", "AR(p)",
                                                                                        fifelse(model == "bvar", "BVAR-SV",
                                                                                                fifelse(model == "bvarconst", "BVAR-Const.",
                                                                                                        fifelse(model == "meanensemble", "Ensemble",
                                                                                                                fifelse(model == "IMF", "IMF", model))))))))))))))

ciscores2 <- ciscores2 |>
  rbind(ciscores[model == "IMF"]) |>
  rbind(ciscores_perfect) |>
  .d(, lvl := lvl/100) |>
  .d(, dev := cvg-lvl) |>
  .d(, model := fifelse(model == "ar-direct", "Direct: AR",
                        fifelse(model == "arannual-direct", "Direct: AR-annual",
                                fifelse(model == "arbic-direct", "Direct: AR(p)",
                                        fifelse(model == "bvarconst-direct", "Direct: BVAR-Const.",
                                                fifelse(model == "bvarqu-direct", "Direct: BVAR-SV",
                                                        fifelse(model == "ar", "AR",
                                                                fifelse(model == "arxannual-direct", "Direct: ARX-annual",

                                                                        fifelse(model == "bvarmix", "BVAR-Mix",

                                                                                fifelse(model == "bvarmix-direct", "Direct: BVAR-Mix",
                                                                                        fifelse(model == "arbic", "AR(p)",
                                                                                                fifelse(model == "bvar", "BVAR-SV",
                                                                                                        fifelse(model == "bvarconst", "BVAR-Const.",
                                                                                                                fifelse(model == "meanensemble", "Ensemble",
                                                                                                                        fifelse(model == "IMF", "IMF", model)))))))))))))))


# Define colors

qucvg_plot <- function(plotdat){
  fancycols <- met.brewer("Hokusai3", n = length(unique(plotdat$model)))[2:length(unique(plotdat$model))]  # Get 10 colors from VanGogh1 palette
  imfcol <-  met.brewer("Hokusai3", n = 4)[1]

  modnames <- unique(plotdat$model)
  modnames <- modnames[modnames != "IMF"] # gets its own manual color

  color_map <- c("nominal" = "black", "IMF" = imfcol, setNames(fancycols, modnames))
  textsize_y <- 17
  font_family <- "serif"

  nomdat <- plotdat |>
    .d(model == "nominal")

  plotdat <- plotdat |>
    .d(model != "nominal")


  plot1 <- ggplot() +
    geom_line(aes(x = lvl, y = cvg, group = model), data = nomdat, color ="black") +
    geom_line(aes(x = lvl, y = cvg, group = model, color = model), data = plotdat) +
    geom_point(aes(x = lvl, y = cvg, group = model, color = model), data = plotdat, shape = 18) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    facet_wrap(~target) +
    scale_x_continuous(breaks=seq(0, 1, 0.1)) +
    scale_y_continuous(breaks=seq(0, 1, 0.1)) +
    theme_uqimf() +
    xlab("Nominal Coverage (in %)") +
    ylab("Empirical Coverage (in %)") +
    theme_uqimf() %+replace%
    theme(#axis.text.x = element_blank(),
      axis.text.x = element_text(size = textsize_y),
      #strip.text = element_text(size = 8),
      axis.text.y = element_text(size = textsize_y),

      axis.title.x = element_text(size = textsize_y),
      axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
      strip.text = element_text(size=textsize_y),
      legend.text=element_text(size=textsize_y),
      text = element_text(family = font_family),
      legend.title=element_blank(),
      plot.margin = margin(t=0,b=20,r=10,l=10, unit = "pt"))

  return(plot1)

}

devqucvg_plot <- function(plotdat, Colscale){
  fancycols <- met.brewer(Colscale, n = length(unique(plotdat$model)))[2:length(unique(plotdat$model))]  # Get 10 colors from VanGogh1 palette
  imfcol <-  met.brewer("Hokusai3", n = 4)[1]

  modnames <- unique(plotdat$model)
  modnames <- modnames[modnames != "IMF"] # gets its own manual color

  color_map <- c("nominal" = "black", "IMF" = imfcol, setNames(fancycols, modnames))
  #print(color_map)
  textsize_y <- 15
  font_family <- "serif"

  nomdat <- plotdat |>
    .d(model == "nominal")

  plotdat <- plotdat |>
    .d(model != "nominal")


  plot1 <- ggplot() +
    geom_hline(yintercept = 0, color = "black") +
    geom_line(aes(x = lvl, y = dev, group = model, color = model), data = plotdat) +
    geom_point(aes(x = lvl, y = dev, group = model, color = model), data = plotdat, shape = 18) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    facet_wrap(~target) +
    scale_x_continuous(breaks=seq(0, 1, 0.1)) +
    scale_y_continuous(limits = c(-0.353, 0.353), breaks=seq(-0.4, 0.4, 0.1)) +
    theme_uqimf() +
    xlab("Nominal Coverage Level") +
    ylab("Deviation from Nominal Coverage") +
    theme_uqimf() %+replace%
    theme(#axis.text.x = element_blank(),
      axis.text.x = element_text(size = textsize_y),
      #strip.text = element_text(size = 8),
      axis.text.y = element_text(size = textsize_y),

      axis.title.x = element_text(size = textsize_y),
      axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
      strip.text = element_text(size=textsize_y),
      legend.text=element_text(size=textsize_y),
      text = element_text(family = font_family),
      legend.title=element_blank(),
      plot.margin = margin(t=0,b=20,r=10,l=10, unit = "pt"))

  return(plot1)

}

if(FALSE){
  van_gogh_colors <- met.brewer("Hokusai3", n = length(unique(ciscores1$model)))  # Get 10 colors from VanGogh1 palette
  color_map <- c("nominal" = "black", setNames(van_gogh_colors, unique(ciscores1$model)))
  textsize_y <- 17
  font_family <- "serif"


  plot1 <- ggplot(aes(x = lvl, y = cvg, group = model, color = model, fill = model), data = ciscores1) +
    geom_line() +
    geom_point(shape = 18) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    facet_wrap(~target) +
    theme_uqimf() +
    xlab("Nominal Coverage Level") +
    ylab("Empirical Coverage Level") +
    theme_uqimf() %+replace%
    theme(#axis.text.x = element_blank(),
      axis.text.x = element_text(size = textsize_y),
      #strip.text = element_text(size = 8),
      axis.text.y = element_text(size = textsize_y),

      axis.title.x = element_text(size = textsize_y),
      axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
      strip.text = element_text(size=textsize_y),
      legend.text=element_text(size=textsize_y),
      text = element_text(family = font_family),
      legend.title=element_blank(),
      plot.margin = margin(t=0,b=0,r=10,l=10, unit = "pt"))


  van_gogh_colors <- met.brewer("Hokusai3", n = length(unique(ciscores2$model)))  # Get 10 colors from VanGogh1 palette
  color_map <- c("nominal" = "black", setNames(van_gogh_colors, unique(ciscores2$model)))
  plot2 <- ggplot(aes(x = lvl, y = cvg, group = model, color = model, fill = model), data = ciscores2) +
    geom_line() +
    geom_point(shape = 18) +
    scale_color_manual(values = color_map) +
    scale_fill_manual(values = color_map) +
    facet_wrap(~target) +
    theme_uqimf()+
    xlab("Nominal Coverage Level") +
    ylab("Empirical Coverage Level") +
    theme_uqimf() %+replace%
    theme(#axis.text.x = element_blank(),
      axis.text.x = element_text(size = textsize_y),
      #strip.text = element_text(size = 8),
      axis.text.y = element_text(size = textsize_y),

      axis.title.x = element_text(size = textsize_y),
      axis.title.y = element_text(size = textsize_y, angle = 90, vjust = 2),
      strip.text = element_text(size=textsize_y),
      legend.text=element_text(size=textsize_y),
      text = element_text(family = font_family),
      legend.title=element_blank(),
      plot.margin = margin(t=0,b=0,r=10,l=10, unit = "pt"))
}

plot1 <- qucvg_plot(ciscores1)
plot2 <- qucvg_plot(ciscores2)

ovr_plot <- (plot1) /
  (plot2)
ovr_plot
ggsave(here("manuscript_plots", "revision", "results", "extcis_qucvgplot.pdf"), width = 8.3, height = 10.5)
ggsave(here("..", "uqimf-manuscript", "figures", "extcis_qucvgplot.pdf"), width = 8.3, height = 10.5)


plot1 <- devqucvg_plot(ciscores1, "Hokusai3")
plot2 <- devqucvg_plot(ciscores2, "OKeeffe2")

ovr_plot <- (plot1) /
  (plot2)
ovr_plot
ggsave(here("manuscript_plots", "revision", "results", "extcis_devqucvgplot.pdf"), width = 8.3, height = 10.5)
ggsave(here("..", "uqimf-manuscript", "figures", "extcis_devqucvgplot.pdf"), width = 8.3, height = 10.5)
