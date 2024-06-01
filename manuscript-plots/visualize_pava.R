library(ggplot2)
library(MetBrewer)
library(data.table)
library(gghighlight)
library(here)
devtools::load_all()

.d <- `[`

lwd_plot <- 14

lower50 <- c(-0.5, -1.25, -0.9)
clowerq5 <- c(lower50[1], lower50[2], lower50[3], lower50[1], mean(lower50[2:3]), mean(lower50[2:3]))
lower80 <- c(-1.25, -1.75, -2.2)
clowerq8 <- c(lower80[1], lower80[2], lower80[3], lower80[1], mean(lower80[2:3]), mean(lower80[2:3]))

linerangedat <- data.table(
  posx = c(1,1.35,1.7,2.6,2.95,3.3),
  lowerq5 = clowerq5,
  upperq5 = -clowerq5,
  lowerq8 = clowerq8,
  upperq8 = -clowerq8,
  mycols = rep(1,6),
  pointfc = rep(0, 6)
)


#######################SOme Annotation Functions##############################

ann_hor <- function(numk){
  subscript <- ifelse(numk %% 3 == 1, "1",
                      ifelse(numk %% 3 == 2, "2", "3"))
  annotate("text",
           x = linerangedat$posx[numk],
           y = -2.5,
           label = paste0("h[", subscript, "]"),
           parse = TRUE,
           size = 7,
           family = "serif")
}

ann_less <- function(x){
  annotate("text",
           x = x,
           y = -2.5,
           label = "'<'",
           parse = TRUE,
           size = 7,
           family = "serif")
}
geom_segment_arrow <- function(x, y, xend, yend = y,
                               lineend = "butt", linejoin = "round",
                               linetype = "dashed", size = 0.25,
                               arrow = NULL) {
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "butt", linejoin = "round",
    color = "black", alpha = 1,
    size = 0.5, arrow = arrow(length = unit(0.075, "inches"))
  )
}

mycol <- met.brewer("Hokusai3", 1)

#######################End of: Some Annotation Functions##############################

vispava <- ggplot(aes(x = posx, color = as.factor(mycols)), data = linerangedat) +
    geom_linerange(
    aes(x = posx,
        ymin = lowerq5,
        ymax = upperq5,
        alpha = "50% Interval"),
    #alpha = 0.5,
    lwd = lwd_plot
  ) +
  geom_linerange(
    aes(x = posx,
        ymin = lowerq8,
        ymax = upperq8,
        alpha = "80% Interval"),
    #alpha = 0.5,
    lwd = lwd_plot
  ) +
  geom_point(aes(x = posx, y = pointfc, color = as.factor(mycols)), size = 7) +


  ann_hor(1) +
  ann_hor(2) +
  ann_hor(3) +
  ann_hor(4) +
  ann_hor(5) +
  ann_hor(6) +
  ann_less(mean(linerangedat$posx[1:2])) +
  ann_less(mean(linerangedat$posx[2:3])) +
  ann_less(mean(linerangedat$posx[4:5])) +
  ann_less(mean(linerangedat$posx[5:6])) +
  geom_segment_arrow(linerangedat$posx[3]+ 0.15, 0, linerangedat$posx[4]-0.15)+

  annotate("text", x = linerangedat$posx[3] + 0.45, 0.175, label = "pool violations",
           family = "serif", size = 7) +

  annotate("text", x = linerangedat$posx[3] + 0.45, -0.175,
           label = expression("at " * h[2] * ", " * h[3]),
           parse = TRUE,
           family = "serif", size = 7) +

  scale_color_met_d("Hokusai3") +
  xlim(0.75, 3.55) +


  guides(color = "none") +
  theme_uqimf() %+replace%
  theme(
    text = element_text(family = "serif"),
    axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
    legend.position = "right",
    legend.text=element_text(size=20),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.0, 'cm'),
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank()) +
  scale_alpha_manual(name = "",
                     breaks = c("50% Interval", "80% Interval"),
                     values = c("50% Interval" = 0.8, "80% Interval" = 0.4),
                     labels = c(expression(tau[1]), expression(tau[2]))) +

  guides(
    alpha = guide_legend(
      title = "",
      override.aes = list(color = c(mycol[1]))
    )
  ) +
  guides(fill = guide_legend(byrow = TRUE))

ggsave(here("..", "uqimf-manuscript", "figures", "illustration_pava.pdf"), vispava, width = 10, height = 5)

