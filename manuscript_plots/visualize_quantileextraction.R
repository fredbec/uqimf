library(ggplot2)
library(MetBrewer)
library(data.table)
library(gghighlight)
library(here)
devtools::load_all()

.d <- `[`

set.seed(2922)
w <- 11
labelvec <- c("raw error\nvalues, from\n previous 11 years", "error values,\nabsolute transformed", "6th and 9th largest\nvalue highlighted", "predictive distribution,\nconstructed around\n a point forecast of zero")

errors <- rnorm(w) |> sort()
errors <- errors * (-1)
errors[1] <- 2.49
abs_errors <- errors |> abs() ##|> sort()

##make data
errordat <- data.frame(
  type = rep(1, w),
  value = errors,
  col = 1:11
)
errordat_abs_before <- data.frame(
  type = rep(2, w),
  value = abs_errors,
  col = 1:11
)
errordat_abs <- data.frame(
  type = rep(3, w),
  value = abs_errors,
  col = 1:11
)
errordat_extra <- data.frame(
  type = rep(4, w),
  value = NA,
  col = 1:11
)



errordat <- rbind(errordat, errordat_abs) |>
  rbind(errordat_abs_before) |>
  rbind(errordat_extra) |>
  setDT() |>
  .d(, value := ifelse(type == 3, value, value + 1e-09))

val80 <- quantile(abs_errors, 0.8)
val50 <- quantile(abs_errors, 0.5)

linerangedat <- data.frame(
  type = rep(c(1, 2, 4), each = 1),
  upper80 = c(0, 0, val80),
  lower80 = c(0, 0, -val80),
  upper50 = c(0, 0, val50),
  lower50 = c(0, 0, -val50)
) |>
  setDT()

pointdat <- data.frame(
  type = rep(c(1, 2,4), each = 1),
  pointval = c(NA, NA, 0))|>
  setDT()


#Define segment function
geom_segment_hor <- function(x, y, xend, yend = y,
                    lineend = "butt", linejoin = "round",
                    color = "grey80", alpha = 0.8,
                    linetype = "dashed", size = 0.25,
                    arrow = NULL) {
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    lineend = lineend, linejoin = linejoin,
    color = color, alpha = alpha,
    linetype = linetype, size = size,
    arrow = arrow
  )
}
#Define segment function
geom_segment_arrow <- function(x, y, xend, yend = y,
                             lineend = "butt", linejoin = "round",
                             color = "grey80", alpha = 0.8,
                             linetype = "dashed", size = 0.25,
                             arrow = NULL) {

  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    lineend = "butt", linejoin = "round",
    color = "grey80", alpha = 0.8,
    size = 0.5, arrow = arrow(length = unit(0.075, "inches"))
  )
}

lrcols <- met.brewer("Hokusai2", 2)

quantvis <- ggplot() +
  geom_point(aes(x = type, y = value, color = as.factor(col)), data = errordat, size = 4) +
  gghighlight(value %in% c(val50, val80)) +
  scale_color_met_d("Hokusai2") +
  geom_hline(aes(yintercept = 0), linetype = "solid", color = "grey80", alpha = 0.7, size = 0.3) +
  guides(color = "none") +
  theme_uqimf() %+replace%
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "right",
        text = element_text(family = "serif"),
        #axis.text.x = element_text(size = textsize_y, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        #strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        strip.text = element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        plot.margin = margin(t=5,b=5,r=5,l=5, unit = "pt")) +
  geom_linerange(
    aes(x = type,
        ymin = lower80,
        ymax = upper80),
    color = lrcols[1],
    data = linerangedat,
    lwd = 7
  ) +
  geom_linerange(
    aes(x = type,
        ymin = lower50,
        ymax = upper50),
    color = lrcols[2],
    data = linerangedat,
    lwd = 7
  ) +
  geom_segment_arrow(x = 1.15, xend = 1.85, y = -2.4) +
  geom_segment_arrow(x = 2.15, xend = 2.85, y = -2.4) +
  geom_segment_arrow(x = 3.15, xend = 3.85, y = -2.4) +
  geom_segment_hor(x = 3.05, xend = 3.935, y = val50) +
  geom_segment_hor(x = 3.05, xend = 3.935, y = val80) +
  geom_segment_hor(x = 4.065, xend = 4.255, y = val50) +
  geom_segment_hor(x = 4.065, xend = 4.255, y = val80) +
  annotate(
    "text", label = "80% Interval\n",
    x = 4.37, y = val80, size = 3.75, colour = "grey60"
  ) +
  annotate(
    "text", label = "50% Interval\n",
    x = 4.37, y = val50, size = 3.75, colour = "grey60"
  ) +
  geom_point(aes(x = type, y = pointval), data = pointdat, color = lrcols[2], size = 1.5, pch = 23) +
  scale_y_continuous(limits = c(-2.5, 2.5)) +
  scale_x_continuous(breaks = c(1,2,3,4), labels = labelvec, limits = c(0.55, 4.45)) +
  ylab("") +
  xlab("")

#quantvis
ggsave(here("..", "uqimf-manuscript", "figures", "illustration_quantileextraction.pdf"), quantvis, width = 7, height = 4.5)

