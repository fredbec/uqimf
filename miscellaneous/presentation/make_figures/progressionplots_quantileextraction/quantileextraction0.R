library(ggplot2)

# Create a blank plot
p <- ggplot() + theme_void()

# Save to PDF with desired size (in inches)
ggsave(here("miscellaneous", "presentation", "make_figures", "progressionplots_quantileextraction", "illustration_pava_0.pdf"), plot = p,width = 10, height = 5)

#ggsave("illustration_quantileextraction_0.pdf", plot = p,width = 8, height = 3.75)
