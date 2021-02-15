theme_trolley <- function() {
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, face = "bold", size = 17),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.text.x = element_text(face = "bold", size = 7, colour = "Black"),
    axis.text.y = element_text(face = "bold", size = 12, colour = "Black"),
    legend.title = element_blank(),
    legend.text = element_text(size = 9))
  }