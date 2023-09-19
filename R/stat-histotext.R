stat_histotext <- function(mapping = NULL, data = NULL,
                           position = "identity", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE, ...) {

  list(
    ggplot2::layer(
      stat = Histotext, data = data, mapping = mapping, geom = "text",
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, parse = TRUE,...)
    )
  )
}


Histotext <- ggplot2::ggproto("Histotext", ggplot2::Stat,
                                       required_aes = c("x"),
                                       compute_panel = function(data, scales, text = "O", binwidth = NULL, breaks = NULL, max_chars_per_column = 25) {
                                         data <- get_text_points(data, text, binwidth, breaks, max_chars_per_column)
                                         #print(data)
                                         data
                                       }
)



