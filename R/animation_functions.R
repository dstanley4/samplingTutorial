#' @export
plot_anim_ci_drep <- function(d = 1, n = 20, level = .95)  {


  df <- n - 1
  tncp = convert_drep_to_t(drep = d, n = n)

  LL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Lower.Limit
  UL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Upper.Limit

  LL_d <- convert_t_to_drep(t = LL_t, n = n)
  UL_d <- convert_t_to_drep(t = UL_t, n = n)

  df_ci      <- samplingTutorial:::get_drep_dist_values(d = d, n = n, level = level)
  df_outline <- samplingTutorial:::get_drep_dist_values(d = d, n = n, level = level, outline = TRUE)

  LL_max_density = max(df_ci$LL_density)
  UL_max_density = max(df_ci$UL_density)
  text_offset = max(LL_max_density, UL_max_density) * .10
  LL_label_y = LL_max_density + text_offset
  UL_label_y = UL_max_density + text_offset

  ymax <- dt(x = 0, df = df, ncp = 0) * 1.2
  xmin <- stats::qt(ncp = LL_t, df = df, p = .001)
  xmax <- stats::qt(ncp = UL_t, df = df, p = .999)
  xmin <- convert_t_to_drep(xmin, n = n)
  xmax <- convert_t_to_drep(xmax, n = n)

  pop_LL_str = sprintf("pop d = %1.2f", LL_d)
  pop_UL_str = sprintf("pop d = %1.2f", UL_d)

  ci_base <- ggplot(data = df_ci) +
    geom_vline(xintercept = d, color = "green") +
    geom_polygon(mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, fill = "red") +
    geom_polygon(mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, fill = "blue") +
    geom_path(data = df_outline, mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, color = "red") +
    geom_path(data = df_outline, mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, color = "blue") +
    annotate(geom = "text", x = LL_d, y = .2*ymax, label = "Middle 95%") +
    annotate(geom = "text", x = UL_d, y = .2*ymax, label = "Middle 95%") +
    annotate(geom = "text", x = LL_d, y = LL_label_y, label = pop_LL_str) +
    annotate(geom = "text", x = UL_d, y = UL_label_y, label = pop_UL_str) +
    labs(x = "d", y = "Density") +
    coord_cartesian(ylim = c(0, ymax), xlim = c(xmin, xmax)) +
    ggtitle(sprintf("d = %1.2f, 95%%[%1.2f, %1.2f]", d, LL_d, UL_d)) +
    theme_classic()


  # geom_density(data = df_ci,
  #              mapping = aes(x = UL_d_seq, y = UL_density)) +

  return(ci_base)

}

get_drep_dist_values <- function(d, n, level, outline = FALSE) {

  level_low <- (1-level)/2
  level_high <- 1 - (1-level)/2

  df = n - 1
  tncp = convert_drep_to_t(drep = d, n = n)

  LL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Lower.Limit
  UL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Upper.Limit

  if (outline == FALSE) {
    LL_left  <- stats::qt(ncp = LL_t, df = df, p = level_low)
    LL_right <- stats::qt(ncp = LL_t, df = df, p = level_high)

    UL_left   <- stats::qt(ncp = UL_t, df = df, p = level_low)
    UL_right  <- stats::qt(ncp = UL_t, df = df, p = level_high)

  } else {
    LL_left  <- stats::qt(ncp = LL_t, df = df, p = .001)
    LL_right <- stats::qt(ncp = LL_t, df = df, p = .999)

    UL_left   <- stats::qt(ncp = UL_t, df = df, p = .001)
    UL_right  <- stats::qt(ncp = UL_t, df = df, p = .999)
  }


  LL_t_seq <- seq(LL_left, LL_right, by = (LL_right - LL_left)/500)
  UL_t_seq <- seq(UL_left, UL_right, by = (UL_right - UL_left)/500)

  LL_d_seq <- convert_t_to_drep(t = LL_t_seq, n = n)
  UL_d_seq <- convert_t_to_drep(t = UL_t_seq, n = n)

  LL_density <- stats::dt(x = LL_t_seq, df = df, ncp = LL_t)
  UL_density <- stats::dt(x = UL_t_seq, df = df, ncp = UL_t)

  LL_density[1] <- 0
  LL_density[length(LL_density)] <- 0

  UL_density[1] <- 0
  UL_density[length(UL_density)] <- 0

  output <- data.frame(LL_t_seq, UL_t_seq,
                       LL_d_seq, UL_d_seq,
                       LL_density, UL_density)
  output <- rbind(output, output[1,])

  return(output)
}


convert_drep_to_t <- function(drep, n) {
  t <- drep * sqrt(n)
  return(t)
}


convert_t_to_drep <- function(t, n) {
  drep <- t / sqrt(n)
  return(drep)
}



