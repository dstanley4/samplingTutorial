#' @export
plot_anim_ci_drep <- function(d = 1, n = 20, level = .95, center.level = NULL, graph = "both")  {

  if (is.null(center.level) == TRUE) {
    center.level = level
  }


  df <- n - 1
  tncp = convert_drep_to_t(drep = d, n = n)

  LL_tlabel <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Lower.Limit
  UL_tlabel <- MBESS::conf.limits.nct(tncp, df, conf.level = level)$Upper.Limit
  LL_dlabel <- convert_t_to_drep(t = LL_tlabel, n = n)
  UL_dlabel <- convert_t_to_drep(t = UL_tlabel, n = n)

  df_ci_LL_label      <- samplingTutorial:::get_drep_dist_values(d = d, n = n,
                                                        level = level,
                                                        center.level = level)

  df_outline_LL_label <- samplingTutorial:::get_drep_dist_values(d = d, n = n,
                                                        level = level,
                                                        center.level = level,
                                                        outline = TRUE)
  LL_label_max_density = max(df_ci_LL_label$LL_density)
  UL_label_max_density = max(df_ci_LL_label$UL_density)
  text_label_offset = max(LL_label_max_density, UL_label_max_density) * .10
  LL_2label_y = LL_label_max_density + text_label_offset


  LL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = center.level)$Lower.Limit
  UL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = center.level)$Upper.Limit
  LL_d <- convert_t_to_drep(t = LL_t, n = n)
  UL_d <- convert_t_to_drep(t = UL_t, n = n)

  df_ci      <- samplingTutorial:::get_drep_dist_values(d = d, n = n,
                                                        level = level,
                                                        center.level = center.level)

  df_outline <- samplingTutorial:::get_drep_dist_values(d = d, n = n,
                                                        level = level,
                                                        center.level = center.level,
                                                        outline = TRUE)


  LL_max_density = max(df_ci$LL_density)
  UL_max_density = max(df_ci$UL_density)
  text_offset = max(LL_max_density, UL_max_density) * .10
  LL_label_y = LL_max_density + text_offset
  UL_label_y = UL_max_density + text_offset

  myymax <- dt(x = 0, df = df, ncp = 0) * 1.2
  myxmin <- stats::qt(ncp = LL_tlabel, df = df, p = .001)
  myxmax <- stats::qt(ncp = UL_tlabel, df = df, p = .999)
  myxmin <- convert_t_to_drep(myxmin, n = n)
  myxmax <- convert_t_to_drep(myxmax, n = n)

  pop_LL_str = sprintf("population\nd = %1.2f", LL_d)
  pop_LL_str2 = sprintf("population\nd = %1.2f", LL_dlabel)

  pop_UL_str = sprintf("population\nd = %1.2f", UL_d)
  pop_UL_str2 = sprintf("population\nd = %1.2f", UL_dlabel)

  title_str <- sprintf("d = %1.2f, 95%%[%1.2f, %1.2f]", d, LL_dlabel, UL_dlabel)
  subtitle_str <- "Nonpivotal CI method (see Kelley, 2007)"

  fontsize = 8



  if (graph == "justeffect") {
    ci_start <- ggplot(data = df_ci) +
      labs(x = "d", y = "Density", title = title_str, subtitle = subtitle_str) +
      annotate(geom = "segment", x = d, xend = d, y = 0, yend = .3*myymax, linewidth = 1) +
      annotate(geom = "text", size = fontsize, x = d, y = .35*myymax, label = sprintf("sample\nd = %1.2f", d)) +
      coord_cartesian(xlim = c(myxmin, myxmax), ylim = c(0, myymax*1.2)) +
      annotate(geom = "text", size = 10, x = d, y = myymax*1.1, label = "Step 1: Sample d-value\nWe start with the sample d-value.", hjust = 0) +
      theme_classic(24)
    output = ci_start

  } else if (graph == "left") {
    ci_base_LL <- ggplot(data = df_ci) +
      geom_polygon(mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, fill = "red") +
      geom_path(data = df_outline, mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, color = "red", linewidth = 2) +
      annotate(geom = "text", size = fontsize, x = LL_d, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      annotate(geom = "text", size = fontsize, x = LL_d, y = LL_label_y, label = pop_LL_str, parse = FALSE) +
      labs(x = "d", y = "Density", title = title_str, subtitle = subtitle_str) +
      annotate(geom = "segment", x = d, xend = d, y = 0, yend = .3*myymax, linewidth = 1) +
      annotate(geom = "text", size = fontsize, x = d, y = .35*myymax, label = sprintf("sample\nd = %1.2f", d)) +
      coord_cartesian(xlim = c(myxmin, myxmax), ylim = c(0, myymax*1.2)) +
      annotate(geom = "text", size = 10, x = d, y = myymax*1.1, label = "Step 2 Lower Limit:\nIteratively DECREASE hypothetical population d-value\nuntil the edge of the middle 95% is the sample d-value.\nNotice how the sampling distribution changes skew/shape.", hjust = 0) +
      theme_classic(24)

    output = ci_base_LL
  } else if (graph == "leftfreeze") {
    ci_base_LLfreeze <- ggplot(data = df_ci) +
      geom_polygon(data = df_ci_LL_label,mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, fill = "red") +
      geom_path(data = df_outline_LL_label, mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, color = "red", linewidth = 2) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = LL_2label_y, label = pop_LL_str2, parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = LL_2label_y+text_label_offset, label = "Lower Limit", parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      labs(x = "d", y = "Density", title = title_str, subtitle = subtitle_str) +
      annotate(geom = "segment", x = d, xend = d, y = 0, yend = .3*myymax, linewidth = 1) +
      annotate(geom = "text", size = fontsize, x = d, y = .35*myymax, label = sprintf("sample\nd = %1.2f", d)) +
      coord_cartesian(xlim = c(myxmin, myxmax), ylim = c(0, myymax*1.2)) +
      annotate(geom = "text", size = 10, x = d, y = myymax*1.1 , label = "We have found the Lower Limit of the confidence interval.") +
    theme_classic(24)

    output = ci_base_LLfreeze
  } else if (graph == "right") {
    ci_base_UL <- ggplot(data = df_ci) +
      geom_polygon(data = df_ci_LL_label,mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, fill = "red") +
      geom_path(data = df_outline_LL_label, mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, color = "red", linewidth = 2) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = LL_2label_y, label = pop_LL_str2, parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = LL_2label_y+text_label_offset, label = "Lower Limit", parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      geom_polygon(mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, fill = "blue") +
      geom_path(data = df_outline, mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, color = "blue", linewidth = 2) +
      annotate(geom = "text", size = fontsize, x = UL_d, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      annotate(geom = "text", size = fontsize, x = UL_d, y = UL_label_y, label = pop_UL_str, parse = FALSE) +
      labs(x = "d", y = "Density", title = title_str, subtitle = subtitle_str) +
      annotate(geom = "segment", x = d, xend = d, y = 0, yend = .3*myymax, linewidth = 1) +
      annotate(geom = "text", size = fontsize, x = d, y = .35*myymax, label = sprintf("sample\nd = %1.2f", d)) +
      coord_cartesian(xlim = c(myxmin, myxmax), ylim = c(0, myymax*1.2)) +
      annotate(geom = "text", size = 10, x = d, y = myymax*1.1, label = "Step 3 Upper Limit:\nIteratively INCREASE hypothetical population d-value\nuntil the edge of the middle 95% is the sample d-value.\nNotice how the sampling distribution changes skew/shape.", hjust = 0) +
      theme_classic(24)

    output = ci_base_UL
  } else {
    ci_base_both <- ggplot(data = df_ci) +
      geom_polygon(mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, fill = "red") +
      geom_polygon(mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, fill = "blue") +
      annotate(geom = "text", size = fontsize, x = LL_dlabel, y = LL_label_y+text_offset, label = "Lower Limit", parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = UL_dlabel, y = UL_label_y+text_offset, label = "Upper Limit", parse = FALSE) +
      geom_path(data = df_outline, mapping = aes(x = LL_d_seq, y = LL_density), alpha = .5, color = "red", linewidth = 2) +
      geom_path(data = df_outline, mapping = aes(x = UL_d_seq, y = UL_density), alpha = .5, color = "blue", linewidth = 2) +
      annotate(geom = "text", size = fontsize, x = LL_d, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      annotate(geom = "text", size = fontsize, x = UL_d, y = .2*myymax, label = "Middle 95%\nof sample d-values") +
      annotate(geom = "text", size = fontsize, x = LL_d, y = LL_label_y, label = pop_LL_str, parse = FALSE) +
      annotate(geom = "text", size = fontsize, x = UL_d, y = UL_label_y, label = pop_UL_str, parse = FALSE) +
      labs(x = "d", y = "Density", title = title_str, subtitle = subtitle_str) +
      annotate(geom = "segment", x = d, xend = d, y = 0, yend = .3*myymax, linewidth = 1) +
      annotate(geom = "text",  size = 10, size = fontsize, x = d, y = .35*myymax, label = sprintf("sample\nd = %1.2f", d)) +
      coord_cartesian(xlim = c(myxmin, myxmax), ylim = c(0, myymax*1.2)) +
      theme_classic(24)

    output = ci_base_both
  }

  return(output)

}

get_drep_dist_values <- function(d, n, level, center.level, outline = FALSE) {

  level_low <- (1-level)/2
  level_high <- 1 - (1-level)/2

  df = n - 1
  tncp = convert_drep_to_t(drep = d, n = n)

  LL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = center.level)$Lower.Limit
  UL_t <- MBESS::conf.limits.nct(tncp, df, conf.level = center.level)$Upper.Limit

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

misc_function <- function() {
  center.levels <- seq(0, .95, by = .005)
  L = length(center.levels)
  number_frames = 2 * L + 30
  i = 0

  png("videotest%05d.png", width = 1920, height = 1080, res = 72)
  for (cur.center in center.levels) {
    print(plot_anim_ci_drep(d = 1, n = 20, center.level = cur.center, graph = "left"))
    i = i + 1
    print(sprintf("%1.0f%% Done creating frames.", i/number_frames*100))
  }

  for (cur.center in center.levels) {
    print(plot_anim_ci_drep(d = 1, n = 20, center.level = cur.center, graph = "right"))
    i = i + 1
    print(sprintf("%1.0f%% Done creating frames.", i/number_frames*100))
  }
  for (j in 1:30) {
    print(plot_anim_ci_drep(d = 1, n = 20, center.level = .95, graph = "both"))
    i = i + 1
    print(sprintf("%1.0f%% Done creating frames.", i/number_frames*100))
  }
  dev.off()

  png_files <- sprintf("videotest%05d.png", 1:number_frames)
  av::av_encode_video(png_files, 'output.mp4', framerate = 15)
  file.remove(png_files)
  utils::browseURL('output.mp4')

  return("")
}


