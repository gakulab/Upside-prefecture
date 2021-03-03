#' ggKobe
#'
#' \code{ggKobe} produces a kobe plot in the manner of Costello
#' et al 2016, using user supplied data
#' @param dat dataframe containing required stuff
#' @param xvar thing you want plotted on the x axis
#' @param yvar thing you want plotted on the y axis
#' @param plot_density logical indicating whether background density should be plotted
#' @param color_name variable to color code dots by
#' @param plot_panel_colors T or F to plot background panel colors instead of density
#' @param summary_points T or F to plot points for the median and MSY-weighted geometric mean fishery, plotted as a green triangle and green square, respectively
#' @return a kobe plot ggplot
#' @export
#'
#' @examples
#' ggKobe(filter(ProjectionData, Year == 2012), xvar = 'BvBmsy', yvar = 'FvFmsy')
ggKobe <- function (dat, xvar = "BvBmsy",
                    yvar = "FvFmsy",
                    plot_density = T,
                    color_name = "RAM",
                    plot_panel_colors = F,
                    summary_points = T,
                    xlimit = 2.5,
                    ylimit = 4)
{
  dat <- ungroup(dat)
  orig_names = colnames(dat)
  xvar_name = xvar
  yvar_name = yvar
  dat = dat %>%
    rename_(xvar = xvar, yvar = yvar) %>%
    mutate(yvar = pmin(ylimit,yvar), xvar = pmin(xlimit, xvar))

  if ("Dbase" %in% colnames(dat) == F) {
    dat$Dbase = "Unknown"
  }

  if ("IdOrig" %in% colnames(dat)) {
    dat$id = dat$IdOrig
  }

  dat$is_ram <- dat$Dbase == color_name

  summary_dat <- dat %>%
    ungroup() %>%
    mutate(has_all = is.na(MSY) == F & is.na(xvar) == F & is.na(yvar) == F,
           has_both_y = is.na(MSY) == F & is.na(yvar) == F) %>%
    filter(has_all == T) %>%
    summarise(median_x = median(xvar,na.rm = T),
              median_y = median(yvar, na.rm = T),
              geom_mean_msy_weight_x = exp(sum(MSY * log(xvar), na.rm = T)/sum(MSY, na.rm = T)),
              geom_mean_msy_weight_y = exp(sum(MSY * log(yvar + 0.001), na.rm = T)/sum(MSY, na.rm = T))) %>%
    mutate(is_ram = NA, MSY = NA)

  kobe <- dat %>%
    ggplot(aes(xvar, yvar))

  if (plot_density == T & plot_panel_colors == F) {
    kobe = kobe +
      stat_density_2d(aes(fill = ..density..), geom = "tile", n = 100, alpha = 0.8, contour = F) +
      scale_fill_gradient2(guide = F, low = "skyblue1",mid = "white", high = "khaki1", midpoint = 0.5)
  }
  if (plot_panel_colors == T) {
    rect_dat = data.frame(panel = c("bottom_left", "top_right",
                                    "bottom_right", "top_left"),
                          x_min = c(-Inf, 1, 1, -Inf),
                          x_max = c(1, Inf, Inf, 1), y_min = c(-Inf,1, -Inf, 1),
                          y_max = c(1, Inf, 1, Inf))

    kobe <- kobe +
      geom_rect(data = rect_dat, aes(xmin = x_min, ymin = y_min, xmax = x_max, ymax = y_max, fill = panel),
                inherit.aes = F, alpha = 0.5) +
      scale_fill_manual(values = c("lightgoldenrod1","limegreen", "#FF4500", "lightgoldenrod1"), guide = F)
  }
  kobe = kobe +
    geom_hline(aes(yintercept = 1), linetype = "longdash") +
    geom_vline(aes(xintercept = 1), linetype = "longdash") +
    geom_point(aes(xvar, yvar, color = is_ram)) + scale_color_manual(guide = F,
                                                                  values = c("#383737", "red"))
  if(summary_points == TRUE){
    kobe = kobe +
      geom_point(data = summary_dat, aes(median_x, median_y), shape = 24, size = 6, fill = "steelblue2", alpha = 0.75) +
      geom_point(data = summary_dat, aes(geom_mean_msy_weight_x, geom_mean_msy_weight_y), shape = 22, size = 6, fill = "steelblue2",alpha = 0.75)
  }

  kobe = kobe +
    scale_size_continuous(guide = F) +
    scale_alpha_continuous(guide = F, range = c(0.9, 1)) +
    xlab(xvar_name) +
    ylab(yvar_name) +
    theme_classic() +
    theme(text = element_text(size = 16)) +
    scale_x_continuous(limits = c(-1, 4), breaks = seq(-1,4, by = 0.5), labels = c(seq(-1, 2, by = 0.5), expression(phantom(x) >= 2.5), seq(3,4, by = 0.5))) +
    scale_y_continuous(limits = c(-1,6), breaks = seq(-1, 6, by = 0.5), labels = c(seq(-1,3.5, by = 0.5), expression(phantom(x) >= 4), seq(4.5,6, by = 0.5))) +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0,4))

  return(kobe)
}
