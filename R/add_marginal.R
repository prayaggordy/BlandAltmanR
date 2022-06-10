add_marginal <- function(p, ylim, rel_size = 5, ...) {
	d <- ggplot2::ggplot(p$data, ggplot2::aes_string(y = ggplot2::quo_name(p$mapping$y))) +
		ggplot2::geom_density(..., color = "darkgray", fill = "darkgray") +
		ggplot2::theme_void() +
		ggplot2::scale_y_continuous(limits = ylim)

	patchwork::wrap_plots(p + ggplot2::theme(plot.margin = ggplot2::margin(r = 0)), d, nrow = 1, widths = c(rel_size, 1))
}
