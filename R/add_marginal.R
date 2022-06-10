add_marginal <- function(p, ylim, rel_size = 5, ...) {
	d <- ggplot(p$data, aes_string(y = quo_name(p$mapping$y))) +
		geom_density(..., color = "darkgray", fill = "darkgray") +
		theme_void() +
		scale_y_continuous(limits = ylim)

	patchwork::wrap_plots(p + theme(plot.margin = margin(r = 0)), d, nrow = 1, widths = c(rel_size, 1))
}
