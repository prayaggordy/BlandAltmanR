ba_plot <- function(df, measure, exts, ...) {
	p <- ba_plot_worker(df = df, measure = measure, exts = exts, ...)

	opts <- list(...)
	if (is.null(opts$ylim)) {
		opts$ylim <- ggplot_build(p)$layout$panel_params[[1]]$y.range
	}

	rlang::exec(add_marginal, p = p, !!!opts)
}

ba_plot_worker <- function(df, measure, exts,
													 point_size = 1, fit_size = 1, ci_size = 1,
													 fit_color = "red", LOA_color = "darkgray",
													 ...) {

	opts <- list(...)

	scale_x <- list(limits = opts$xlim, breaks = opts$xbreaks, labels = opts$xlabels) %>% null.omit()
	scale_y <- list(limits = opts$ylim, breaks = opts$ybreaks, labels = opts$ylabels) %>% null.omit()
	all_labs <- list(xlab = opts$xlab, ylab = opts$ylab, title = opts$title) %>% null.omit()

	d <- ba_plot_data(df = df, measure = measure, exts = exts, ...) %>%
		tidyr::pivot_longer(-c(size, diffs),
												names_to = "line") %>%
		dplyr::mutate(color = as.character(stringr::str_starts(line, "m")),
									linetype = as.character(stringr::str_ends(line, "m")))

	ggplot(d, aes(x = size, y = diffs)) +
		geom_line(aes(group = line, color = color, linetype = linetype, size = linetype, y = value), show.legend = F) +
		geom_point(size = point_size) +
		scale_color_manual(values = c("TRUE" = fit_color, "FALSE" = LOA_color)) +
		scale_linetype_manual(values = c("TRUE" = 1, "FALSE" = 2)) +
		scale_size_manual(values = c("TRUE" = fit_size, "FALSE" = ci_size)) +
		rlang::exec(scale_x_continuous, !!!scale_x) +
		rlang::exec(scale_y_continuous, !!!scale_y) +
		rlang::exec(labs, !!!all_labs)
}
