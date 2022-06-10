ba_plot_grid <- function(df, measure, exts, group,
												 include_all = T, all_lab = "All", title = "",
												 scales = "fixed", axes = "remove", ...) {
	rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
	rlang::arg_match0(axes, c("leave", "remove_x", "remove_y", "remove"))

	if (include_all) {
		df <- df %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(group), ~ all_lab)) %>%
			dplyr::bind_rows(df)
	}

	# for each fixed axis, figure out the limits and breaks on the full df if not already given
	d <- ba_plot_data(df, measure = measure, exts = exts, ...)
	scales_x <- scales_y <- list()
	if (!grepl("^free(_x)?$", scales)) {  # x axis fixed
		scales_x <- build_scales(d, "x", ...)
	}
	if (!grepl("^free(_y)?$", scales)) {  # y axis fixed
		scales_y <- build_scales(d, "y", ...)
	}

	plots <- rlang::exec(
		purrr::map,
		seq(length(unique(df[[group]]))),
		plot_indiv,
		df = df, measure = measure, exts = exts, group = group,
		scales = scales, axes = axes,
		!!!scales_x, !!!scales_y,
		...
	)

	patchwork::wrap_plots(plots)
}

plot_indiv <- function(group_val_idx, df, measure, exts, group, scales, axes, ...) {
	opts <- list(...)
	list2env(opts, envir = environment())

	group_val <- unique(df[[group]])[group_val_idx]

	p <- ba_plot_worker(df = df %>%
												dplyr::filter(dplyr::if_any(dplyr::all_of(group),
																										~ .x == group_val)),
											measure = measure, exts = exts, title = group_val, ...)

	if (!is.null(opts$theme_fn)) {
		p <- p +
			rlang::exec(opts$theme_fn)
	}

	l <- length(unique(df[[group]]))
	dims <- ggplot2::wrap_dims(l)

	if (group_val_idx <= l - dims[2]) {
		p <- p +
			ggplot2::theme(axis.title.x = element_blank())
		if (grepl("^remove(_x)?$", axes) &
				!grepl("^free(_x)?$", scales)) {
			p <- p +
				ggplot2::theme(axis.text.x = element_blank(),
											 axis.ticks.x = element_blank())
		}
	}
	if (group_val_idx %in% setdiff(seq(l), seq(1, l, by = dims[2]))) {
		p <- p +
			ggplot2::theme(axis.title.y = element_blank())
		if (grepl("^remove(_y)?$", axes) &
				!grepl("^free(_y)?$", scales)) {
			p <- p +
				ggplot2::theme(axis.text.y = element_blank(),
											 axis.ticks.y = element_blank())
		}
	}

	opts[["ylim"]] <- opts[["ylim"]] %||% ggplot_build(p)$layout$panel_params[[1]]$y.range

	rlang::exec(add_marginal, p = p, !!!opts)
}

build_scales <- function(d, xy, extend = 0.05, nbreaks = 5, digits = 2, ...) {
	opts <- list(...)

	nam <- c(paste0(xy, "lim"), paste0(xy, "breaks"), paste0(xy, "labels"))

	r <- range(d[[list(x = "size", y = "diffs")[[xy]]]]); rdiff <- abs((r[2] - r[1]))*extend
	lims <- opts[[nam[1]]] %||% r + c(-rdiff, rdiff)
	breaks <- opts[[nam[2]]] %||% labeling::extended(dmin = r[1], dmax = r[2], m = nbreaks)
	label_text <- opts[[nam[3]]] %||% round(x = labeling::extended(dmin = r[1], dmax = r[2], m = nbreaks), digits = digits)
	list(lims, breaks, label_text) %>%
		magrittr::set_names(nam)
}
