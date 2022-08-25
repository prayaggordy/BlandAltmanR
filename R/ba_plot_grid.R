#' @export
ba_plot_grid <- function(df, g1, g2, group = "",
												 include_all = T, all_lab = "All", include_void = T, title = "",
												 scales = "fixed", axes = "remove", na.rm = T, na.replace = "NA", ...) {
	rlang::arg_match0(scales, c("fixed", "free_x", "free_y", "free"))
	rlang::arg_match0(axes, c("leave", "remove_x", "remove_y", "remove"))

	if (group == "") {
		if (length(g2) == 1) {
			return(ba_plot(df, g1, g2, ...))
		} else {
			df <- df %>%
				tidyr::pivot_longer(cols = g2)
			group <- "name"
			g2 <- "value"
		}
	} else {
		if (length(g2) > 1) {
			stop("Either `group` must be empty or `g2` must be a single column name")
		}
	}

	if (na.rm) {
		df <- df %>%
			dplyr::filter(dplyr::if_all(dplyr::all_of(group), ~ !is.na(.)))
	} else {
		df <- df %>%
			dplyr::mutate(dplyr::across(dplyr::all_of(group), ~ tidyr::replace_na(., na.replace)))
	}

	if (include_all) {
		if (is.factor(df[[group]])) {
			df <- df %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(group),
																		~ forcats::fct_relabel(., ~ rep(all_lab, length(levels(df[[group]])))))) %>%
				dplyr::bind_rows(df)
		} else {
			df <- df %>%
				dplyr::mutate(dplyr::across(dplyr::all_of(group), ~ all_lab)) %>%
				dplyr::bind_rows(df)
		}
	}

	# for each fixed axis, figure out the limits and breaks on the full df if not already given
	d <- ba_plot_data(df, g1 = g1, g2 = g2, ...)
	scales_x <- scales_y <- list()
	if (!grepl("^free(_x)?$", scales)) {  # x axis fixed
		scales_x <- build_scales(d, "x", ...)
	}
	if (!grepl("^free(_y)?$", scales)) {  # y axis fixed
		scales_y <- build_scales(d, "y", ...)
	}

	opts <- list(...)
	opts[c(names(scales_x), names(scales_y))] <- NULL

	dims <- rlang::exec(ggplot2::wrap_dims, length(unique(df[[group]])), opts$nrow, opts$ncol)

	plots <- rlang::exec(
		purrr::map,
		seq(length(unique(df[[group]]))),
		plot_indiv,
		df = df, g1 = g1, g2 = g2, group = group,
		scales = scales, axes = axes, dims = dims, include_void = include_void,
		!!!scales_x, !!!scales_y, !!!opts
	)

	dims <- as.list(magrittr::set_names(dims, c("nrow", "ncol")))
	rlang::exec(patchwork::wrap_plots, plots, !!!dims)
}

plot_indiv <- function(group_val_idx, df, g1, g2, group, scales, axes, dims, include_void, ...) {
	opts <- list(...)
	list2env(opts, envir = environment())

	if (is.factor(df[[group]])) {
		group_val <- levels(df[[group]])[group_val_idx]
	} else {
		group_val <- unique(df[[group]])[group_val_idx]
	}

	d <- df %>%
		dplyr::filter(dplyr::if_any(dplyr::all_of(group), ~ .x == group_val))

	if (include_void) {
		if (d %>% dplyr::filter(dplyr::if_all(dplyr::all_of(c(g1, g2)), ~ !is.na(.))) %>% nrow == 0) {
			return(ggplot2::ggplot() +
						 	ggplot2::theme(panel.background = ggplot2::element_blank()) +
						 	ggplot2::ylab(opts[["ylab"]]))
		}
	}

	p <- ba_plot_worker(df = d, g1 = g1, g2 = g2, title = group_val, ...)

	l <- length(unique(df[[group]]))

	if (group_val_idx <= l - dims[2]) {
		p <- p +
			ggplot2::theme(axis.title.x = ggplot2::element_blank())
		if (grepl("^remove(_x)?$", axes) &
				!grepl("^free(_x)?$", scales)) {
			p <- p +
				ggplot2::theme(axis.text.x = ggplot2::element_blank(),
											 axis.ticks.x = ggplot2::element_blank())
		}
	}
	if (group_val_idx %in% setdiff(seq(l), seq(1, l, by = dims[2]))) {
		p <- p +
			ggplot2::theme(axis.title.y = ggplot2::element_blank())
		if (grepl("^remove(_y)?$", axes) &
				!grepl("^free(_y)?$", scales)) {
			p <- p +
				ggplot2::theme(axis.text.y = ggplot2::element_blank(),
											 axis.ticks.y = ggplot2::element_blank())
		}
	}

	opts[["ylim"]] <- opts[["ylim"]] %||% ggplot_build(p)$layout$panel_params[[1]]$y.range

	rlang::exec(add_marginal, p = p, !!!opts)
}

build_scales <- function(d, xy, extend = 0.05, nbreaks = 5, digits = 2, ...) {
	opts <- list(...)

	nam <- c(paste0(xy, "lim"), paste0(xy, "breaks"), paste0(xy, "labels"))

	r <- opts[[nam[1]]] %||% range(d[[list(x = "size", y = "diffs")[[xy]]]]); rdiff <- abs((r[2] - r[1]))*extend
	lims <- opts[[nam[1]]] %||% r + c(-rdiff, rdiff)
	breaks <- opts[[nam[2]]] %||% labeling::extended(dmin = r[1], dmax = r[2], m = nbreaks)
	label_text <- opts[[nam[3]]] %||% round(x = labeling::extended(dmin = r[1], dmax = r[2], m = nbreaks), digits = digits)
	list(lims, breaks, label_text) %>%
		magrittr::set_names(nam)
}

