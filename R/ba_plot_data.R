#' @export
ba_plot_data <- function(df, g1, g2, ...) {
	opts <- full_opts(df, g1, g2, ...)
	list2env(opts, envir = environment())

	d <- df %>%
		dplyr::filter(dplyr::if_all(c(g1, g2), ~ !is.na(.)))

	if (nrow(df) - nrow(d) > 0) {
		warning("Removed ", nrow(df) - nrow(d), " row(s) containing missing values in ", g1, " or ", g2)
	}

	classes <- c(any(class(df[[g1]]) == "POSIXt"), any(class(df[[g2]]) == "POSIXt"))

	if (sum(classes) == 1) {
		stop("Both or neither of `g1`, `g2` must be POSIXt")
	} else if (sum(classes) == 2) {
		d <- d %>%
			dplyr::mutate(dplyr::across(c(g1, g2), interval_mins))
	}

	ba <- calculate_points(
		g1 = d[[g1]],
		g2 = d[[g2]],
		xaxis = xaxis
	)

	ba_log <- calculate_log_points(
		g1 = d[[g1]],
		g2 = d[[g2]],
		xaxis = xaxis
	)

	if (sum(classes) == 2) {
		ba$size <- ba$size %% (24*60)
		ba_log$size <- ba_log$size %% (24*60)
	}

	ret <- calculate_lines(df = ba, df_log = ba_log, opts)
	attr(ret, "plot_details") <- c(attr(ret, "plot_details"), opts)

	ret
}

full_opts <- function(df, g1, g2, ...) {
	opts <- utils::modifyList(list(xaxis = "mean", log_transf = F,
																 CI.type = "classic", CI.level = 0.95,
																 boot.type = "basic", boot.R = 1000),
														list(...))

	rlang::arg_match0(opts$xaxis, c("reference", "mean"))
	rlang::arg_match0(opts$CI.type, c("classic", "boot"))
	rlang::arg_match0(opts$boot.type, c("norm", "basic", "stud", "perc", "bca", "all"))
	assertthat::assert_that(all(length(g1) == 1, length(g2) == 1))
	assertthat::assert_that(opts$CI.level > 0 & opts$CI.level < 1)
	assertthat::assert_that(opts$boot.R %% 1 == 0)

	opts
}

calculate_points <- function(g1, g2, xaxis) {
	if (xaxis == "mean") {
		data.frame(size = (g1 + g2)/2, diffs = g2 - g1)  # rename size to x, diffs to y
	} else {
		data.frame(size = g2, diffs = g2 - g1)
	}
}

calculate_log_points <- function(g1, g2, xaxis, const = 1e-4) {
	calculate_points(g1 = log(abs(g1) + const)*sign(g1),
									 g2 = log(abs(g2) + const)*sign(g1),
									 xaxis = xaxis)
}

calculate_lines <- function(df, df_log, opts) {
	m <- stats::lm(diffs ~ size, df); mRes <- stats::lm(abs(stats::resid(stats::lm(diffs ~ size))) ~ size, df)  # can't otherwise extract full mRes formula

	pb <- check_b1(fit = m, df = df, opts = opts)
	hs <- check_b1(fit = mRes, df = df, opts = opts)

	ret <- mean_diff(df = df, m = m, pb = pb, opts) %>%
		LOAs(df = ., df_log = df_log, m = m, mRes = mRes, pb = pb, hs = hs, opts = opts) %>%
		dplyr::select(size, diffs,
									m_m, m_u, m_l,
									u_m, u_u, u_l,
									l_m, l_u, l_l)

	attr(ret, "plot_details") <- c(attr(ret, "plot_details"), list(pb = pb, hs = hs))

	ret
}

check_b1 <- function(fit, df, opts) {
	list2env(opts, envir = environment())

	if (CI.type == "classic") {
		ci <- stats::confint(fit, level = CI.level)[2,]
	} else if (CI.type == "boot") {
		ci <- boot::boot.ci(boot::boot(data = df, statistic = boot_b1, formula = stats::formula(fit), R = boot.R),
												type = boot.type, conf = CI.level)[[boot.type]][4:5]
	} else {
		stop("Error: CI.type argument must be either 'classic' or 'boot'")
	}

	prod(ci) > 0  # i.e., statistically significant non-zero beta1
}

predict_regular <- function(df, fit, CI.level) {
	stats::predict(fit, newdata = data.frame(size = df$size), interval="confidence", level = CI.level) %>%
		tibble::as_tibble()
}

predict_boot <- function(df, fit, CI.level, boot.R) {
	fitted <- t(replicate(boot.R, boot_lm(df = df, formula = stats::formula(fit))))

	apply(fitted, 2, stats::quantile, probs = c((1 - CI.level)/2, CI.level + (1 - CI.level)/2)) %>%
		t() %>%
		tibble::as_tibble()
}

boot_b1 <- function(df, formula, indices) {
	stats::coef(stats::lm(formula, data = df[indices,]))[2]
}

boot_lm <- function(df, formula) {
	indices <- sample(1:nrow(df), replace = T)
	stats::predict(stats::lm(formula, data = df[indices,]), newdata = data.frame(size = df$size))
}
