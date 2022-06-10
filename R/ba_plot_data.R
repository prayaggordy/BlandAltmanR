ba_plot_data <- function(df, measure, exts, ...) {
	opts <- full_opts(df, measure, exts, ...)
	list2env(opts, envir = environment())

	ba <- calculate_points(
		g1 = df %>% dplyr::pull(paste0(measure, exts[1])),
		g2 = df %>% dplyr::pull(paste0(measure, exts[2])),
		xaxis = xaxis
	)

	ba_log <- calculate_log_points(
		g1 = df %>% dplyr::pull(paste0(measure, exts[1])),
		g2 = df %>% dplyr::pull(paste0(measure, exts[2])),
		xaxis = xaxis
	)

	calculate_lines(df = ba, df_log = ba_log, opts)
}

full_opts <- function(df, measure, exts, ...) {
	opts <- modifyList(list(exts = c("_device", "_ref"), xaxis = "mean", log_transf = F,
													CI.type = "classic", CI.level = 0.95,
													boot.type = "basic", boot.R = 1000),
										 c(list(exts = exts), list(...)))

	rlang::arg_match0(opts$xaxis, c("reference", "mean"))
	rlang::arg_match0(opts$CI.type, c("classic", "boot"))
	rlang::arg_match0(opts$boot.type, c("norm", "basic", "stud", "perc", "bca", "all"))
	assertthat::assert_that(length(measure) == 1)
	assertthat::assert_that(length(opts$exts) == 2)
	assertthat::assert_that(opts$CI.level > 0 & opts$CI.level < 1)
	assertthat::assert_that(opts$boot.R %% 1 == 0)

	opts
}

calculate_points <- function(g1, g2, xaxis) {
	if (xaxis == "mean") {
		data.frame(size = (g1 + g2)/2, diffs = g1 - g2)  # rename size to x, diffs to y
	} else if (xaxis == "reference") {
		data.frame(size = g2, diffs = g1 - g2)
	} else {
		stop("Error: xaxis argument must be either 'reference' or 'mean'")
	}
}

calculate_log_points <- function(g1, g2, xaxis, const = 1e-4) {
	calculate_points(g1 = log(g1 + const),
									 g2 = log(g2 + const),
									 xaxis = xaxis)
}

calculate_lines <- function(df, df_log, opts) {
	m <- lm(diffs ~ size, df); mRes <- lm(abs(resid(lm(diffs ~ size))) ~ size, df)  # can't otherwise extract full mRes formula

	pb <- check_b1(fit = m, df = df, opts = opts)
	hs <- check_b1(fit = mRes, df = df, opts = opts)

	mean_diff(df = df, m = m, pb = pb, opts) %>%
		LOAs(df = ., df_log = df_log, m = m, mRes = mRes, pb = pb, hs = hs, opts = opts) %>%
		dplyr::select(size, diffs,
									m_m, m_u, m_l,
									u_m, u_u, u_l,
									l_m, l_u, l_l)
}

check_b1 <- function(fit, df, opts) {
	list2env(opts, envir = environment())

	if (CI.type == "classic") {
		ci <- confint(fit, level = CI.level)[2,]
	} else if (CI.type == "boot") {
		ci <- boot::boot.ci(boot::boot(data = df, statistic = boot_b1, formula = formula(fit), R = boot.R),
												type = boot.type, conf = CI.level)[[boot.type]][4:5]
	} else {
		stop("Error: CI.type argument must be either 'classic' or 'boot'")
	}

	prod(ci) > 0  # i.e., statistically significant non-zero beta1
}

predict_regular <- function(df, fit, CI.level) {
	predict(fit, newdata = data.frame(size = df$size), interval="confidence", level = CI.level) %>%
		tibble::as_tibble()
}

predict_boot <- function(df, fit, CI.level, boot.R) {
	fitted <- t(replicate(boot.R, boot_lm(df = df, formula = formula(fit))))

	apply(fitted, 2, quantile, probs = c((1 - CI.level)/2, CI.level + (1 - CI.level)/2)) %>%
		t() %>%
		tibble::as_tibble()
}

boot_b1 <- function(df, formula, indices) {
	coef(lm(formula, data = df[indices,]))[2]
}

boot_lm <- function(df, formula) {
	indices <- sample(1:nrow(df), replace = T)
	predict(lm(formula, data = df[indices,]), newdata = data.frame(size = df$size))
}