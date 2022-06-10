mean_diff <- function(df, m, pb, opts) {
	list2env(opts, envir = environment())

	if (pb) {
		mean_diff_pbT(df, m, opts = opts)
	} else {
		mean_diff_pbF(df)
	}
}

mean_diff_pbT <- function(df, m, opts) {
	list2env(opts, envir = environment())

	if (CI.type == "classic"){
		df %>%
			dplyr::bind_cols(
				predict_regular(df = df, fit = m, CI.level = CI.level) %>%
					dplyr::rename(m_m = fit, m_u = upr, m_l = lwr)
			)
	} else {
		fitted <- predict_boot(df = df, fit = m, CI.level = CI.level, boot.R = boot.R)

		df %>%
			dplyr::bind_cols(
				fitted %>%
					magrittr::set_colnames(c("m_l", "m_u"))
			) %>%
			dplyr::mutate(m_m = stats::predict(m, newdata = data.frame(size = df$size)))
	}
}

mean_diff_pbF <- function(df) {
	df %>%
		dplyr::mutate(m_m = stats::t.test(df$diffs)[["estimate"]],
									m_l = stats::t.test(df$diffs)[["conf.int"]][1],
									m_u = stats::t.test(df$diffs)[["conf.int"]][2])
}
