LOAs <- function(df, df_log, m, mRes, pb, hs, opts) {
	list2env(opts, envir = environment())

	if (log_transf) {
		LOA_log(df = df, df_log = df_log, m = m, pb = pb, opts = opts)
	} else if (hs) {
		LOA_hsT(df = df, mRes = mRes, opts = opts)
	} else {
		LOA_hsF(df = df, m = m, pb = pb, opts = opts)
	}
}

LOA_hsT <- function(df, mRes, opts) {
	list2env(opts, envir = environment())

	if (CI.type == "classic"){
		df %>%
			dplyr::bind_cols(
				predict_regular(df = df, fit = mRes, CI.level = CI.level) %>%
					dplyr::rename(u_m = fit, u_u = upr, u_l = lwr)
			)  %>%
			dplyr::bind_cols(
				predict_regular(df = df, fit = mRes, CI.level = CI.level) %>%
					dplyr::rename(l_m = fit, l_u = upr, l_l = lwr)
			) %>%
			dplyr::mutate(dplyr::across(dplyr::starts_with("u"), ~ m_m + 2.46*.),
										dplyr::across(dplyr::starts_with("l"), ~ m_m - 2.46*.))
	} else {
		fitted <- predict_boot(df = df, fit = mRes, CI.level = CI.level, boot.R = boot.R)

		df %>%
			dplyr::bind_cols(
				fitted %>%
					magrittr::set_colnames(c("u_l", "u_u"))
			) %>%
			dplyr::bind_cols(
				fitted %>%
					magrittr::set_colnames(c("l_l", "l_u"))
			) %>%
			dplyr::mutate(u_m = predict(mRes, newdata = data.frame(size = df$size)),
										l_m = predict(mRes, newdata = data.frame(size = df$size)),
										dplyr::across(dplyr::starts_with("u"), ~ m_m + 2.46*.),
										dplyr::across(dplyr::starts_with("l"), ~ m_m - 2.46*.))
	}
}

LOA_hsF <- function(df, m, pb, opts) {
	list2env(opts, envir = environment())

	if (pb) {
		v <- 1.96*sd(resid(m))
		df %>%
			dplyr::mutate(u_m = m_m + v, l_m = m_m - v,
										u_l = m_l + v, l_l = m_l - v,
										u_u = m_u + v, l_u = m_u - v)
	} else {
		v <- 1.96*sd(df$diffs)
		if (CI.type == "classic") {
			t1 <- qt((1 - CI.level)/2, df = nrow(df) - 1)*sqrt(sd(df$diffs)^2*3/nrow(df))
			t2 <- qt((CI.level + 1)/2, df = nrow(df) - 1)*sqrt(sd(df$diffs)^2*3/nrow(df))

			df %>%
				dplyr::mutate(u_m = m_m + v, l_m = m_m - v,
											u_l = u_m + t1, l_l = l_m + t1,
											u_u = u_m + t2, l_u = l_m + t2)
		} else {
			l <- boot.ci(boot(df$diffs - 1.96*sd(df$diffs),
												function(dat, idx) {mean(dat[idx], na.rm = TRUE)},
												R = boot.R),
									 type = boot.type, conf = CI.level)[[4]]
			u <- boot.ci(boot(df$diffs + 1.96*sd(df$diffs),
												function(dat, idx) {mean(dat[idx], na.rm = TRUE)},
												R = boot.R),
									 type = boot.type, conf = CI.level)[[4]]
			df %>%
				dplyr::mutate(u_m = m_m + v, l_m = m_m - v,
											u_l = u[4], l_l = l[4],
											u_u = u[5], l_u = l[5])
		}
	}
}

LOA_log <- function(df, df_log, m, pb, opts) {
	list2env(opts, envir = environment())

	mRes_log <- lm(abs(resid(m)) ~ size, df_log)
	hs <- check_b1(fit = mRes_log, df = df_log, opts = opts)

	if (CI.type == "classic") {
		t1 <- qt((1 - CI.level)/2, df = nrow(df) - 1)
		t2 <- qt((CI.level + 1)/2, df = nrow(df) - 1)

		ci_dist_u <- 2 * (exp(1.96 * sd(df_log$diffs) + t2 * sqrt(sd(df_log$diffs)^2 * 3/nrow(df))) - 1) / (exp(1.96*sd(df_log$diffs) + t2 * sqrt(sd(df_log$diffs)^2 * 3/nrow(df))) + 1)
		ci_dist_l <- 2 * (exp(1.96 * sd(df_log$diffs) + t1 * sqrt(sd(df_log$diffs)^2 * 3/nrow(df))) - 1) / (exp(1.96*sd(df_log$diffs) + t1 * sqrt(sd(df_log$diffs)^2 * 3/nrow(df))) + 1)
	} else {
		ci_dist_u <- boot::boot.ci(boot::boot(df_log$diffs, function(dat,idx) ANTILOGslope(dat[idx]), R = boot.R),
															 type = boot.type, conf = CI.level)[[4]][4]
		ci_dist_l <- boot::boot.ci(boot::boot(df_log$diffs, function(dat,idx) ANTILOGslope(dat[idx]), R = boot.R),
															 type = boot.type, conf = CI.level)[[4]][5]
	}

	df %>%
		dplyr::mutate(u_m = m_m + size * ANTILOGslope(df_log$diffs),
									u_u = m_m + size * ci_dist_u,
									u_l = m_m + size * ci_dist_l,
									l_m = m_m - size * ANTILOGslope(df_log$diffs),
									l_u = m_m - size * ci_dist_u,
									l_l = m_m - size * ci_dist_l)
}

ANTILOGslope <- function(x) {
	2 * (exp(1.96 * sd(x)) - 1) / (exp(1.96*sd(x)) + 1)
}
