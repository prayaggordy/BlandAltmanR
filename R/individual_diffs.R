individual_diffs <- function(df, measures = NULL, names_sep = "_") {
	df %>%
		tidyr::pivot_longer(cols = dplyr::starts_with(measures),
												names_to = c("measure", ".value"),
												names_sep = names_sep)
}
