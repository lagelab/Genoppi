#' @title enumerate replicate combinations
#' @description enumerates unique replicate comparisons, extract rep-rep,
#'   sample-sample, control-control, and sample-control combinations
#' @param df the data frame from which replicate combinations are extracted
#' @family misc
#' @export

enumerate_replicate_combinations <- function(df) {
  reps = colnames(df)[grepl('^rep[0-9]+$',colnames(df))]
  samples = colnames(df)[grepl('^sample[0-9]+$',colnames(df))]
  controls = colnames(df)[grepl('^control[0-9]+$',colnames(df))]
  combinations = c(
    outer(reps, reps, paste, sep = ".")[upper.tri(outer(reps,
                                                        reps,
                                                        paste,
                                                        sep = "."))],
    as.vector(outer(samples, controls, paste, sep = ".")),
    outer(samples, samples, paste, sep = ".")[upper.tri(outer(samples,
                                                              samples,
                                                              paste,
                                                              sep = "."))],
    outer(controls, controls, paste, sep = ".")[upper.tri(outer(controls,
                                                                controls,
                                                                paste,
                                                                sep = "."))]
  )
  return(combinations)
}
