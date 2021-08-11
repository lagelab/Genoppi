#' @title Perform (one-sample or two-sample) moderated t-test
#' @description Use (one-sample or two-sample) moderated t-test implemented in
#'   limma package to calculate protein logFC, pvalue, and FDR. See
#'   \code{?limma::lmFit} and \code{?limma::eBayes} for more details.
#' @param df data.frame containing gene, rep[0-9] (replicate logFC) columns, control[0-9] (control replicate intensity value) and
#'   sample[0-9] (sample replicate intensity value). For performing one-sample
#'   mod t-test, user must include at least 2 rep columns. And to perform
#'   two-sample mod t-test, user must include at least two sample columns and
#'   two control columns.
#' @param iter integer indicating maximum number of iterations to perform in
#'   limma::limFit().for one-sample moderated t-test
#' @param order boolean. Should the result be ordered by logFC and FDR?
#' @param two_sample boolean indicates whether to use the two-sample moderated
#'   t-test, if \code{FALSE}, use the one-sample moderated t-test. If parameter
#'   is not provided, the type of mod t-test will be selected automatically and
#'   the user is notified of the choice through a message. Two-sample mod t-test
#'   will be prioritize if there are >= 2 sample columns and >=2 control
#'   columns.
#' @return data.frame containing input df + logFC, pvalue, and FDR columns;
#'   sorted by decreasing logFC, then FDR.
#' @export
#' @examples
#' \dontrun{
#' data("example_data")
#' stats_df <- calc_mod_ttest(example_data)
#' }
#'
calc_mod_ttest <-
  function(df,
           iter = 2000,
           order = T,
           two_sample = NULL) {
    # check input
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
    rep_columns = grepl('rep[0-9]', colnames(df))
    control_columns = grepl('sample[0-9]', colnames(df))
    sample_columns = grepl('control[0-9]', colnames(df))
    if (is.null(two_sample)) {
      # check whether columns enable two-sample mod t-test, i.e. has both control
      # and sample replicate values (each with >= 2 columns)
      if (sum(sample_columns) >= 2 && sum(control_columns) >= 2) {
        # two-sample moderated t-test
        message('Type of moderated t-test not specified (defaulting to two-sample mod t-test).')
        return(calc_mod_ttest_two_sample_helper(df, iter, order))
      } else {
        # check for rep columns (check that there's >=2 rep columns)
        if (sum(rep_columns) < 2) {
          stop('data does not contain enough rep or sample and control columns! LmFit failed!')
        }
        # one-sample moderated t-test
        message('Type of moderated t-test not specified (defaulting to one-sample mod t-test).')
        return(calc_mod_ttest_one_sample_helper(df, iter, order))
      }
    } else if (two_sample) {
      # check that the required columns for two-sample mod t-test (i.e. at least
      # one sample and one control)  exists in the given df
      if (sum(sample_columns) < 2 || sum(control_columns) < 2) {
        stop(
          "trying to use two-sample moderated t-test in calc_mod_ttest but less than 2 columns of sample or control columns is provided."
        )
      }
      # explicitly using two-sample mod t-test
      message("Using two-sample mod t-test.")
      return(calc_mod_ttest_two_sample_helper(df, iter, order))
    } else if (!two_sample) {
      # check that the required columns for one-sample mod t-test (i.e. rep
      # columns) exists in the given df
      if (sum(rep_columns) < 2) {
        stop(
          "trying to use one-sample moderated t-test in calc_mod_ttest but less than 2 rep columns is provided."
        )
      }
      # explicitly using one-sample mod t-test
      message("Using one-sample mod t-test.")
      return(calc_mod_ttest_one_sample_helper(df, iter, order))
    }
  }

# helper method for calc_mod_ttest to perform two-sample moderated t-test on the
# given data
calc_mod_ttest_two_sample_helper <-
  function(df, iter = 2000, order = T) {
    # check input
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
    sample_columns = grepl('sample[0-9]', colnames(df))
    control_columns = grepl('control[0-9]', colnames(df))
    
    # check for col names
    if (all(!sample_columns) ||
        all(!control_columns))
      stop('data does not contain both sample and control columns! LmFit failed!')
    
    # create design matrix and contrast matrix for two-sample mod t-test
    sample_col_names <- colnames(df)[sample_columns]
    control_col_names <- colnames(df)[control_columns]
    
    columns <- c(sample_col_names, control_col_names)
    design <-
      cbind(Sample = as.numeric(grepl('sample[0-9]', columns)),
            Control = as.numeric(grepl('control[0-9]', columns)))
    cont.matrix <-
      limma::makeContrasts(SamplevsControl = Sample - Control, levels = design)
    
    # two-sample mod t-test
    fit <-
      limma::lmFit(df[, columns], design, method = "robust", maxit = iter)
    cont.fit <- limma::contrasts.fit(fit, cont.matrix)
    bayes <- limma::eBayes(cont.fit)
    mod.ttest <- limma::topTable(bayes,
                                 number = nrow(bayes),
                                 sort.by = 'none')
    colnames(mod.ttest)[colnames(mod.ttest) == "P.Value"] <- "pvalue"
    colnames(mod.ttest)[colnames(mod.ttest) == "adj.P.Val"] <- "FDR"
    
    # return data frame with test results: gene, sample1, ..., control1, ...,
    # logFC, pvalue, FDR
    result <- cbind(df, mod.ttest[, c("logFC", "pvalue", "FDR")])
    
    #check order
    if (order)
      result <- result[with(result, order(-logFC, FDR)), ]
    
    return(result)
  }

# helper function used in calc_mod_ttest.R to carry out one-sample mod t-test
calc_mod_ttest_one_sample_helper <-
  function(df, iter = 2000, order = T) {
    # check input
    stopifnot(is.data.frame(df))
    stopifnot(nrow(df) > 0)
    columns = grepl('rep[0-9]', colnames(df))
    
    # check for rep names
    if (all(!columns))
      stop('data does not contain rep columns! LmFit failed!')
    
    # moderated t-test
    myfit <- limma::lmFit(df[, columns], method = "robust", maxit = iter)
    myfit <- limma::eBayes(myfit)
    modtest <-
      limma::topTable(myfit, number = nrow(myfit), sort.by = 'none')
    colnames(modtest)[4:5] <- c("pvalue", "FDR")
    
    # return data frame with test results: gene, rep1, rep2, ..., logFC, pvalue,
    # FDR
    result <- data.frame(cbind(df, modtest[, -c(2, 3, 6)]))
    
    # order columns
    if (order)
      result <- result[with(result, order(-logFC, FDR)), ]
    
    return(result)
  }
