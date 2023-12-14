#' @title Impute missing values in proteomics data
#' @description replace missing values (NAs) in each sample (i.e. column) by
#' sampling from a normal distribution with mean of x - y*shift and SD of y*width,
#' where x and y are the mean and SD of the observed values in the sample.
#' @param df data.frame containing columns with numeric values (e.g. normalized,
#' log2-transformed protein intensities) and NAs
#' @param shift numeric value
#' @param width numeric value
#' @return data.frame containing same columns as input df (with NAs replaceD), and
#' corresponding "_imp" columns indicating if values were imputed (TRUE) vs. observed (FALSE).
#' @export

impute_na <- function(df,shift=1.8,width=0.3) {

  if (!all(sapply(df,is.numeric))) { stop('all columns in df must be numeric') }
  if (!is.numeric(shift)) { stop('shift must be numeric') }
  if (!is.numeric(width)) { stop('width must be numeric') } 

  impDf <- df

  for (col in names(df)) {
    # add imp flag columns
    impCol <- paste(col,'_imp',sep='')
    impDf[impCol] <- is.na(df[col])

    # impute missing values
    mu <- mean(df[,col],na.rm=T) - sd(df[,col],na.rm=T)*shift
    sd <- sd(df[,col],na.rm=T)*width
    impVals <- rnorm(nrow(df),mean=mu,sd=sd)
    impDf[col][impDf[,impCol]] <- impVals[impDf[,impCol]]
  }

  return(impDf)
}

