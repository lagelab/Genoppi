# provides a data frame containing statistics column (logFC, pvalue, FDR)
dataServer <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}

# provides a data frame containing significant column
sigificanceServer <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}

# provides a vector of the columns used for calculating enrichment statistics
#   mod_ttest_columns
#     initial value: NULL
#     special value: list() (means that no data columns e.g., sample1/rep1/control1 is provided)
#     example values: c("sample1", "control1", "sample2", "control2")
columnsValues <- function(id) {
  moduleServer(id, function(input, output, session){
    return(reactiveValues())})
}
dataPathServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}

dataFrameServer <- function(id, dataPathServer) {
  moduleServer(id, function(input, output, session) {
    return(
      eventReactive(dataPathServer(), {
        validate(need(!is.null(dataPathServer()), ''))
        read_input(dataPathServer(), sep = '\t')
      })
    )
  })
}

###### ERROR checking for input ######
errorValues <- function(id) {
  moduleServer(id, function(input, output, session) {
    return(reactiveValues())
  })
}

inputErrorServer <- function(id, dataFrameServer, errorValues){
  if (!is.reactive(dataFrameServer)) {
    stop("dataFrameServer passed to inputErrorServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    observeEvent(dataFrameServer()$data,{
      req(dataFrameServer()$data)
      d <- dataFrameServer()$data
      pulldown <- dataFrameServer() # change name after testing code
      errorValues$columns_check <- get_shiny_errors(d)
      allowed = unlist(pulldown$format$allowed[unlist(pulldown$format$check)])
      if (length(allowed)==0) {
        allowed_vec = !as.logical(1:ncol(pulldown$data))
      } else {
        allowed_cols = lapply(allowed, function(x) grepl(x, colnames(pulldown$data)))
        allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
      }
      accepted = colnames(pulldown$data)[allowed_vec]
      discarded = colnames(pulldown$data)[!allowed_vec]
      
      # check for NAs in rows
      na_rows = sum(apply(pulldown$data, 1, function(x) any(is.na(x))))
      na_cols = apply(pulldown$data, 2, function(x) any(is.na(x)))
      
      # check if p-values are already -log10 transformed
      check_log_pvalues <- any(pulldown$data$pvalue > 1)
      
      # pre-rendered messages
      msg1 = paste0(
        genoppi:::bold('Error:'),' None of the inputted column names are allowed')
      msg2 = paste0(
        genoppi:::bold('Warning:'),' only ', length(accepted),'/',length(allowed_vec),
        ' input column names were accepted.')
      msg3 = paste0(
        'The following column names were invalid and discarded: ', 
        genoppi:::italics(paste0(discarded, collapse = ', ')),'.')
      msg4 = paste0(
        'See supplementary protocol for a description of allowed data inputs.')
      msg5 = paste0(
        genoppi:::bold('Warning: '), 'NA(s) were found in ', na_rows,
        ' row(s). Check column(s): ', 
        paste(names(na_cols)[na_cols], collapse = ', '))
      msg6 = paste0(
        genoppi:::bold('Warning: '),
        'It looks like you have already -log10 transformed your p-values. 
        Please, use raw p-values to accurately display volcano plots.')
      
      # no valid cols
      msg = ''
      if (length(accepted) == 0){
        msg = paste(msg, msg1, msg4)
        #return(HTML(paste(msg1, msg4)))
        # enough valid but some invalid
      } else if (length(accepted) != length(allowed_vec)){
        msg = paste(msg, msg2, msg3, msg4)
        #return(HTML(paste(msg2, msg3, msg4)))
      } 
      if (na_rows > 0){msg = paste(msg, msg5)}
      if (check_log_pvalues){msg = paste(msg, msg6)}
      # if (msg != '') {
      #   errorValues$rendered_message <- HTML(msg)
      # } else {
      #   errorValues$rendered_message <- NULL
      # }
      errorValues$input_errors <- msg
    })
  })
}

accessionMapErrorServer <- function(id, mapAccessionToGeneServer, errorValues) {
  moduleServer(id, function(input, output, session) {
    observeEvent(mapAccessionToGeneServer()$data, {
      data <- mapAccessionToGeneServer()$data
      req(data)
      # if gene column is provided, parse it to check for synonyms
      fmt <-mapAccessionToGeneServer()$format$check
      if (fmt$gene_rep |fmt$gene_sample_control |fmt$gene_signif) {
        synonyms = strsplit(data$gene, split = '(\\;)|(\\|)')
        synonyms_bool = unlist(lapply(synonyms, length)) > 1
        synonym_example = data$gene[synonyms_bool][1]
        # messages
        if (sum(synonyms_bool) > 0) {
          errorValues$gene_symbol_error <- paste0(
            genoppi:::bold('Note:  '), sum(synonyms_bool),
            ' rows contain synonyms in "gene" column, e.g. gene "',synonym_example,
            '". This column should only contain a single gene-name.')
        }
      }
      # if no gene column is provided, check correctness of mapping accession_number(if provided) to gene
      else {
        # pulldown_mapping <- mapAccessionToGeneServer()$data
        failed = data$accession_number[is.na(data$gene)]
        absolute = paste0(length(failed),'/',nrow(data))
        fraction = paste0(format(100*length(failed)/nrow(data), digits = 3),'%')
        
        # messages
        msg0 = genoppi:::bold(paste(
          'ERROR: ', absolute, ' (',fraction,
          ') accesion_numbers were not mapped to a genes. 
          The App may crash during Integrated Plotting!'))
        msg1 = paste0(
          genoppi:::bold('Warning:'), absolute, ' (',fraction,
          ') accesion_number(s) were not mapped to a gene(s).')
        msg2 = paste0(
          'The following accesion_number(s) were not mapped:', 
          genoppi:::italics(paste0(failed,collapse=', ')),'.')
        msg3 = paste0(
          'These will be ignored in downstream analysis. 
          To include, manually specify the entry in a seperate "gene" (HGNC) column.')
        msg4 = paste0('Are you using human accession numbers?')
        
        if (length(failed) > 0){
          # if more than 99% are unmapped give a warning:
          if (length(failed)/nrow(data) > 0.99){
            errorValues$mapping_error <- HTML(paste(msg0, msg4, msg2))
          } else { # otherwise, print out failed accession mapping
            errorValues$mapping_error <- HTML(paste(msg1, msg2, msg3))
          }
        }
      }
    })
  })
}
######################################

# extractColumnsServer <- function(id, dataFrameServer) {
#   moduleServer(id, function(input, output, session) {
#     return(
#       eventReactive(dataFrameServer()$data, {
#         reps = colnames(dataFrameServer()$data)[grepl('^rep[0-9]+$',colnames(dataFrameServer()$data))]
#         samples = colnames(dataFrameServer()$data)[grepl('^sample[0-9]+$',colnames(dataFrameServer()$data))]
#         controls = colnames(dataFrameServer()$data)[grepl('^control[0-9]+$',colnames(dataFrameServer()$data))]
#         c(reps, samples, controls)
#       })
#     )
#   })
# }

mapAccessionToGeneServer <- function(id, dataFrameServer, errorValues) {
  moduleServer(id, function(input, output, session) {
    return(
      eventReactive(dataFrameServer()$data, {
        req(!is.null(dataFrameServer()$format))
        fmt <- dataFrameServer()$format
        data <- dataFrameServer()$data
        # if gene columns is not provided, map accession_number (if provided) to gene name
        if ((!fmt$check$gene_rep & !fmt$check$gene_sample_control &!fmt$check$gene_signif) & 
            (fmt$check$accession_rep|fmt$check$accession_sample_control|fmt$check$accession_signif)){
          data <- map_gene_id(dataFrameServer()$data)
        }
        mappedDataFrame <- list(data=data, format=fmt)
        mappedDataFrame
      })
    )
  })
}

enrichmentStatsServer <- function(id, 
                                  mapAccessionToGeneServer, 
                                  statsParamsValues,
                                  dataServer,
                                  errorValues) {
  if (!is.reactive(mapAccessionToGeneServer)){
    stop("mapAccessionToGeneServer passed to enrichmentStatsServer is not reactive")}
  if (!is.reactivevalues(statsParamsValues)){
    stop("statsParamsValues passed to enrichmentStatsServer is not reactiveValues")}
  moduleServer(id, function(input, output, session) {
    observeEvent(
      c(mapAccessionToGeneServer()$data, statsParamsValues$modTTest),
      {
        # TODO check for error checking comprehensiveness
        validate(need(!(grepl("Error", errorValues$input_errors)), 
                      "input_errors detected from enrichmentStatsServer"))
        req(mapAccessionToGeneServer()$data)
        df <- mapAccessionToGeneServer()$data
        fmt <- mapAccessionToGeneServer()$format
        req(!is.null(fmt), cancelOutput = TRUE)
        req(statsParamsValues$modTTest)
        req(statsParamsValues$signifType)
        if (statsParamsValues$signifType == "fdr") {
          req(statsParamsValues$fdrThresh)
        } else if (statsParamsValues$signifType == "pvalue") {
          req(statsParamsValues$pValThresh)
        } else {stop(
          "invalid signifType from statsParamsValues passed to enrichmentStatsServer.")
        }
        # moderated t.test still needed (i.e., provided data without significance statistics)
        if ((fmt$check$gene_rep | fmt$check$accession_rep | 
            fmt$check$gene_sample_control | fmt$check$accession_sample_control) &
            (fmt$check$gene_signif|fmt$check$accession_signif)) {
          errorValues$stats_errors <- paste0(
            genoppi:::bold('Note:  '),
            "Not using user provided significance statistics 
            (calculating statistics using provided columns).")
        }
        if (fmt$check$gene_rep | fmt$check$accession_rep | 
            fmt$check$gene_sample_control | fmt$check$accession_sample_control) {
          # set allowed column names
          allowed = unlist(fmt$allowed[unlist(fmt$check)])
          allowed_cols = lapply(allowed, function(x) grepl(x, colnames(df)))
          allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
          allowed_vec = allowed_vec | 'gene' %in% colnames(df)
          
          # ensure moderated t.test is only calculated on allowed columns
          df = df[,colnames(df)[allowed_vec]]
          # determine two_sample parameter to the calc_mod_ttest call
          modTTest <- statsParamsValues$modTTest
          req(modTTest)
          df <- calc_mod_ttest(df, two_sample = modTTest=="Two-sample")
        }
        else if (fmt$check$gene_signif|fmt$check$accession_signif) {
          errorValues$stats_errors <- paste0(
            genoppi:::bold('Note:  '),
            "Using user provided significance statistics, 
            not enough columns given to perform moderated t-test.")
          print("Using user provided significance statistics.")
        } else {
          errorValues$stats_errors <- genoppi:::bold(paste0(
            "ERROR: dataServer passed to enrichmentStatsServer does not fit any allowed format in format check, 
            i.e., $format$check all FALSE"))
          print(
          "dataServer passed to enrichmentStatsServer does not fit any allowed 
          format in format check, i.e., $format$check all FALSE")
        }
        dataServer(df)
    })
  })
}

findSignificantServer <- function(id, statsParamsValues, dataServer, sigificanceServer) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      c(dataServer(),
        statsParamsValues$signifType,
        statsParamsValues$fdrThresh,
        statsParamsValues$pValThresh,
        statsParamsValues$logfcThresh,
        statsParamsValues$logfcDir), 
      {
        req(statsParamsValues$logfcDir)
        req(statsParamsValues$logfcThresh)
        df <- dataServer()
        req(df$logFC)
        if (statsParamsValues$signifType == 'fdr'){
         df <- id_significant_proteins(df, fdr_cutoff = statsParamsValues$fdrThresh, 
                                       logfc_dir = statsParamsValues$logfcDir, 
                                       logfc_cutoff = statsParamsValues$logfcThresh)
        } else {
         df <- id_significant_proteins(df, fdr_cutoff = NULL, 
                                       p_cutoff = statsParamsValues$pValThresh, 
                                       logfc_dir = statsParamsValues$logfcDir, 
                                       logfc_cutoff = statsParamsValues$logfcThresh)
        }
        sigificanceServer(df)
    })
  })
}