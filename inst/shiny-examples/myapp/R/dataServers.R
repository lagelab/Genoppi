dataPathServer <- function(id){
  moduleServer(id, function(input, output, session){
    return(reactiveVal(value=NULL))})
}

# dataServer <- function(id, dataPathServer) {
#   # dataPath <- dataPathServer(id)
#   moduleServer(id, function(input, output, session) {
#     return(reactive({
#       validate(need(!is.null(dataPathServer()), ''))
#       data.frame(datapath = dataPathServer(), stringsAsFactors = F)
#     }))
#   })
# }

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

extractColumnsServer <- function(id, dataFrameServer) {
  moduleServer(id, function(input, output, session) {
    return(
      eventReactive(dataFrameServer()$data, {
        reps = colnames(dataFrameServer()$data)[grepl('^rep[0-9]+$',colnames(dataFrameServer()$data))]
        samples = colnames(dataFrameServer()$data)[grepl('^sample[0-9]+$',colnames(dataFrameServer()$data))]
        controls = colnames(dataFrameServer()$data)[grepl('^control[0-9]+$',colnames(dataFrameServer()$data))]
        c(reps, samples, controls)
      })
    )
  })
}

mapAccessionToGeneServer <- function(id, dataFrameServer) {
  moduleServer(id, function(input, output, session) {
    return(
      eventReactive(dataFrameServer()$data, {
        req(!is.null(dataFrameServer()$format))
        if (dataFrameServer()$format$check$accession_rep | dataFrameServer()$format$check$accession_signif) {
          dataFrameServer()$data <- map_gene_id(dataFrameServer()$data)
        }
        dataFrameServer()
      })
    )
  })
}

enrichmentStatsServer <- function(id, mapAccessionToGeneServer, statsParamsServer) {
  if (!is.reactive(mapAccessionToGeneServer)){stop("mapAccessionToGeneServer passed to enrichmentStatsServer is not reactive")}
  if (!is.reactive(statsParamsServer)){stop("statsParamsServer passed to enrichmentStatsServer is not reactive")}
  moduleServer(id, function(input, output, session) {
    return(
      eventReactive(mapAccessionToGeneServer()$data, {
        df <- mapAccessionToGeneServer()$data
        fmt <- mapAccessionToGeneServer()$format
        req(!is.null(fmt), cancelOutput = TRUE)
        # enable check for input params after fixing reactive graph
        # req(statsParamsServer()$modTTest)
        # req(statsParamsServer()$signifType)
        # req(statsParamsServer()$fdrThresh)
        # req(statsParamsServer()$pvalThresh)
        # req(statsParamsServer()$logfcDir)
        # req(statsParamsServer()$logfcThresh)
        # moderated t.test still needed
        if (fmt$check$gene_rep | fmt$check$accession_rep){
          # set allowed column names
          allowed = unlist(fmt$allowed[unlist(fmt$check)])
          allowed_cols = lapply(allowed, function(x) grepl(x, colnames(df)))
          allowed_vec = apply(do.call(rbind, allowed_cols), 2, any)
          allowed_vec = allowed_vec | 'gene' %in% colnames(df)
          
          # ensure moderated t.test is only calculated on allowed columns
          df = df[,colnames(df)[allowed_vec]]
          # determine two_sample parameter to the calc_mod_ttest call
          twoSample <- statsParamsServer()$modTTest == "Two sample"
          # TODO fix bug if example button is clicked too quickly and input params are not loaded yet (add timer?)
          # Warning: Error in if: argument is of length zero
          # $modTTest
          # NULL
          df <- calc_mod_ttest(df, two_sample = twoSample)
        }
        # if pvalue, fdr is supplied from user, do nothing
        # else if (fmt$check$gene_signif | fmt$check$accession_signif){
        # }
        if (statsParamsServer()$signifType == 'fdr'){
          df <- id_enriched_proteins(df, fdr_cutoff = statsParamsServer()$fdrThresh, 
                                    logfc_dir = statsParamsServer()$logfcDir, 
                                    logfc_cutoff = statsParamsServer()$logfcThresh)
        } else {
          df <- id_enriched_proteins(df, fdr_cutoff = NULL, 
                                     p_cutoff = statsParamsServer()$pvalThresh, 
                                     logfc_dir = statsParamsServer()$logfcDir, 
                                     logfc_cutoff = statsParamsServer()$logfcThresh)
        }
        df
      })
    )
  })
}