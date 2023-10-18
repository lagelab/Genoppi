# TODO document
# get_PPI_DB_interactor
# take specific threshold as input (refer to Iref, Bioplex and Inweb)
# include the three new dataset (HuRI, PCNet, STRING)
# redownload from paper directly,
# makes a list of metrics we could use as threshold
get_ppi_list <- function(bait, database_name){
  stopifnot(database_name %in% c(
    'bioplex', 'irefindex', 'inweb', 'huri', 'string', 'pcnet'))
  database_lookup_table <- list(
    'bioplex' = genoppi::get_bioplex_list,
    'irefindex' = genoppi::get_irefindex_list,
    'inweb' = genoppi::get_inweb_list,
    'huri' = genoppi::get_huri_list,
    'string' = genoppi::get_string_list,
    'pcnet' = genoppi::get_pcnet_list
  )
  database_func <- database_lookup_table[[as.character(database_name)]]

  return(database_func(bait))
}