# TODO run scripts with:
# - data path for HuRI, STRING, PCNet
# - processing scripts + path for InWeb, BioPlex, IRefIndex

#' Read data from a specified PPI database
#'
#' This function reads data from a specified PPI database and returns a data
#' frame containing the interactions between genes.
#'
#' @param data_name The name of the PPI database to read from. Valid options
#' are "HuRI", "STRING", or "PCNet".
#'
#' @return A data frame containing the interactions between genes.
#'
#' @examples
#' read_data("HuRI")
#' read_data("STRING")
#' read_data("PCNet")
read_data <- function(data_name) {
    if (data_name == "HuRI") {
        huri_genes <- read.csv("PPI_Databases/HuRI_Luck2020_SuppTable2.txt",
            header = TRUE, sep = "\t", stringsAsFactors = FALSE
        )
        huri_genes$ENSG <- sapply(
            strsplit(huri_genes$ensembl_gene_id, "\\."), "[[", 1)

        huri_ints <- read.csv("PPI_Databases/HuRI_Luck2020_SuppTable11.txt",
            header = TRUE, sep = "\t", stringsAsFactors = FALSE
        )

        huri_table <- data.frame(
            Gene1 = huri_genes$symbol[
                match(huri_ints$Ensembl_gene_id_a, huri_genes$ENSG)],
            Gene2 = huri_genes$symbol[
                match(huri_ints$Ensembl_gene_id_b, huri_genes$ENSG)]
        )

        return(huri_table)
    } else if (data_name == "STRING") {
        string_genes <- read.table(
            "PPI_Databases/STRING_9606.protein.info.v11.5.short.txt.gz",
            header = FALSE, sep = "\t", stringsAsFactors = FALSE
        )
        names(string_genes) <- c("protein_id", "gene_name", "protein_size")

        string_ints <- read.table(
            "PPI_Databases/STRING_9606.protein.physical.links.v11.5.txt.gz",
            header = TRUE, sep = " ", stringsAsFactors = FALSE
        )

        string_table <- data.frame(
            Gene1 = string_genes$gene_name[
                match(string_ints$protein1, string_genes$protein_id)],
            Gene2 = string_genes$gene_name[
                match(string_ints$protein2, string_genes$protein_id)]
        )

        return(string_table)
    } else if (data_name == "PCNet") {
        rcx <- RCX::readCX("PPI_Databases/PCNet_Huang2018.cx")
        pcnet_table <- data.frame(
            Gene1 = rcx$nodes$name[match(rcx$edges$source, rcx$nodes$id)],
            Gene2 = rcx$nodes$name[match(rcx$edges$target, rcx$nodes$id)]
        )

        return(pcnet_table)
    } else {
        stop("Invalid data name. Please provide a valid data name: 
        HuRI, STRING, or PCNet.")
    }
}

# ----------------------------------------------------------------------
# function for returning interactors of a bait in a interaction database
get_ints_list <- function(bait, database_df) {
    db_df <- NULL
    gene1 <- database_df$Gene1
    gene2 <- database_df$Gene2

    # all genes in the databaseDf
    db_genes <- unique(c(database_df$Gene1, database_df$Gene2))
database_df
    if (bait %in% db_genes) {
        # interactor genes
        ints <- setdiff(unique(c(
            subset(database_df, gene1 == bait)$Gene2,
            subset(database_df, gene2 == bait)$Gene1
        )), bait)

        # df with gene and significant columns
        # (significant==T for ints, F otherwise)
        db_df <- data.frame(gene = setdiff(db_genes, bait))
        db_df$significant <- db_df$gene %in% ints
    }

    return(db_df)
}

# ----------------------------------------------------------------------
# Functions for writing results


plot_pie <- function(pie_df, ct) {
    p <- ggplot2::ggplot(
        subset(pie_df, CellType == ct), aes(x = "", y = n, fill = value)) +
        ggplot2::facet_wrap(. ~ variable, nrow = 1) +
        geom_bar(width = 1, stat = "identity", color = "black") +
        ggplot2::coord_polar("y") +
        scale_fill_manual(name = "Reported", values = c("grey", "blue")) +
        ggplot2::ggtitle(paste("Combined PPI network in", ct)) +
        ggplot2::theme_void() +
        theme(
            legend.position = "bottom", 
            plot.title = element_text(hjust = 0.5, vjust = 2))
    print(p)
}

write_to_file <- function(df, path) {
    write.table(df, path, quote = FALSE, row.names = FALSE, sep = "\t")
}


# ----------------------------------------------------------------------

# Read in index gene interactors from our study
# Assess overlap with known interactors in PPI databases
int_df <- read.table("InteractorLists/210406_MICOM_MasterInteractorTable.txt",
    header = TRUE, sep = "\t", stringsAsFactors = FALSE
)

prefixes <- c(
    "ARHGEF26_EC", "BCAS3_EC", "EDN1_EC", "FLT1_EC", "FN1_EC",
    "HDAC9_EC", "JCAD_EC", "PHACTR1_EC", "PLPP3_EC",
    "ADAMTS7_SMC", "ARHGEF26_SMC", "BCAS3_SMC", "EDN1_SMC", "EDNRA_SMC",
    "FN1_SMC", "HDAC9_SMC", "JCAD_SMC", "PHACTR1_SMC", "PLPP3_SMC"
)

db_list <- list(
    # InWeb = inweb_table, BioPlex = bioplex_table, iRefIndex = irefindex_table,
    HuRI = huri_table, STRING = string_table, PCNet = pcnet_table
)

overlap_res <- NULL
int_annot_df <- NULL

# Iterate through int lists (BAIT_CELLTYPE)
for (prefix in prefixes) {
    prefix_table <- subset(int_df, ListName == prefix)
    bait <- strsplit(prefix, "_")[[1]][1]
    temp_df <- subset(
        prefix_table, Interactor)[, c("CellType", "Bait", "GeneName")]

    # Get known ints in databases
    db_list_df <- NULL
    for (db in names(db_list)) {
        # Handle the case when the individual PPI database is null
        if (is.null(db_list[[db]])) {
            print(paste("Skipping", db, "in int list iteration as it is null."))
        }
        db_df <- get_ints_list(bait, db_list[[db]])
        if (!is.null(db_df)) { # If bait in database
            db_list_df <- rbind(db_list_df, data.frame(listName = db, db_df))
            temp_df[db] <- tempDf$GeneName %in% subset(db_df, significant)$gene
        } else {
            temp_df[db] <- FALSE
        }
    }

    # table of interactors with T/F annotation for each database
    int_annot_df <- rbind(int_annot_df, tempDf)

    # overlap enrichment test
    pre_df <- data.frame(
        gene = prefix_table$GeneName, significant = prefix_table$Interactor)
    intersect_df <- data.frame(
        listName = unique(db_list_df$listName), intersectN = TRUE)
    overlap_res <- rbind(overlap_res, data.frame(
        Dataset = prefix,
        calc_hyper(pre_df, db_list_df, intersect_df, bait)$statistics
    ))
}

# combined EC, SMC, EC-SMC pie charts
pie_df <-
    rbind(int_annot_df,
          data.frame(CellType = "EC-SMC",
          unique(int_annot_df[, -1]))) |>
    melt(id.vars = c("CellType", "Bait", "GeneName")) |>
    group_by(CellType, variable) |>
    count(value) |>
    # Calculate percentage as n / sum of n by variable
    mutate(perc = 100 * n / sum(n))

# collapse and output interactor table with database annotations
int_annot_df <- int_annot_df |>
    group_by(Bait, GeneName) |>
    summarise(
        CellTypes = toString(CellType),
        # InWeb = unique(InWeb),
        # BioPlex = unique(BioPlex),
        # iRefIndex = unique(iRefIndex),
        HuRI = unique(HuRI),
        STRING = unique(STRING),
        PCNet = unique(PCNet)
    )


write_to_file(overlap_res, "230123_MICOM_DatabaseIntsOverlapStats.txt")
write_to_file(pie_df, "230123_MICOM_DatabaseIntsPieChartStats.txt")
write_to_file(int_annot_df, "230123_MICOM_IntDatabaseAnnotations.txt")

pdf("Plots/230123_MICOM_DatabaseIntsPieCharts.pdf", width = 7, height = 3)

for (ct in c("EC", "SMC", "EC-SMC")) {
    plot_pie(pieDf, ct)
}

dev.off()
