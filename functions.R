#Created 11/1/16 by April Kim
#Functions file for Shiny app Genoppi
'%!in%' <- function(x,y)!('%in%'(x,y))

calculate_moderated_ttest_hgnc <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file))
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  ### apply median normalization
  medianNorm <- function(d){return(d - median(d, na.rm = TRUE))}
  calculated$input_file.rep1 <- medianNorm(calculated$input_file.rep1)
  calculated$input_file.rep2 <- medianNorm(calculated$input_file.rep2)
  ### calc moderated t test
  myfit <- lmFit(calculated[,-1], method="robust")
  myfit <- eBayes(myfit)
  # modtest <- topTable(myfit, number=nrow(m), sort.by='none')
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  ### prepare final data to use in plots
  calculated <- data.frame(cbind(calculated, modtest))
  calculated <- calculated[,-c(5,6,9)]
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR")
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  calculated$gene <- tmp_g$input_file.gene
  calculated
}


calculate_moderated_ttest_uniprot <- function(input_file){
  tmp <- data.frame(input_file$gene, rownames(input_file), input_file$accession_number)
  calculated <- data.frame(rownames(input_file), input_file$rep1, input_file$rep2)
  ### apply median normalization
  medianNorm <- function(d){return(d - median(d, na.rm = TRUE))}
  calculated$input_file.rep1 <- medianNorm(calculated$input_file.rep1)
  calculated$input_file.rep2 <- medianNorm(calculated$input_file.rep2)
  ### calc moderated t test
  myfit <- lmFit(calculated[,-1], method="robust")
  myfit <- eBayes(myfit)
  # modtest <- topTable(myfit, number=nrow(m), sort.by='none')
  modtest <- topTable(myfit, number=nrow(myfit), sort.by='none')
  ### prepare final data to use in plots
  calculated <- data.frame(cbind(calculated, modtest))
  calculated <- calculated[,-c(5,6,9)]
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR")
  ### add gene names
  tmp_g <- subset(tmp, rownames.input_file. %in% calculated$id)
  tmp_g[is.na(tmp_g$input_file.gene),] <- "NotAssigned"
  # calculated$gene <- tmp_g$input_file.gene
  calculated <- merge(calculated, tmp_g, by.x = "id", by.y = "rownames.input_file.")
  colnames(calculated) <- c("id", "rep1", "rep2","logFC","pvalue","FDR","gene","accession_number")
  calculated
}

#Gradient for user upload color
separate_to_groups_for_color <- function(vp_data, color_data, color_theme, type){
  # color_data <- color_data[complete.cases(color_data),]
  # color_data <- as.numeric(color_data$score)
  if(type == "cont"){
    color_data$score_rounded <- round(as.numeric(color_data$score), 1)
    col <- c(color = colorRampPalette(brewer.pal(11, color_theme))(((max(color_data$score_rounded)-min(color_data$score_rounded))*10 + 1)))
    unique_col <- data.frame(cbind("unique_score"=seq(min(color_data$score_rounded), max(color_data$score_rounded), by=0.1), col))
    color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score_rounded")
  } else {
    unique_score <- unique(sort(color_data$score))
    col <- c(color = colorRampPalette(brewer.pal(3, color_theme))(length(unique_score)))
    unique_col <- data.frame(cbind(unique_score, col))
    color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score")
  }
  vp_data$score <- color_data$score[match(vp_data$gene, color_data$gene)]
  vp_data$score[is.na(vp_data$score)] <- "empty"
  yes_exist <- subset(vp_data, score != "empty")
  no_exist <- subset(vp_data, score == "empty")
  
  yes_exist$col <- color_d$col[match(yes_exist$gene, color_d$gene)]
  df1 <- yes_exist
  
  list(df1=df1, no_exist=no_exist)
}

#Discrete for user upload color
# separate_to_groups_for_color_discrete <- function(vp_data, color_data, color_theme, type){
#   unique_score <- unique(sort(color_data$score))
#   col <- c(color = colorRampPalette(brewer.pal(3, color_theme))(length(unique_score)))
#   unique_col <- data.frame(cbind(unique_score, col))
#   color_d <- merge(unique_col, color_data, by.x = "unique_score", by.y = "score")
#   
#   vp_data$score <- color_data$score[match(vp_data$gene, color_data$gene)]
#   vp_data$score[is.na(vp_data$score)] <- "empty"
#   yes_exist <- subset(vp_data, score != "empty")
#   no_exist <- subset(vp_data, score == "empty")
#   
#   yes_exist$col <- color_d$col[match(yes_exist$gene, color_d$gene)]
#   df1 <- yes_exist
#   
#   list(df1=df1, no_exist=no_exist)
# }

#Marker colors based on FDR
separate_to_groups_for_color_integrated <- function(vp_data, threshold, col1, col2){
  below_thresh <- subset(vp_data, FDR < threshold)
  above_thresh <- subset(vp_data, FDR >= threshold)
  a_col <- colorRampPalette(c(col1, col1))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c(col2, col2))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}
# separate_to_groups_for_color_integrated <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR < threshold)
#   above_thresh <- subset(vp_data, FDR >= threshold)
#   below_thresh$col <- "sig"
#   above_thresh$col <- "insig"
#   # a_col <- colorRampPalette(c(col1, col1))(nrow(below_thresh))
#   # below_thresh <- cbind(below_thresh, rev(a_col))
#   # names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   # b_col <- colorRampPalette(c(col2, col2))(nrow(above_thresh))
#   # above_thresh <- cbind(above_thresh, b_col)
#   # names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

#Blue and green separate, specifically in case plot opacity is low
# separate_to_groups_for_color_integrated_bar <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR <= threshold)
#   above_thresh <- subset(vp_data, FDR > threshold)
#   #
#   a_col <- colorRampPalette(c('seagreen3', 'seagreen3'))(nrow(below_thresh))
#   below_thresh <- cbind(below_thresh, rev(a_col))
#   names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   b_col <- colorRampPalette(c("royalblue2", "royalblue2"))(nrow(above_thresh))
#   above_thresh <- cbind(above_thresh, b_col)
#   names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

#Two gray tones
separate_to_groups_for_cbf_integrated <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR < threshold)
  above_thresh <- subset(vp_data, FDR >= threshold)
  a_col <- colorRampPalette(c('#525252', '#525252'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("grey76", "grey76"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

#Two gray tones, specifically in case plot opacity is low
# separate_to_groups_for_cbf_integrated_bar <- function(vp_data, threshold){
#   below_thresh <- subset(vp_data, FDR <= threshold)
#   above_thresh <- subset(vp_data, FDR > threshold)
#   a_col <- colorRampPalette(c('#969696', '#969696'))(nrow(below_thresh))
#   below_thresh <- cbind(below_thresh, rev(a_col))
#   names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
#   b_col <- colorRampPalette(c("#d9d9d9", "#d9d9d9"))(nrow(above_thresh))
#   above_thresh <- cbind(above_thresh, b_col)
#   names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
#   data <- rbind(below_thresh, above_thresh)
# }

separate_to_groups_for_exac_bar <- function(vp_data){
  grp1 <- subset(vp_data, FDR <= 0.33)
  grp2 <- subset(vp_data, 0.33 < FDR & FDR <= 0.66)
  grp3 <- subset(vp_data, 0.66 < FDR & FDR <= 1)
  a_col <- colorRampPalette(c('#66c2a5', '#66c2a5'))(nrow(grp1))
  grp1 <- cbind(grp1, rev(a_col))
  names(grp1)[names(grp1) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("#fc8d62", "#fc8d62"))(nrow(grp2))
  grp2 <- cbind(grp2, b_col)
  names(grp2)[names(grp2) == 'b_col'] <- 'col'
  c_col <- colorRampPalette(c("#8da0cb", "#8da0cb"))(nrow(grp3))
  grp3 <- cbind(grp3, c_col)
  names(grp3)[names(grp3) == 'c_col'] <- 'col'
  data <- rbind(grp1, grp2, grp3)
}

#volcano - gg

plot_volcano_qc_gg <- function(d){
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(alpha = 0.5, size = 1.5, colour = d$col) + 
    xlab("log2FC") + ylab("-log10(P)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}

plot_volcano_qc_gg_1 <- function(data, data1, col, col1, logfc1, logfc2, pval){
  d <- rbind(data, data1)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text=sprintf("Gene: %s<br>FDR: %s", gene, FDR))) +
    geom_point(data = data1, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col1) + 
    geom_point(data = data, aes(x = logFC, y = -log10(pvalue)),alpha = 0.5, size = 1.5, colour = col) + 
    geom_vline(xintercept = logfc1, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_vline(xintercept = logfc2, size = 0.5, linetype = "dotted", colour = "#737373") +
    geom_hline(yintercept = pval, size = 0.5, linetype = "dotted", colour = "#737373") +
    # xlab("log2FC") + ylab("-log10(P)") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"),
          aspect.ratio=1)
  p
}

plot_volcano_exac_gg <- function(b, a, n){
  d <- rbind(b, a, n)
  p <- ggplot(data = d, aes(x = logFC, y = -log10(pvalue), text = gene)) +
    geom_point(data = b, alpha = 0.5, size = 1.5, colour = "#66c2a5") + 
    geom_point(data = a, alpha = 0.5, size = 1.5, colour = "#fc8d62") + 
    geom_point(data = n, alpha = 0.5, size = 1.5, colour = "#8da0cb") + 
    xlab(bquote(log[2]*"(Fold change)")) + ylab(bquote(-log[10]*"("*italic(.("P"))*"-value)")) +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}

plot_scatter_qc_gg <- function(d){
  p <- ggplot(data = d, aes(x = rep1, y = rep2, text = gene)) +
    geom_point(alpha = 0.5, size = 1.5, colour = d$col) + 
    geom_abline(intercept = 0, slope = 1, linetype = "longdash", size=0.2) +
    xlab("rep1") + ylab("rep2") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}

plot_scatter_exac_gg <- function(b, a, n){
  d <- rbind(b, a, n)
  p <- ggplot(data = d, aes(x = rep1, y = rep2, text = gene)) +
    geom_point(data = b, alpha = 0.5, size = 1.5, colour = "#66c2a5") + 
    geom_point(data = a, alpha = 0.5, size = 1.5, colour = "#fc8d62") + 
    geom_point(data = n, alpha = 0.5, size = 1.5, colour = "#8da0cb") + 
    xlab("rep1") + ylab("rep2") +
    theme_minimal() +
    theme(axis.text.x = element_text(size=7),
          axis.title.x=element_text(size=8),
          axis.text.y=element_text(size=7),
          axis.title.y=element_text(size=8),
          panel.grid.minor = element_blank(),
          plot.margin = unit(c(1,1,1,1), "pt"))
  p
}

#volcano - opacity = 1 colorscale
plot_volcano_qc <- function(d){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.9,
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

plot_volcano_user_color <- function(n_exist, y_exist){
  p <- plot_ly(showlegend = T, width = 550, height = 550) 
  p <- add_markers(p, data = n_exist, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#f7f7f7"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in data (", nrow(df$no_exist), ")"))
  for(i in nrow(y_exist)){
    p <- add_markers(p, data = y_exist, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.9, 
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text") #, name = "pull down"
  }
  p
}
 
#volcano - lower opacity to highlight different layers
plot_volcano_multiple_cond <- function(d){
  p <- plot_ly(showlegend = FALSE, height = 320, width = 320) #
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.8, 
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

#volcano - lower opacity to highlight different layers WITH legend
plot_volcano_multiple_cond_for_goi <- function(d){
  p <- plot_ly(showlegend = TRUE, height = 350) #, height = 320
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.8, 
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

#volcano - exac colorscale
plot_volcano_exac <- function(b, a, n){
  p <- plot_ly(showlegend = T, width = 650, height = 550)
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI<0.9 (", nrow(b), ")"))
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI>=0.9 (", nrow(a), ")"))
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in ExAC (", nrow(n), ")"))
}

#volcano - exac colorscale
plot_volcano_exac_multi <- function(b, a, n){
  p <- plot_ly(showlegend = T, width = 320, height = 390)
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI<0.9 (", nrow(b), ")"))
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("pLI>=0.9 (", nrow(a), ")"))
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text", name = paste0("not in ExAC (", nrow(n), ")"))
}



#volcano - search/found highlight
search_volcano <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- add_markers(p, data = found, x = ~logFC, y = ~-log10(pvalue), 
                marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
                hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

search_volcano_gg <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p + 
      geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, fill = "#ffffb3", colour = "black", shape=21) +
      # geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text_repel(data = found, aes(label = gene),
                      arrow = arrow(length = unit(0.5, 'npc')), box.padding = unit(0.15, "lines"),
                      point.padding = unit(0.2, "lines"), color = "black", size=2)
  }
  p
}

search_volcano_gg_reg_label <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p + 
      geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, fill = "#ffffb3", colour = "black", shape=21) +
      # geom_point(data = found, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text(data = found, aes(label = gene), color = "black", size=2, position=position_jitter(1L))
  }
  p
}

search_scatter_gg <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- p + geom_point(data = found, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "red") +
      geom_point(data = found, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
      geom_text_repel(data = found, aes(label = gene),
                      arrow = arrow(length = unit(0.5, 'npc')), box.padding = unit(0.15, "lines"),
                      point.padding = unit(0.2, "lines"), color = "black", size=2)
  }
  p
}

#scatter - opacity = 1 colorscale
plot_scatter_qc <- function(orig, d){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550) 
  p <- add_markers(p, x = 0, y = 0, opacity = 0)
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 text = "x=y", hoverinfo = "text",
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2, 
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")),
                     opacity = 0.9,
                     text = ~paste0(gene, ", rep1=", round(rep1, digits = 2), ", rep2=", round(rep2, digits = 2)), hoverinfo = "text", name = "pull down")
  }
  p
}

#scatter - lower opacity to highlight different layers
plot_scatter_multiple_cond <- function(orig, d){
  p <- plot_ly(showlegend = FALSE, width = 320, height = 320) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 text = "x=y", hoverinfo = "text",
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2, 
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.8, 
                     text = ~paste0(gene, ", rep1=", round(rep1, digits = 2), ", rep2=", round(rep2, digits = 2)), hoverinfo = "text", name = "pull down")
  }
  p
}

#scatter - lower opacity to highlight different layers
plot_scatter_white <- function(orig){
  p <- plot_ly(data = orig, x = ~c(min(rep1), max(rep1)), y = 0, marker = list(color = "white"), hoverinfo = "none", showlegend = FALSE, width = 320, height = 320) 
  p %>% layout(xaxis = list(fixedrange=TRUE), yaxis = list(fixedrange=TRUE))
}

#scatter - exac colorscale
plot_scatter_exac <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 550, height = 550) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI<0.9 (", nrow(b), ")")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI>=0.9 (", nrow(a), ")")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("not in ExAC (", nrow(n), ")")
}

plot_scatter_exac_multi <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 320, height = 320) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#66c2a5"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI<0.9 (", nrow(b), ")")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#fc8d62"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("pLI>=0.9 (", nrow(a), ")")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#8da0cb"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text") #, name = paste0("not in ExAC (", nrow(n), ")")
}
search_volcano <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- add_markers(p, data = found, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
  p
}

search_scatter <- function(p, found){
  if(nrow(found)==0){
    p
  } else{
    p <- add_markers(p, data = found, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
                     textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
                     hoverinfo="text+x+y", text = ~paste(gene), showlegend = FALSE)
  }
}

vp_layer_for_inweb <- function(p, d_in, marker_col, lab){
  if(nrow(d_in)==0){
    validate(
      need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
    )
  } else if(nrow(d_in)>0){
    if(lab == "yes_label"){
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                  marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                  mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                  text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                  name = "InWeb") 
    } else{
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                  marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                  mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                  name = "InWeb") 
    }
  }
}

vp_layer_for_inweb_sf_gg <- function(p, d_in, marker_col, lab){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- p + geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_in, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

# vp_layer_for_inweb_no_text <- function(p, d_in, marker_col){
#   if(nrow(d_in)==0){
#     validate(
#       need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
#     )
#   } else if(nrow(d_in)>0){
#     add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
#                 marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
#                 mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
#                 name = "InWeb") 
#   }
# }

vp_layer_for_inweb_cbf <- function(p, d_in, lab){
  if(nrow(d_in)==0){
    validate(
      need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
    )
  } else if(nrow(d_in)>0){
    if(lab == "yes_label"){
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                  marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                  # mode = "markers",
                  mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                  text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                  name = "InWeb") 
    } else{
      p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                  marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                  # mode = "markers",
                  mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                  name = "InWeb") 
    }
  }
}

# vp_layer_for_inweb_cbf_no_text <- function(p, d_in){
#   if(nrow(d_in)==0){
#     validate(
#       need(nrow(d_in)>0, "No overlapping InWeb interactors identified")
#     )
#   } else if(nrow(d_in)>0){
#     add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
#                 marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
#                 # mode = "markers",
#                 mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
#                 name = "InWeb") 
#   }
# }

vp_layer_for_inweb_sf <- function(p, d_in, marker_col){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                name = "InWeb") 
  }
}

vp_layer_for_inweb_no_text_sf <- function(p, d_in, marker_col){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                name = "InWeb") 
  }
}

vp_layer_for_inweb_cbf_sf <- function(p, d_in){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                # mode = "markers",
                mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
                name = "InWeb") 
  }
}

vp_layer_for_inweb_cbf_sf_gg <- function(p, d_in, lab){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- p + geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "#ffffff") +
      geom_point(data = d_in, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_in, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_inweb_cbf_no_text_sf <- function(p, d_in){
  if(nrow(d_in)==0){
    p
  } else if(nrow(d_in)>0){
    p <- add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
                # mode = "markers",
                mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
                name = "InWeb") 
  }
}

vp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl, marker_col){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_gg <- function(p, snp_sgl, marker_col, lab){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- p + geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_sgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl, marker_col){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
                mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
                mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
                name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf_gg <- function(p, snp_sgl, lab){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- p + geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "#ffffff", shape=15) +
      geom_point(data = snp_sgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=0) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_sgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  if(nrow(snp_sgl)==0){
    p
  } else if(nrow(snp_sgl)>0){
    p <- add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
                marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
                mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
                name = "SGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl, marker_col){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, snpid, sep = "\n"), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_gg <- function(p, snp_mgl, marker_col, lab){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- p + geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = marker_col) +
      geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_mgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl, marker_col){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = marker_col, size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf_gg <- function(p, snp_mgl, lab){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- p + geom_point(data = snp_mgl, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=23, fill="#ffffff") +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = snp_mgl, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
  p
}

vp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  if(nrow(snp_mgl)==0){
    p
  } else if(nrow(snp_mgl)>0){
    p <- add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              name = "MGL gene")
  }
  p
}

vp_layer_for_snp_to_gene_none <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3", hoverinfo = "none",
              name = "no SNP to gene")
}

vp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3", hoverinfo = "none",
              name = "no SNP to gene")
}

vp_layer_for_uploaded_genes_gg <- function(p, d_g2s, usr_palette, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue), colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = usr_palette) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
}

vp_layer_for_uploaded_genes_cbf_gg <- function(p, d_g2s, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue), colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = logFC, y = -log10(pvalue)), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = "Greys") +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
}

vp_layer_for_uploaded_genes <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    print(d_g2s)
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15, 
                color = ~factor(.id), 
                mode = "markers+text", hoverinfo = "text", 
                text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15, 
                color = ~factor(.id), 
                mode = "markers+text", hoverinfo = "text") 
  }
}

vp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
                color = ~factor(.id), 
                mode = "markers+text", hoverinfo = "text", 
                text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
                marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
                color = ~factor(.id), 
                mode = "markers+text", hoverinfo = "text") 
    # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
  }
}

vp_layer_for_uploaded_genes_none <- function(p, d){
  p <- add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group1", hoverinfo = "text",
              name = "no genes of interest")
}

vp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  add_markers(p, data = d, x = 0, y= ~-log10(max(pvalue)),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2", hoverinfo = "none",
              name = "no genes of interest")
}

sp_layer_for_inweb <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_inweb_no_text <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              name = "InWeb") 
}

sp_layer_for_inweb_cbf <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_inweb_cbf_no_text <- function(p, d_in){
  p <- add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              name = "InWeb") 
}

sp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  p <- add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              name = "SGL gene")
}


sp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  p <- add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_none <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3", hoverinfo = "none",
              name = "no SNP to gene")
}

sp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3", hoverinfo = "none",
              name = "no SNP to gene")
}

sp_layer_for_uploaded_genes <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), 
              color = ~factor(.id), colors = "Blues",
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_gg <- function(p, d_g2s, usr_palette, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2, colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = usr_palette) +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
}

sp_layer_for_uploaded_genes_cbf_gg <- function(p, d_g2s, lab){
  if(nrow(d_g2s)==0){
    validate(
      need(nrow(d_g2s)>0, "No overlapping GOIs identified")
    )
  } else if(nrow(d_g2s)>0){
    p <- p + geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2, colour = factor(.id)), size = 2, alpha = 1) +
      geom_point(data = d_g2s, mapping=aes(x = rep1, y = rep2), size = 2, alpha = 1, colour = "black", shape=1) +
      scale_color_brewer(name=" ", palette = "Greys") +
      theme(legend.text = element_text(size=7),
            legend.title = element_text(size=8))
    if(lab == "yes_label"){
      p <- p + geom_text_repel(data = d_g2s, aes(label = gene),
                               arrow = arrow(length = unit(0.05, 'inches')), 
                               color = "black", size=2)
    }
    p
  }
}


sp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), 
              color = ~factor(.id), colors = "Blues",
              mode = "markers+text", hoverinfo = "text") 
}

sp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  p <- add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text") 
}

sp_layer_for_uploaded_genes_none <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2", hoverinfo = "none",
              name = "no genes of interest")
}

sp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  p <- add_markers(p, data = d, x = ~min(rep1), y= ~min(rep2),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2", hoverinfo = "none",
              name = "no genes of interest")
}

assignFamily_inc_doubles <- function(d, overlay_d){
  if (nrow(d)<1){
    d_families <- data.frame("gene" = character(0), "id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), "overlay" = character(0), "frequency" = numeric(0))
  }
  else{
    d1 <- d[FALSE,]
    d_families <- ldply(unique(c(d$gene)), function(name) { 
      ix <- overlay_d[[name]]
      d_ix <- d[d$gene == name,]
      d_ix <- d_ix[rep(seq_len(nrow(d_ix)), each=length(ix)),]
      d_ix$overlay <- ix
      d1 <- rbindlist(list(d1,d_ix), use.names=T, fill = T)
    })
    if (nrow(d_families)<1){
      d_families <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), "gene" = character(0), "accession_number" = character(0), "overlay" = character(0), "frequency" = numeric(0))
      # }
    } else if (nrow(d_families) >= 1){
      d_families <- d_families[with(d_families, order(overlay, decreasing = FALSE)),]
      # howmany <- length(unique(d_families$overlay))
      tmp <- data.frame(table(d_families$overlay))
      d_families$frequency <- tmp$Freq[match(d_families$overlay, tmp$Var1)]
      ### uncomment the line below if you would like to show protein families with more than one member
      #d_families <- subset(d_families, frequency>1)
      ### in case it becomes empty after subsetting
    }
  }
  # print(d_families)
  d_families
}

addNames <- function(d){
  if((file.exists("data/genes_naf_wnames.txt")) == TRUE){
    ### read genes that have no family assigned
    genesNotAs <- read.csv("data/genes_naf_wnames.txt", sep = "\t")
  }
  else {
    system("echo -e \"gene\"$'\t'\"name\" > data/genes_naf_secondary.txt")
    genesNotAs <- read.csv("data/genes_naf_secondary.txt", sep = "\t")
  }
  system("rm data/genes_naf_wnames.txt")
  system("rm data/genes_naf_secondary.txt")
  ### gna = gene not assigned
  d_gna <- subset(d, gene %in% genesNotAs$gene)
  d_gna$name <- genesNotAs$name[match(d_gna$gene, genesNotAs$gene)]
  d_gna
}

makePlotFamilies_1quadrant <- function(data_fam, data, sortPF){ #data_gna, 
  ### replace logFC and pvalue in di* by these from dpm
  data_fam$logFC <- data$logFC[match(data_fam$gene, data$gene)]
  data_fam$pvalue <- data$pvalue[match(data_fam$gene, data$gene)]
  if(sortPF == "sort_f"){
    data_fam <- data_fam[order(-data_fam$frequency), ]
    if(nrow(data_fam)>=1){
      data_fam <- cbind(data_fam, new_f = paste0("(", data_fam$frequency, ") ", data_fam$overlay))
    }
    else {
      data_fam <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0), 
                             "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0), 
                             "gene" = character(0), "overlay" = character(0), "frequency" = numeric(0),
                             "new_f" = character(0))
    }
  }
  else if(sortPF == "sort_a"){
    data_fam 
    if(nrow(data_fam)>=1){
      data_fam <- cbind(data_fam, new_f = paste0(data_fam$overlay, " (", data_fam$frequency, ")"))
    }
    else {
      data_fam <- data.frame("id" = character(0), "rep1" = numeric(0), "rep2" = numeric(0),
                             "logFC" = numeric(0), "pvalue" = numeric(0), "FDR" = numeric(0),
                             "gene" = character(0), "overlay" = character(0), "frequency" = numeric(0),
                             "new_f" = character(0))
    }
  }
  data_fam$new_f <- factor(data_fam$new_f, levels = unique(data_fam$new_f))

  # data_gna$logFC <- data$logFC[match(data_gna$gene, data$gene)]
  # data_gna$pvalue <- data$pvalue[match(data_gna$gene, data$gene)]
  # pf_list <- list("list" = data_fam) #, "list" = data_gna
  pf_list <- data_fam
  pf_list
  #for download
  # families <- cbind(data_fam$gene, data_fam$family, data_fam$frequency)
  # colnames(families) <- c("gene", "family", "frequency")
}

compare_two_files_a <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_a_scatter <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 420, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2, 
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_b <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_b_scatter <- function(orig, subset, overlaps){
  if(nrow(subset)==0 & nrow(overlaps)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps)>0){
    p <- plot_ly(showlegend = T, width = 420, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2, 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_aa <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ef3b2c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_aa_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2,
                       marker = list(color = "#ef3b2c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f1")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_bb <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_bb_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2, 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f2")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f12")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_cc <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f3")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_cc_scatter <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  if(nrow(subset)==0 & nrow(overlaps1)==0 & nrow(overlaps2)==0 & nrow(overlaps3)==0){
    validate(
      need(nrow(subset)>0 & nrow(overlaps1)>0 & nrow(overlaps2)>0 & nrow(overlaps3)>0, "No overlapping proteins identified")
    )
  } else if(nrow(subset)>0 | nrow(overlaps1)>0 | nrow(overlaps2)>0 | nrow(overlaps3)>0){
    p <- plot_ly(showlegend = T, width = 300, height = 390)
    p <- add_markers(p, data = orig, x = ~rep1, y = ~rep2, 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~rep1, y = ~rep2, 
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f3")
    }
    if(nrow(overlaps1)>0){
      p <- add_markers(p, data = overlaps1, x = ~rep1, y = ~rep2,
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f13")
    }
    if(nrow(overlaps2)>0){
      p <- add_markers(p, data = overlaps2, x = ~rep1, y = ~rep2,
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f23")
    }
    if(nrow(overlaps3)>0){
      p <- add_markers(p, data = overlaps3, x = ~rep1, y = ~rep2,
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9,
                       text = ~paste(gene), hoverinfo = "text", name = "f123")
    }
  }
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_pf_a <- function(orig, subset, overlaps){ #s_nga, , o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue),
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f1 unassigned")
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f12 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "none")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "none")
    }
  }
  p
}

compare_two_files_pf_a_size <- function(orig, subset, overlaps, increase){ # s_nga, o_nga, 
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    print(head(orig))
    print(head(subset))
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     text = ~gene, hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f1 unassigned") 
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f12 unassigned") 
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#e41a1c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_b <- function(orig, subset, overlaps){ #, s_nga , o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2 unassigned") 
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f12 unassigned") 
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_b_size <- function(orig, subset, overlaps, increase){ #, s_nga, o_nga
  if(nrow(subset)==0 & nrow(overlaps)==0){ #nrow(s_nga)==0 &  & nrow(o_nga)==0
    validate(
      need(nrow(subset)>0& nrow(overlaps)>0, "No protein families identified") # & nrow(s_nga)>0 & nrow(o_nga)>0
    )
  } else if(nrow(subset)>0| nrow(overlaps)>0){ # | nrow(s_nga)>0  | nrow(o_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2 unassigned") 
    # }
    # if(nrow(o_nga)>0){
    #   p <- add_markers(p, data = o_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f12 unassigned") 
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(overlaps)>0){
      p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_aa <- function(orig, subset,o_12, o_13, o_123){ # s_nga, o_12_nga, o_13_nga, , o_123_nga
  if(nrow(subset)==0 & nrow(o_12)==0 & nrow(o_13)==0 #nrow(s_nga)==0 & nrow(o_12_nga)==0 & 
     & nrow(o_123)==0){ #nrow(o_13_nga)==0 & & nrow(o_123_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_12)>0 & nrow(o_13)>0 #& nrow(s_nga)>0 & nrow(o_12_nga)>0 
           & nrow(o_123)>0, "No protein families identified") #nrow(o_13_nga)>0 & & nrow(o_123_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_12)>0 | nrow(o_13)>0 #| nrow(s_nga)>0 | nrow(o_12_nga)>0 
            | nrow(o_123)>0){ #nrow(o_13_nga)>0 | | nrow(o_123_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f1 unassigned") 
    # }
    # if(nrow(o_12_nga)>0){
    #   p <- add_markers(p, data = o_12_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f12 unassigned") 
    # }
    # if(nrow(o_13_nga)>0){
    #   p <- add_markers(p, data = o_13_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f13 unassigned") 
    # }
    # if(nrow(o_123_nga)>0){
    #   p <- add_markers(p, data = o_123_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f123 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_12)>0){
      p <- add_markers(p, data = o_12, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_13)>0){
      p <- add_markers(p, data = o_13, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_123)>0){
      p <- add_markers(p, data = o_123, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_aa_size <- function(orig, subset, o_12, o_13, o_123, increase){ #s_nga, o_12_nga, o_13_nga, o_123_nga, 
  if(nrow(subset)==0 & nrow(o_12)==0 & nrow(o_13)==0 #nrow(s_nga)==0 & nrow(o_12_nga)==0 & 
     & nrow(o_123)==0){ #nrow(o_13_nga)==0 & & nrow(o_123_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_12)>0 & nrow(o_13)>0 #& nrow(s_nga)>0 & nrow(o_12_nga)>0 
           & nrow(o_123)>0, "No protein families identified") #nrow(o_13_nga)>0 & & nrow(o_123_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_12)>0 | nrow(o_13)>0 #| nrow(s_nga)>0 | nrow(o_12_nga)>0 
            | nrow(o_123)>0){ #nrow(o_13_nga)>0 | | nrow(o_123_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#e41a1c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f1 unassigned") 
    # }
    # if(nrow(o_12_nga)>0){
    #   p <- add_markers(p, data = o_12_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fdb462'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f12 unassigned") 
    # }
    # if(nrow(o_13_nga)>0){
    #   p <- add_markers(p, data = o_13_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f13 unassigned") 
    # }
    # if(nrow(o_123_nga)>0){
    #   p <- add_markers(p, data = o_123_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f123 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#e41a1c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_12)>0){
      p <- add_markers(p, data = o_12, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fdb462", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_13)>0){
      p <- add_markers(p, data = o_13, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_123)>0){
      p <- add_markers(p, data = o_123, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1,
                                     opacity = 0.8), color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_bb <- function(orig, subset, o_21, o_213){ #s_nga, o_21_nga, o_23, o_23_nga, , o_213_nga
  if(nrow(subset)==0 & nrow(o_21)==0 & nrow(o_23)==0 #& nrow(s_nga)==0 & nrow(o_21_nga)==0 
     & nrow(o_213)==0){ #nrow(o_23_nga)==0 & & nrow(o_213_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_21)>0 & nrow(o_23)>0 # & nrow(s_nga)>0  & nrow(o_21_nga)>0 
           & nrow(o_213)>0, "No protein families identified") # nrow(o_23_nga)>0 & & nrow(o_213_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_21)>0 | nrow(o_23)>0 # | nrow(s_nga)>0| nrow(o_21_nga)>0 
            | nrow(o_213)>0){ #nrow(o_23_nga)>0 | | nrow(o_213_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2 unassigned") 
    # }
    # if(nrow(o_21_nga)>0){
    #   p <- add_markers(p, data = o_21_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fd8d3c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2&1 unassigned") 
    # }
    # if(nrow(o_23_nga)>0){
    #   p <- add_markers(p, data = o_23_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2&3 unassigned") 
    # }
    # if(nrow(o_213_nga)>0){
    #   p <- add_markers(p, data = o_213_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f21&3 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_21)>0){
      p <- add_markers(p, data = o_21, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_23)>0){
      p <- add_markers(p, data = o_23, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_213)>0){
      p <- add_markers(p, data = o_213, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_bb_size <- function(orig, subset, o_21, o_23, o_213, increase){ # s_nga, o_21_nga, o_23_nga, o_213_nga, 
  if(nrow(subset)==0 & nrow(o_21)==0 & nrow(o_23)==0 #& nrow(s_nga)==0 & nrow(o_21_nga)==0 
     & nrow(o_213)==0){ #nrow(o_23_nga)==0 & & nrow(o_213_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_21)>0 & nrow(o_23)>0 # & nrow(s_nga)>0  & nrow(o_21_nga)>0 
           & nrow(o_213)>0, "No protein families identified") # nrow(o_23_nga)>0 & & nrow(o_213_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_21)>0 | nrow(o_23)>0 # | nrow(s_nga)>0| nrow(o_21_nga)>0 
            | nrow(o_213)>0){ #nrow(o_23_nga)>0 | | nrow(o_213_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#ffff33'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2 unassigned") 
    # }
    # if(nrow(o_21_nga)>0){
    #   p <- add_markers(p, data = o_21_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#fd8d3c'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2&1 unassigned") 
    # }
    # if(nrow(o_23_nga)>0){
    #   p <- add_markers(p, data = o_23_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f2&3 unassigned") 
    # }
    # if(nrow(o_213_nga)>0){
    #   p <- add_markers(p, data = o_213_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f21&3 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#ffff33", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_21)>0){
      p <- add_markers(p, data = o_21, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#fd8d3c", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_23)>0){
      p <- add_markers(p, data = o_23, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_213)>0){
      p <- add_markers(p, data = o_213, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_cc <- function(orig, subset, o_31, o_32, o_312){ #s_nga, o_31_nga, o_32_nga, , o_312_nga
  if(nrow(subset)==0 & nrow(o_31)==0 & nrow(o_32)==0 # & nrow(s_nga)==0 & nrow(o_31_nga)==0 
     & nrow(o_312)==0){#nrow(o_32_nga)==0 & & nrow(o_312_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_31)>0 & nrow(o_32)>0 #& nrow(s_nga)>0  & nrow(o_31_nga)>0 
           & nrow(o_312)>0, "No protein families identified") #nrow(o_32_nga)>0 & & nrow(o_312_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_31)>0 | nrow(o_32)>0 #| nrow(s_nga)>0 | nrow(o_31_nga)>0 
            | nrow(o_312)>0){ #nrow(o_32_nga)>0 |  | nrow(o_312_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#1f78b4'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3 unassigned") 
    # }
    # if(nrow(o_31_nga)>0){
    #   p <- add_markers(p, data = o_31_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3&1 unassigned") 
    # }
    # if(nrow(o_32_nga)>0){
    #   p <- add_markers(p, data = o_32_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3&2 unassigned") 
    # }
    # if(nrow(o_312_nga)>0){
    #   p <- add_markers(p, data = o_312_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f31&1 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_31)>0){
      p <- add_markers(p, data = o_31, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_32)>0){
      p <- add_markers(p, data = o_32, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_312)>0){
      p <- add_markers(p, data = o_312, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}

compare_two_files_pf_cc_size <- function(orig, subset, o_31, o_32, o_312, increase){ #s_nga, o_31_nga, o_32_nga, o_312_nga, 
  if(nrow(subset)==0 & nrow(o_31)==0 & nrow(o_32)==0 # & nrow(s_nga)==0 & nrow(o_31_nga)==0 
     & nrow(o_312)==0){#nrow(o_32_nga)==0 & & nrow(o_312_nga)==0
    validate(
      need(nrow(subset)>0 & nrow(o_31)>0 & nrow(o_32)>0 #& nrow(s_nga)>0  & nrow(o_31_nga)>0 
           & nrow(o_312)>0, "No protein families identified") #nrow(o_32_nga)>0 & & nrow(o_312_nga)>0
    )
  } else if(nrow(subset)>0 | nrow(o_31)>0 | nrow(o_32)>0 #| nrow(s_nga)>0 | nrow(o_31_nga)>0 
            | nrow(o_312)>0){ #nrow(o_32_nga)>0 |  | nrow(o_312_nga)>0
    p <- plot_ly(showlegend = T)
    p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                     marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                     opacity = 0.9,
                     text = ~paste(gene), hoverinfo = "text", showlegend = F)
    # if(nrow(s_nga)>0){
    #   p <- add_markers(p, data = s_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#1f78b4'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3 unassigned") 
    # }
    # if(nrow(o_31_nga)>0){
    #   p <- add_markers(p, data = o_31_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#8c6bb1'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3&1 unassigned") 
    # }
    # if(nrow(o_32_nga)>0){
    #   p <- add_markers(p, data = o_32_nga, x = ~logFC, y = ~-log10(pvalue), 
    #                    marker = list(size = 6, symbol = 2, color = c('#41ab5d'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text", 
    #                    name="f3&2 unassigned") 
    # }
    # if(nrow(o_312_nga)>0){
    #   p <- add_markers(p, data = o_312_nga, x = ~logFC, y = ~-log10(pvalue),
    #                    marker = list(size = 6, symbol = 2, color = c('#d9d9d9'), opacity = 0.4, line = list(width=0.9, color = "black")),
    #                    text = ~paste(gene, name, sep = "  "), hoverinfo="text",
    #                    name="f31&1 unassigned")
    # }
    if(nrow(subset)>0){
      p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                       marker = list(color = "#1f78b4", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_31)>0){
      p <- add_markers(p, data = o_31, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#8c6bb1", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_32)>0){
      p <- add_markers(p, data = o_32, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#41ab5d", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
    if(nrow(o_312)>0){
      p <- add_markers(p, data = o_312, x = ~logFC, y = ~-log10(pvalue),
                       marker = list(color = "#d9d9d9", size = ~increase*frequency, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                       opacity = 0.9, color = ~new_f,
                       text = ~paste(gene, overlay, frequency, sep = "  "), hoverinfo = "text")
    }
  }
  p
}
