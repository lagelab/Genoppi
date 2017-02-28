#Created 11/1/16 by April Kim
#Functions file for Shiny app Genoppi
calculate_moderated_ttest <- function(input_file){
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

#Blue to green gradient
separate_to_groups_for_color <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold+0.01)
  thresh <- subset(vp_data, (FDR > threshold & FDR <= threshold+0.01))
  below_thresh <- below_thresh[order(below_thresh$FDR), ]
  above_thresh <- above_thresh[order(above_thresh$FDR), ]
  thresh <- thresh[order(thresh$FDR), ]
  a_col <- colorRampPalette(c('lightskyblue1', 'blue'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c('grey100', "lightgreen", "darkgreen"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  c_col <- colorRampPalette(c('lightskyblue1', 'grey100'))(nrow(thresh))
  thresh <- cbind(thresh, c_col)
  names(thresh)[names(thresh) == 'c_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh, thresh)
}

#Grayscale gradient
separate_to_groups_for_cbf <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold+0.01)
  thresh <- subset(vp_data, (FDR > threshold & FDR <= threshold+0.01))
  below_thresh <- below_thresh[order(below_thresh$FDR), ]
  above_thresh <- above_thresh[order(above_thresh$FDR), ]
  thresh <- thresh[order(thresh$FDR), ]
  a_col <- colorRampPalette(c('#737373', '#525252'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c('#F2F2F2', '#AAAAAA'))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  c_col <- colorRampPalette(c('#FCFCFC', '#ffffff'))(nrow(thresh))
  thresh <- cbind(thresh, c_col)
  names(thresh)[names(thresh) == 'c_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh, thresh)
}

#Blue and green separate
separate_to_groups_for_color_integrated <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold)
  a_col <- colorRampPalette(c('seagreen3', 'seagreen3'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("royalblue2", "royalblue2"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

#Blue and green separate, specifically in case plot opacity is low
separate_to_groups_for_color_integrated_bar <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold)
  #
  a_col <- colorRampPalette(c('seagreen3', 'seagreen3'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("royalblue2", "royalblue2"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

#Two gray tones
separate_to_groups_for_cbf_integrated <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold)
  a_col <- colorRampPalette(c('#525252', '#525252'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("grey76", "grey76"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

#Two gray tones, specifically in case plot opacity is low
separate_to_groups_for_cbf_integrated_bar <- function(vp_data, threshold){
  below_thresh <- subset(vp_data, FDR <= threshold)
  above_thresh <- subset(vp_data, FDR > threshold)
  a_col <- colorRampPalette(c('#969696', '#969696'))(nrow(below_thresh))
  below_thresh <- cbind(below_thresh, rev(a_col))
  names(below_thresh)[names(below_thresh) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("#d9d9d9", "#d9d9d9"))(nrow(above_thresh))
  above_thresh <- cbind(above_thresh, b_col)
  names(above_thresh)[names(above_thresh) == 'b_col'] <- 'col'
  data <- rbind(below_thresh, above_thresh)
}

separate_to_groups_for_exac_bar <- function(vp_data){
  grp1 <- subset(vp_data, FDR <= 0.33)
  grp2 <- subset(vp_data, 0.33 < FDR & FDR <= 0.66)
  grp3 <- subset(vp_data, 0.66 < FDR & FDR <= 1)
  a_col <- colorRampPalette(c('#fc8d59', '#fc8d59'))(nrow(grp1))
  grp1 <- cbind(grp1, rev(a_col))
  names(grp1)[names(grp1) == 'rev(a_col)'] <- 'col'
  b_col <- colorRampPalette(c("#99d594", "#99d594"))(nrow(grp2))
  grp2 <- cbind(grp2, b_col)
  names(grp2)[names(grp2) == 'b_col'] <- 'col'
  c_col <- colorRampPalette(c("#ffffbf", "#ffffbf"))(nrow(grp3))
  grp3 <- cbind(grp3, c_col)
  names(grp3)[names(grp3) == 'c_col'] <- 'col'
  data <- rbind(grp1, grp2, grp3)
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
 
#volcano - lower opacity to highlight different layers
plot_volcano_multiple_cond <- function(d){
  p <- plot_ly(showlegend = FALSE, height = 350, width = 320) 
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~logFC, y = ~-log10(pvalue),
                     # marker = list(size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1, color = ~col),
                     # opacity = 0.5, 
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
                     # marker = list(size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1, color = ~col),
                     # opacity = 0.5, 
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.8, 
                     text = ~paste0(gene, ", FDR=", signif(FDR, digits = 3)), hoverinfo = "text", name = "pull down")
  }
  p
}

#volcano - exac colorscale
plot_volcano_exac <- function(b, a, n){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550)
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
}

#volcano - exac colorscale
plot_volcano_exac_multi <- function(b, a, n){
  p <- plot_ly(showlegend = F, height = 320, width = 320) #, height = 320
  p <- add_markers(p, data = b, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = a, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = n, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                   opacity = 0.8, 
                   text = ~paste(gene), hoverinfo = "text")
}

#volcano - search/found highlight
search_volcano <- function(p, found){
  p %>%
    add_trace(data = found, x = ~logFC, y = ~-log10(pvalue), 
              mode = "markers+text", hoverinfo="text+x+y", text = ~paste(gene), 
              marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
              textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
              type = 'scatter', showlegend = FALSE)
}

#scatter - opacity = 1 colorscale
plot_scatter_qc <- function(orig, d){
  p <- plot_ly(showlegend = FALSE, width = 550, height = 550) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 text = "x=y", hoverinfo = "text",
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  for(i in nrow(d)){
    p <- add_markers(p, data = d, x = ~rep1, y = ~rep2, 
                     marker = list(size = 7, cmin = 0, cmax = 1, color = ~col, line = list(width=0.2, color = "grey89")), 
                     opacity = 0.9, 
                     text = ~paste0(gene, ", rep1=", rep1, ", rep2=", rep2), hoverinfo = "text", name = "pull down")
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
                     text = ~paste0(gene, ", rep1=", rep1, ", rep2=", rep2), hoverinfo = "text", name = "pull down")
  }
  p
}

#scatter - exac colorscale
plot_scatter_exac <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 550, height = 550) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
}

plot_scatter_exac_multi <- function(orig, b, a, n){
  p <- plot_ly(showlegend = F, width = 320, height = 320) 
  p <- add_lines(p, data = orig, x = ~c((min(rep1, rep2)), (max(rep1, rep2))), y = ~c((min(rep1, rep2)), (max(rep1, rep2))),
                 line = list(dash = "dash", width = 1, color = "#252525"), showlegend = FALSE)
  p <- add_markers(p, data = b, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = 'black'), cmin = 0, cmax = 1, color = "#fc8d59"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = a, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#99d594"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
  p <- add_markers(p, data = n, x = ~rep1, y = ~rep2, 
                   marker = list(size = 8, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1, color = "#ffffbf"),
                   opacity = 0.7, 
                   text = ~paste(gene), hoverinfo = "text")
}

search_scatter <- function(p, found){
  p %>%
    add_trace(data = found, x = ~rep1, y = ~rep2, 
              mode = "markers+text", hoverinfo="text+x+y", text = ~paste(gene), 
              marker = list(color = "#f7f4f9", size = 10, line = list(width=1.3, color = "#3f007d")),
              textposition = ~ifelse(logFC>0, "middle right", "middle left"), textfont = list(color='black', size = 10), 
              type = 'scatter', showlegend = FALSE)
}

vp_layer_for_inweb <- function(p, d_in){
  add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

vp_layer_for_inweb_no_text <- function(p, d_in){
  add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              # text = ~paste(gene), textposition =  ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

vp_layer_for_inweb_cbf <- function(p, d_in){
  add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              # mode = "markers",
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

vp_layer_for_inweb_cbf_no_text <- function(p, d_in){
  add_markers(p, data = d_in, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              # mode = "markers",
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

vp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

vp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

vp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

vp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

vp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

vp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

vp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

vp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~logFC, y= ~-log10(pvalue),
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

vp_layer_for_snp_to_gene_none <- function(p, d){
  add_markers(p, data = d, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3",
              name = "no SNP to gene")
}

vp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  add_markers(p, data = d, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3",
              name = "no SNP to gene")
}

vp_layer_for_uploaded_genes <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15, 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

vp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), #symbol = 15, 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text") 
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

vp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

vp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text") 
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

vp_layer_for_uploaded_genes_none <- function(p, d){
  add_markers(p, data = d, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2",
              name = "no genes of interest")
}

vp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  add_markers(p, data = d, x = ~logFC, y= ~-log10(pvalue),
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2",
              name = "no genes of interest")
}

sp_layer_for_inweb <- function(p, d_in){
  add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_inweb_no_text <- function(p, d_in){
  add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffff33", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_inweb_cbf <- function(p, d_in){
  add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_inweb_cbf_no_text <- function(p, d_in){
  add_markers(p, data = d_in, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1),
              mode = "markers+text", hoverinfo = "text", legendgroup = "group1",
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,"middle right","middle left"), textfont = list(size = 11),
              name = "InWeb") 
}

sp_layer_for_snp_to_gene_sgl <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_no_text <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#80cdc1", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = "square"
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}

sp_layer_for_snp_to_gene_sgl_cbf_no_text <- function(p, snp_sgl){
  add_markers(p, data = snp_sgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = "square"), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group3",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "SGL gene")
}


sp_layer_for_snp_to_gene_mgl <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_no_text <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#c2a5cf", size = 7, line = list(width=0.4, color = "black"), opacity = 1), #, symbol = 2
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_mgl_cbf_no_text <- function(p, snp_mgl){
  add_markers(p, data = snp_mgl, x = ~rep1, y= ~rep2,
              marker = list(color = "#ffffff", size = 7, line = list(width=0.4, color = "black"), opacity = 1, symbol = 2), 
              mode = "markers+text", hoverinfo = "text", legendgroup = "group4",
              # text = ~paste(gene, '</br>', snpid), textposition = ~ifelse(logFC>0,"top right","top left"), textfont = list(size = 11),
              name = "MGL gene")
}

sp_layer_for_snp_to_gene_none <- function(p, d){
  add_markers(p, data = d, x = ~rep1, y= ~rep2,
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3",
              name = "no SNP to gene")
}

sp_layer_for_snp_to_gene_none_cbf <- function(p, d){
  add_markers(p, data = d, x = ~rep1, y= ~rep2,
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group3",
              name = "no SNP to gene")
}

sp_layer_for_uploaded_genes <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), 
              color = ~factor(.id), colors = "Blues",
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_no_text <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 7), 
              color = ~factor(.id), colors = "Blues",
              mode = "markers+text", hoverinfo = "text") 
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_cbf <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 11, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text", 
              text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_cbf_no_text <- function(p, d_g2s){
  add_markers(p, data = d_g2s, x = ~rep1, y= ~rep2,
              marker = list(opacity = 1, line = list(width=0.6, color = "black"), size = 11, symbol = 15), 
              color = ~factor(.id), 
              mode = "markers+text", hoverinfo = "text") 
              # text = ~paste(gene), textposition = ~ifelse(logFC>0,'middle right','middle left'), textfont = list(size = 11))
}

sp_layer_for_uploaded_genes_none <- function(p, d){
  add_markers(p, data = d, x = ~rep1, y= ~rep2,
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2",
              name = "no genes of interest")
}

sp_layer_for_uploaded_genes_none_cbf <- function(p, d){
  add_markers(p, data = d, x = ~rep1, y= ~rep2,
              marker = list(opacity = 0),
              mode = "markers", legendgroup = "group2",
              name = "no genes of interest")
}

assignFamily_inc_doubles <- function(d){
  ### exclude bait
  # tmp <- subset(d, gene != bait)
  tmp <- d
  ### save file with gene names
  write.table(tmp$gene, file = "data/t.txt", quote = FALSE, row.names = FALSE, col.names = FALSE)
  ### run sh script that assigns protein families and saves two files: one for genes that have families assigned and one for those that do not belong to any defined protein family
  system("scripts/find_families_inc-doubles.sh")
  ### read in families
  families <- read.csv("data/gene_families.txt", sep = "\t")
  system("rm data/gene_families.txt")
  ### subset data for genes that have families assigned
  d_families <- subset(d, gene %in% families$gene)
  d_families$family <- families$family[match(d_families$gene, families$gene)]
  ### alphabetic sort
  d_families <- d_families[with(d_families, order(family, decreasing = FALSE)),]
  ### count members of the same protein family
  tmp <- data.frame(table(d_families$family))
  d_families$frequency <- tmp$Freq[match(d_families$family, tmp$Var1)]
  d_families
}

addNames <- function(d){
  ### read genes that have no family assigned
  genesNotAs <- read.csv("data/genes_naf_wnames.txt", sep = "\t")
  system("rm data/genes_naf_wnames.txt")
  ### gna = gene not assigned
  d_gna <- subset(d, gene %in% genesNotAs$gene)
  d_gna$name <- genesNotAs$name[match(d_gna$gene, genesNotAs$gene)]
  d_gna
}

# compareSP <- function(d1,d2){
#   g = ggplot(data = rbind(d1, d2), mapping=aes(x = rep1, y = rep2, group = gene))+
#     geom_line(color = "dodgerblue")+
#     geom_point(data = d1, mapping=aes(x = rep1, y = rep2), alpha = 0.9, size = 1, color="black")+
#     geom_point(data = d2, mapping=aes(x = rep1, y = rep2), alpha = 0.9, size = 1, color="red")+
#     theme_bw() +
#     xlab("rep1") + ylab("rep2")+
#     geom_hline(yintercept = 0)+
#     geom_vline(xintercept = 0)+
#     geom_abline(intercept = 0, slope = 1, color = "gray")
#   g
#   ggplotly(g)
# }

compare_two_files_a <- function(orig, subset, overlaps){
  p <- plot_ly(showlegend = T, width = 300, height = 390)
  p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", showlegend = F)
  p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#e41a1c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "f1")
  p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 2")
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_b <- function(orig, subset, overlaps){
  p <- plot_ly(showlegend = T, width = 300, height = 390)
  p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", showlegend = F)
  p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "f2")
  p <- add_markers(p, data = overlaps, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#fdb462", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 2")
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_aa <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  p <- plot_ly(showlegend = T, width = 300, height = 390)
  p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", showlegend = F)
  p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ef3b2c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "f1")
  p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 2")
  p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 3")
  p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1, 2 & 3")
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_bb <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  p <- plot_ly(showlegend = T, width = 300, height = 390)
  p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", showlegend = F)
  p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffff33", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "f2")
  p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#fd8d3c", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 2")
  p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "2 & 3")
  p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1, 2 & 3")
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}

compare_two_files_cc <- function(orig, subset, overlaps1, overlaps2, overlaps3){
  p <- plot_ly(showlegend = T, width = 300, height = 390)
  p <- add_markers(p, data = orig, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#ffffff", size = 6, line = list(width=0.1, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", showlegend = F)
  p <- add_markers(p, data = subset, x = ~logFC, y = ~-log10(pvalue), 
                   marker = list(color = "#1f78b4", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "f3")
  p <- add_markers(p, data = overlaps1, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#8c6bb1", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1 & 3")
  p <- add_markers(p, data = overlaps2, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#41ab5d", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "2 & 3")
  p <- add_markers(p, data = overlaps3, x = ~logFC, y = ~-log10(pvalue),
                   marker = list(color = "#d9d9d9", size = 8, line = list(width=0.2, color = "black"), cmin = 0, cmax = 1),
                   opacity = 0.9,
                   text = ~paste(gene), hoverinfo = "text", name = "1, 2 & 3")
  p <- p %>% layout(legend = list(orientation = 'h', y = -0.23))
}
