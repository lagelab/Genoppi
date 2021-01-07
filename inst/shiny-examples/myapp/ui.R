body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidRow(
  tabItems(
    tabItem(tabName = "dashboard",
            shinyjs::useShinyjs(),
            tabsetPanel(id = "basic", #width = 12,
                        tabPanel("Basic plotting", id = 'basicplot', value = "p1",
                                 br(),
                                 column(width = 4,
                                        box(
                                          title = "Basic plot options", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE,
                                          fluidRow(
                                            column(12, uiOutput('a_select_scatterplot_ui')) 
                                          ),
                                          fluidRow(
                                            column(6, uiOutput("a_color_theme_indv_sig")),
                                            column(6, uiOutput("a_color_theme_indv_insig"))
                                          )
                                        ),
                                        box(
                                          title = "Summary", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE,
                                          fluidRow(
                                            column(12, uiOutput("VP_count_text")),
                                            br(),
                                          ),
                                          fluidRow(
                                            column(12, tableOutput("a_verbatim_count_ui")),
                                            br()
                                          ),
                                          fluidRow(
                                            br(),
                                            column(12, uiOutput('a_replicate_summar_text_ui')),
                                          ),
                                          fluidRow(
                                            column(12, tableOutput("a_replicate_summary_table_ui"))
                                          ),
                                          br(),
                                          fluidRow(
                                            column(12, uiOutput("a_monitor_pulldown_ui"))
                                          ),
                                          fluidRow(
                                            column(12, uiOutput("a_monitor_pulldown_mapping_ui"))
                                          ),
                                          fluidRow(
                                            br(),
                                            column(12, shinyjs::hidden(myDownloadButton("a_mttest_mapping_download", 'Proteomic data', icon("download"))))
                                          )
                                        )#,
                                        #shinyjs::hidden(box( #
                                        #  id='box_mapping_warning_ui', title = "Error!", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = FALSE,
                                        #  fluidRow(
                                        #    column(12, uiOutput('a_in_pulldown_check_ui'))
                                        #  )
                                        #))
                                 ),
                                 column(width = 8,
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                            column(11, shinyjs::hidden(myDownloadButton("a_volcano_plot_download", 'Volcano plot')))
                                          ),
                                          fluidRow(style = "padding-bottom:75px",
                                            #column(1, plotOutput("FDR_colorbar", width = "50px")),
                                            column(12, shinycssloaders::withSpinner(plotlyOutput("VolcanoPlot"), spinner_type)) #, width = "550px", height = "550px"
                                          ),
                                        ),
                                        box(
                                          title = tagList(img(src='icon_scatter.png',width='22px'), "Scatter plot"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = FALSE,
                                          fluidRow(
                                            column(11, shinyjs::hidden(myDownloadButton("a_scatter_plot_download", 'Scatter plot')))
                                          ),
                                          fluidRow(
                                            column(11, shinycssloaders::withSpinner(plotlyOutput("ScatterPlot"), spinner_type)) #, width = "550px", height = "550px"
                                          )
                                        )
                                 )
                        ),
                        tabPanel("Integrated plotting", id = 'integratedplot', value = "p2", 
                                br(),
                                 column(width = 4, 
                                    box(
                                     title = "PPI Databases", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE, # tagList(img(src='icon_inweb.png',width='20px'), 
                                     fluidRow(
                                       column(12, 
                                              uiOutput("a_ppi_select_ui"))
                                     ),
                                     # InWeb
                                     fluidRow(
                                       column(8, uiOutput("a_bait_layer")),
                                       column(4, 
                                              uiOutput("a_inweb_type"),
                                              uiOutput("a_bioplex_type_ui"),
                                              uiOutput("a_irefindex_type_ui")
                                              )
                                     ),
                                     
                                     # footer
                                     fluidRow(
                                       column(4, uiOutput("a_overlay_inweb_ui"), uiOutput("a_inweb_message")),
                                       column(4, uiOutput("a_label_inweb_ui")),
                                       column(4, shinyjs::hidden(myDownloadButton("a_ppi_mapping_df_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                     ),
                                     fluidRow(
                                       column(12, uiOutput("info_inweb_ui"))
                                     )
                                    ),
                                    box(
                                      title = "GWAS catalog", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(
                                        column(12, uiOutput('a_gwas_catalogue_ui'))
                                      ),
                                      fluidRow(
                                        column(4, uiOutput("a_overlay_gwas_cat_ui")),
                                        column(4, uiOutput("a_label_gwas_cat_ui")),
                                        column(4, shinyjs::hidden(myDownloadButton("a_gwas_catalogue_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput('a_gwas_subset_traits_by_data'))
                                        #column(6, uiOutput('a_gwas_subset_traits_by_data_freq'))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("info_gwas_ui"))
                                      )
                                    ),
                                    box(
                                      title = tagList('gnomAD'), width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(
                                        #column(4,uiOutput('a_select_gnomad_pli_type_ui')),
                                        column(12, 
                                               uiOutput('a_slide_gnomad_pli_threshold_ui'),
                                               uiOutput('a_gnomad_colorscale_text_ui'),
                                               #plotOutput('a_gnomad_colorscale_ui', width = "275px", height = '100px')
                                        )
                                      ),
                                      fluidRow(
                                        column(4, uiOutput("a_overlay_gnomad_ui")),
                                        column(4, uiOutput("a_label_gnomad_ui")),
                                        column(4, shinyjs::hidden(myDownloadButton("a_gnomad_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("info_gnomad_ui"))
                                      )
                                      #fluidRow(column(12, uiOutput('a_gnomad_constraints_available_ui'))),
                                      #fluidRow(column(12, tableOutput("a_table_gnomad_constraints_ui")))
                                    ),
                                    box(
                                      title = "GTEx or HPA", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(
                                        column(12, uiOutput('a_tissue_select_ui'),
                                               uiOutput("a_gtex_rna_tissue_ui"),
                                               uiOutput("a_gtex_protein_tissue_ui"),
                                               uiOutput("a_hpa_rna_tissue_ui")),
                                      ),
                                      fluidRow(
                                        column(4, uiOutput("a_overlay_tissue_ui")),
                                        column(4, uiOutput("a_label_tissue_ui")),
                                        column(4, shinyjs::hidden(myDownloadButton("a_tissue_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("info_tissue_ui"))
                                      )
                                    ),
                                    box(
                                     title ='Upload SNPs', width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                     fluidRow(
                                       column(12, uiOutput("a_SNP_file"))
                                     ),
                                     fluidRow(
                                       column(4, uiOutput("a_overlay_snp_ui")),
                                       column(4, uiOutput("a_label_snp_ui")),
                                       column(4, shinyjs::hidden(myDownloadButton("a_snp_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                     )
                                   ),
                                  box(
                                     title = "Upload genes", width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                     fluidRow(
                                        column(12, uiOutput("a_genes_file")),
                                     ),
                                     fluidRow(
                                       column(4, uiOutput("a_overlay_genes_upload_ui")),
                                       column(4, uiOutput("a_label_genes_upload_ui")),
                                       column(4, shinyjs::hidden(myDownloadButton("a_gene_upload_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                     )
                                  ),
                                  box(
                                    title = "Settings", width = NULL, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
                                    fluidRow(
                                      column(3, align = 'left',  h5(strong('Overlay'))),
                                      column(3, align = 'center',  h5(strong(uiOutput("a_sig_text_ui")))),
                                      column(3, align = 'center',  h5(strong(uiOutput("a_insig_text_ui")))),
                                      column(3, align = 'center',  h5(strong('Symbol'))),
                                    ),
                                    #fluidRow(column(12, h5('InWeb'))),
                                    fluidRow(
                                      column(3, h5('PPI Database')),
                                      column(3, uiOutput("a_color_inweb_sig_ui")),
                                      column(3, uiOutput("a_color_inweb_insig_ui")),
                                      column(3, uiOutput("a_symbol_inweb_ui"))
                                    ),
                                    #fluidRow(column(12, h5('GWAS catalogue'))),
                                    fluidRow(
                                      column(3, h5('GWAS catalog')),
                                      column(3, uiOutput("a_color_gwas_cat_sig_ui")),
                                      column(3, uiOutput("a_color_gwas_cat_insig_ui")),
                                      column(3, uiOutput("a_symbol_gwas_cat_ui"))
                                    ),
                                    #fluidRow(column(12, h5('gnomAD'))),
                                    fluidRow(
                                      column(3, h5('gnomAD')),
                                      column(3, uiOutput("a_color_gnomad_sig_ui")),
                                      column(3, uiOutput("a_color_gnomad_insig_ui")),
                                      column(3, uiOutput("a_symbol_gnomad_ui"))
                                    ),
                                    fluidRow(
                                      column(3, h5('GTEx or HPA')),
                                      column(3, uiOutput("a_color_tissue_sig_ui")),
                                      column(3, uiOutput("a_color_tissue_insig_ui")),
                                      column(3, uiOutput("a_symbol_tissue_ui"))
                                    ),
                                    #fluidRow(column(12, h5('SNPs upload'))),
                                    fluidRow(
                                      column(3, h5('SNPs upload')),
                                      column(3, uiOutput("a_color_snp_sig_ui")),
                                      column(3, uiOutput("a_color_snp_insig_ui")),
                                      column(3, uiOutput("a_symbol_snp_ui"))
                                    ),
                                    #fluidRow(column(12, h5('genes upload'))),
                                    fluidRow(
                                      column(3, h5('Genes upload')),
                                      column(3, uiOutput("a_color_genes_upload_sig_ui")),
                                      column(3, uiOutput("a_color_genes_upload_insig_ui")),
                                      column(3, uiOutput("a_symbol_genes_upload_ui"))
                                    )
                                  )
                                 ),
                                column(width = 8,
                                    box(
                                      title = tagList(shiny::icon('chart-area'), "Volcano plot"), solidHeader = TRUE, status = 'success', collapsible = TRUE, width = 12,
                                      # height = '800px'
                                          fluidRow(
                                            column(12, shinyjs::hidden(myDownloadButton("a_integrated_plot_download",'Volcano plot')))
                                          ),
                                          fluidRow(style = "padding-bottom:150px",
                                           column(12, shinycssloaders::withSpinner(plotlyOutput("Multi_VolcanoPlot"), spinner_type)),
                                         )
                                    ),
                                    tabBox(
                                      title = tagList(img(src='icon_venn_a.png',width='22px'), 'Venn diagrams'),  width = 12, #status = 'success', collapsible = TRUE,
                                      tabPanel('PPI Database',
                                        fluidRow(
                                          column(4, plotOutput('a_inweb_venn_ui', width = "220px", height = "220px")),
                                          column(5, br(), br(), br(), br(), uiOutput("a_ppi_venn_verbatim_ui")),
                                          column(3, shinyjs::hidden(myDownloadButton("a_inweb_venn_mapping_download", 'Genes', icon("download"))))
                                         ),
                                      ),
                                      tabPanel('GWAS catalog',
                                               fluidRow(
                                                 column(4, plotOutput('a_gwas_catalogue_venn_all_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(), uiOutput('a_gwas_catalogue_venn_verbatim_ui')),
                                                 column(3, shinyjs::hidden(myDownloadButton("a_gwas_catalogue_venn_mapping_download", 'Genes', icon("download"))))
                                               ),
                                      ),
                                      tabPanel('gnomAD',
                                               fluidRow(
                                                 column(4, plotOutput('a_gnomad_venn_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(), uiOutput("a_gnomad_venn_verbatim_ui")),
                                                 column(3, shinyjs::hidden(myDownloadButton("a_gnomad_venn_mapping_download", 'Genes', icon("download"))))
                                               )
                                      ),
                                      tabPanel('GTEx or HPA',
                                               fluidRow(
                                                 column(4, plotOutput('a_tissue_venn_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(), uiOutput("a_tissue_venn_verbatim_ui")),
                                                 column(3, shinyjs::hidden(myDownloadButton("a_tissue_venn_mapping_download", 'Genes', icon("download"))))
                                               ),
                                      ),
                                      tabPanel('SNPs upload',
                                         fluidRow(
                                          column(4, plotOutput('a_snp_venn_ui', width = "220px", height = "220px")),
                                          column(5, br(), br(), br(), br(),
                                                    uiOutput('a_snp_venn_verbatim_ui')),
                                          column(3, uiOutput('a_select_venn_snp_ui'),
                                                    uiOutput('a_select_venn_snp_loci_ui'),
                                                    shinyjs::hidden(myDownloadButton("a_snp_venn_mapping_download", 'Genes', icon("download"))))
                                         )
                                      ),
                                      tabPanel('Genes upload',
                                               fluidRow(
                                                 column(4, plotOutput('a_genes_upload_venn_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(),
                                                        uiOutput('a_genes_upload_venn_verbatim_ui')),
                                                 column(3, uiOutput('a_select_venn_genes_upload_ui'),
                                                           shinyjs::hidden(myDownloadButton("a_genes_upload_venn_mapping_download", 'Genes', icon("download")))),
                                               ),
                                      )
                                    )
                                )
                        ),
                        tabPanel("Gene set annotations", value = "p4",
                                 br(),
                                 column(4,
                                    box(
                                      title = tagList("Settings"), width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                      fluidRow(
                                        column(12, uiOutput("a_pf_loc_selection")),
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("a_pathway_mapping_freq_slider_ui"))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("a_pathway_mapping_search_ui"))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("a_pathway_mapping_type_sort_ui"))
                                      ),
                                      fluidRow(
                                        column(8,''),
                                        column(4, shinyjs::hidden(myDownloadButton("a_pathway_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                      ),
                                      fluidRow(
                                        column(12, uiOutput("info_geneset_ui"))
                                      )
                                   )
                                 ),
                                 column(8,
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                            column(3, shinyjs::hidden(myDownloadButton("a_pathway_plot_download", 'Volcano plot'))),
                                            column(3, shinyjs::hidden(myDownloadButton("a_pathway_plot_legend_download", 'Volcano plot (legend)'))),
                                            column(6, '')
                                          ),
                                          fluidRow(style = "padding-bottom:125px",
                                            column(12, shinycssloaders::withSpinner(plotlyOutput('VolcanoPlotPathway'), spinner_type))
                                          )
                                        ),
                                        box(
                                          title = tagList("Annotation table"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                                   column(12, DT::dataTableOutput('a_pathway_data_table_ui'))
                                          )
                                        )
                                 )
                        ),
                        tabPanel("Tissue enrichment", value = "p4",
                                 br(),
                                 column(4,
                                      box(
                                        title = tagList('Settings'), width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                        column(12, 
                                          fluidRow(
                                            uiOutput('a_tissue_select_source_ui'),
                                            uiOutput('a_tissue_enrichment_upload_ui'),
                                            uiOutput('a_tissue_enrichment_type_select_ui'),
                                            uiOutput('a_tissue_enrichment_slider_ui'),  
                                            uiOutput('a_tissue_enrichment_xaxis_ui'),
                                            uiOutput('a_tissue_enrichment_scientific_notation_ui'),
                                            shinyjs::hidden(myDownloadButton("a_tissue_enrichment_download",'Tissue enrichment', img=icon('file-alt', lib = "font-awesome"))),
                                            uiOutput("info_tissue_enrichment_ui")
                                          )
                                        )
                                      )
                                 ),
                                 column(8,
                                        box(
                                          title = tagList('Enrichment bar plot'), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, height = 1000,
                                          fluidRow(
                                            column(12,
                                                   shinycssloaders::withSpinner(plotlyOutput('a_tissue_enrichment_ui'), spinner_type)
                                                   )
                                          )
                                        )
                                 )
                                      
                        ),
                        tabPanel("Inspect uploaded data (dev)", value = "p5",
                            fluidRow(
                              column(12,
                                     DT::dataTableOutput('a_file_display_table_ui')  
                                     )
                            )
                            
                        )
            )
    ),
    
    tabItem(tabName = "widgets",
            shinyjs::useShinyjs(),
            tabsetPanel(id = "comparison",
                        tabPanel("Basic plotting", 
                                 br(),
                                 column(4,
                                        box(
                                          title = 'Settings for file 1', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(6, 
                                                          uiOutput("b_file_1_significance_type_ui"),
                                                          uiOutput("b_file_1_FDR_thresh"),
                                                          uiOutput("b_file_1_PVal_thresh")
                                                   ),
                                                   column(6,
                                                          uiOutput('b_file_1_logfc_direction_ui'),
                                                          uiOutput("b_file_1_logFC_thresh")
                                                   )
                                                 ),
                                                 fluidRow(
                                                   uiOutput('b_file_1_select_scatterplot_ui')
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'Summary for file 1', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, 
                                                          uiOutput("b_file_1_summary_text_ui")
                                                   ),
                                                   column(12,
                                                          tableOutput('b_file_1_summary_table_ui')
                                                   ),
                                                   column(12, 
                                                          shinyjs::hidden(myDownloadButton("b_file_1_mapping_download", 'Proteomic data', icon("download"))))
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_1_volcano_download", 'Volcano plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_1_volcano'), spinner_type),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(img(src='icon_scatter.png',width='22px'), "Scatter plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_1_scatter_download", 'Scatter plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_1_scatter'), spinner_type),
                                                 )
                                          )
                                        )
                                 ),
                                 column(4,
                                        box(
                                          title = 'Settings for file 2', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(6, 
                                                          uiOutput("b_file_2_significance_type_ui"),
                                                          uiOutput("b_file_2_FDR_thresh"),
                                                          uiOutput("b_file_2_PVal_thresh")
                                                   ),
                                                   column(6,
                                                          uiOutput('b_file_2_logfc_direction_ui'),
                                                          uiOutput("b_file_2_logFC_thresh")
                                                   )
                                                 ),
                                                 fluidRow(
                                                   uiOutput('b_file_2_select_scatterplot_ui')
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'Summary for file 2', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, 
                                                          uiOutput("b_file_2_summary_text_ui")
                                                   ),
                                                   column(12,
                                                          tableOutput('b_file_2_summary_table_ui')
                                                   ),
                                                   column(12,
                                                          shinyjs::hidden(myDownloadButton("b_file_2_mapping_download", 'Proteomic data', icon("download")))
                                                          )
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_2_volcano_download", 'Volcano plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_2_volcano'), spinner_type),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(img(src='icon_scatter.png',width='22px'), "Scatter plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_2_scatter_download", 'Scatter plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_2_scatter'), spinner_type),
                                                 )
                                          )
                                        )
                                 ),
                                 column(4,
                                        box(
                                          title = 'Settings for file 3', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(6, 
                                                          uiOutput("b_file_3_significance_type_ui"),
                                                          uiOutput("b_file_3_FDR_thresh"),
                                                          uiOutput("b_file_3_PVal_thresh")
                                                          ),
                                                   column(6,
                                                          uiOutput('b_file_3_logfc_direction_ui'),
                                                          uiOutput("b_file_3_logFC_thresh")
                                                          )
                                                 ),
                                                 fluidRow(
                                                   uiOutput('b_file_3_select_scatterplot_ui')
                                                 )
                                                )
                                        ),
                                        box(
                                          title = 'Summary for file 3', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, 
                                                          uiOutput("b_file_3_summary_text_ui")
                                                   ),
                                                   column(12,
                                                          tableOutput('b_file_3_summary_table_ui')
                                                   ),
                                                   column(12,
                                                          shinyjs::hidden(myDownloadButton("b_file_3_mapping_download", 'Proteomic data', icon("download")))
                                                          )
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_3_volcano_download", 'Volcano plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_3_volcano'), spinner_type),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = tagList(img(src='icon_scatter.png',width='22px'), "Scatter plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   column(12, shinyjs::hidden(myDownloadButton("b_file_3_scatter_download", 'Scatter plot')))
                                                 ),
                                                 fluidRow(
                                                   shinycssloaders::withSpinner(plotlyOutput('b_file_3_scatter'), spinner_type),
                                                 )
                                          )
                                        )
                                )
                         
                        ),
                        tabPanel("Venn diagrams", 
                                 br(),
                                 column(6,
                                   box(
                                     title = tagList(img(src='icon_venn_a.png',width='22px'), 'Venn diagrams'), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                     column(1, ''),
                                     column(8,
                                            fluidRow(
                                              plotOutput('b_file_comparison_venn_ui', width = "280px", height = "280px")
                                            ),
                                            fluidRow(
                                              uiOutput('b_file_comparison_venn_verbatim_ui')
                                            )
                                     ),
                                     column(3, '')
                                   ),
                                   box(
                                     title = 'Venn diagram explanations', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                     column(12,
                                            fluidRow(
                                              uiOutput('b_file_comparison_venn_explanations_ui')
                                            )
                                     )
                                   )
                                   
                                 ),
                                 column(6,
                                        box(
                                          title = 'Explore overlap', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          fluidRow(
                                            column(4, 
                                                   uiOutput('b_file_comparison_data_table_select_ui')   
                                                   ),
                                            column(4,
                                                   myDownloadButton("b_file_comparison_data_table_download_ui", 'Genes', icon("download"))
                                                   ),
                                            column(4,
                                                   ''
                                                   )
                                          ),
                                          fluidRow(
                                            column(12,
                                                   DT::dataTableOutput('b_file_comparison_data_table_ui')
                                            )
                                          )
                                        )
                                        
                                 )
                                 
                        )
            
            )
    ),
    tabItem(tabName = "guide",
            
            # works in browser format
            tags$iframe(src = "welcome_guide_200509.pdf",
                                           style="width:100%;",  #frameborder="0"
                                           height = "3100px")
    ),
    tabItem(tabName = "start",
            fluidRow(
              column(3, ''),
              column(6, 
                # top of table
                fluidRow(style = "padding-top:200px",
                  column(12, align = 'center',
                         h1(strong("Genoppi"), style = "font-size:100px;"),
                         br(),
                         uiOutput('a_example_file_ui'),
                         uiOutput('a_get_example_file_ui')
                    )
                  ),
                  fluidRow(
                    br(),
                    h5(HTML(paste('<b>Genoppi</b> is an open-source software for performing quality control and 
                             analyzing quantitative proteomic data. Genoppi streamlines the integration of 
                             proteomic data with external datasets such as known protein-protein interactions 
                             in published literature, data from genetic studies, gene set annotations, or other 
                             user-defined inputs. See the', actionLink('tab_welcome','welcome guide'), 
                             'for more details.') ))
                    ),
                     fluidRow(style = "padding-top:300px",
                         column(12, align = 'center',
                                h6(HTML(paste0(actionLink('enable_dev_mode', 'Developer mode'))))
                               )
                             ),
                  ),
              column(3, '')
            )
    ),
    tabItem(tabName = "documentation",
            fluidRow(
              column(1, ''),
              column(10, 
                     # top of table
                     fluidRow(style = "padding-top:100px",
                              column(12, align = 'left',
                                     h1(strong("Data documentation"), style = "font-size:30px;"),
                                     br(),
                                     uiOutput('documentation_ui')
                              )
                     )
              ),
              column(1, '')
            )
    )
   )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              #h6(as.character(genoppi.ver)),
              menuItem("Quick Start", tabName = "start", icon = icon("stream")),
              menuItem("Guide", tabName = "guide", icon = icon("info-circle")),
              menuItem("Data Documentation", icon = icon("question-circle"), tabName = "documentation", badgeLabel = "new", badgeColor = "green"),
              menuItem("Single File", tabName = "dashboard", icon = icon("file")),
              menuItem("Multi Files Comparison", icon = icon("copy"), tabName = "widgets"),
              conditionalPanel("input.sidebarmenu === 'dashboard'",
                               uiOutput("a_file"),
                               uiOutput("a_in_pulldown_check_ui"),
                               #uiOutput('a_make_example_file'),
                               HTML('<hr style="border-color: #D6DBE0;">'),
                               uiOutput("a_bait_search"),
                               uiOutput("a_GOI_search"),
                               uiOutput("a_GOI_search_alpha"),
                               #uiOutput("a_color_scheme"),
                               uiOutput("a_color_style"),
                               uiOutput("a_file_color"),
                               uiOutput("a_significance_type_ui"),
                               uiOutput("FDR_thresh"),
                               uiOutput("PVal_thresh"),
                               uiOutput('a_logfc_direction_ui'),
                               uiOutput("logFC_thresh")
              ),
              conditionalPanel("input.sidebarmenu == 'widgets'",
                               uiOutput("b_file_1_ui"),
                               uiOutput("b_file_2_ui"),
                               uiOutput("b_file_3_ui"),
                               HTML('<hr style="border-color: #D6DBE0;">'),
                               uiOutput("b_GOI_search"),
                               uiOutput("b_GOI_search_alpha")
              ),
              br(),
              tags$footer(
                tags$p(paste('version',genoppi.ver), style = "color: grey; text-align: center;")
              )
                
              
  )
)


# Put them together into a dashboardPage
dashboardPage(skin = "black",
              #header,
              dashboardHeader(title = 'Genoppi'),
              sidebar,
              body
)

