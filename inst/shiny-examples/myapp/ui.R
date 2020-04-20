body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  fluidRow(
  tabItems(
    tabItem(tabName = "dashboard",
            shinyjs::useShinyjs(),
            tabsetPanel(id = "basic", #width = 12,
                        tabPanel("Basic plotting", value = "p1",
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
                                            column(12, tagList(h5(HTML(bold('Replicate corrlation(s):'))))),
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
                                            column(12, shinyjs::hidden(myDownloadButton("a_mttest_mapping_download", 'Pulldown data', icon("download"))))
                                          )
                                        ),
                                        shinyjs::hidden(box(
                                          id='box_mapping_warning_ui', title = "Gene mapping", width = NULL, solidHeader = TRUE, status = "warning", collapsible = TRUE, collapsed = FALSE,
                                          fluidRow(
                                            h6('Explore mapping of accession to gene symbol')
                                          )
                                        ))
                                 ),
                                 column(width = 8,
                                        box(
                                          title = tagList(shiny::icon('chart-area'), "Volcano plot"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                            column(11, shinyjs::hidden(myDownloadButton("a_volcano_plot_download", 'Volcano plot')))
                                          ),
                                          fluidRow(style = "padding-bottom:75px",
                                            #column(1, plotOutput("FDR_colorbar", width = "50px")),
                                            column(12, plotlyOutput("VolcanoPlot")) #, width = "550px", height = "550px"
                                          ),
                                        ),
                                        box(
                                          title = tagList(img(src='icon_scatter.png',width='22px'), "Scatter plot"), width = NULL, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = FALSE,
                                          fluidRow(
                                            column(11, shinyjs::hidden(myDownloadButton("a_scatter_plot_download", 'Scatter plot')))
                                          ),
                                          fluidRow(
                                            column(11, plotlyOutput("ScatterPlot")) #, width = "550px", height = "550px"
                                          )
                                        )
                                 )
                        ),
                        tabPanel("Integrated plotting", value = "p2", 
                                br(),
                                 column(width = 4, 
                                    box(
                                     title = tagList(img(src='icon_inweb.png',width='20px'), "InWeb InBiomap"), width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = FALSE,
                                     fluidRow(
                                       column(12, uiOutput("a_bait_layer"))
                                     ),
                                     fluidRow(
                                       column(8, uiOutput("a_label_inweb_ui")),
                                       column(4, shinyjs::hidden(myDownloadButton("a_inweb_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                     ),
                                    ),
                                    box(
                                      title = tagList(shiny::icon('list-alt'), "GWAS catalog"), width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(
                                        column(12, uiOutput('a_gwas_catalogue_ui'))
                                      ),
                                      fluidRow(
                                        column(8, uiOutput("a_label_gwas_cat_ui")),
                                        column(4, shinyjs::hidden(myDownloadButton("a_gwas_catalogue_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                       ),
                                      
                                    ),
                                    box(
                                      title = tagList('gnomAD'), width = NULL, solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = TRUE,
                                      fluidRow(
                                        column(4,uiOutput('a_select_gnomad_pli_type_ui')),
                                        column(8, 
                                               uiOutput('a_slide_gnomad_pli_threshold_ui'),
                                               uiOutput('a_gnomad_colorscale_text_ui'),
                                               #plotOutput('a_gnomad_colorscale_ui', width = "275px", height = '100px')
                                        )
                                      ),
                                      fluidRow(
                                        column(8, uiOutput("a_label_gnomad_ui")),
                                        column(4, shinyjs::hidden(myDownloadButton("a_gnomad_mapping_download",'Mapping', img=icon('file-alt', lib = "font-awesome"))))
                                      )
                                      #fluidRow(column(12, uiOutput('a_gnomad_constraints_available_ui'))),
                                      #fluidRow(column(12, tableOutput("a_table_gnomad_constraints_ui")))
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
                                    fluidRow(column(12, h5('InWeb'))),
                                    fluidRow(
                                      column(4, uiOutput("a_color_inweb_sig_ui")),
                                      column(4, uiOutput("a_color_inweb_insig_ui")),
                                      column(4, uiOutput("a_symbol_inweb_ui"))
                                    ),
                                    fluidRow(column(12, h5('GWAS catalogue'))),
                                    fluidRow(
                                      column(4, uiOutput("a_color_gwas_cat_sig_ui")),
                                      column(4, uiOutput("a_color_gwas_cat_insig_ui")),
                                      column(4, uiOutput("a_symbol_gwas_cat_ui"))
                                    ),
                                    fluidRow(column(12, h5('gnomAD'))),
                                    fluidRow(
                                      column(4, uiOutput("a_color_gnomad_sig_ui")),
                                      column(4, uiOutput("a_color_gnomad_insig_ui")),
                                      column(4, uiOutput("a_symbol_gnomad_ui"))
                                    ),
                                    fluidRow(column(12, h5('SNPs upload'))),
                                    fluidRow(
                                      column(4, uiOutput("a_color_snp_sig_ui")),
                                      column(4, uiOutput("a_color_snp_insig_ui")),
                                      column(4, uiOutput("a_symbol_snp_ui"))
                                    ),
                                    fluidRow(column(12, h5('genes upload'))),
                                    fluidRow(
                                      column(4, uiOutput("a_color_genes_upload_sig_ui")),
                                      column(4, uiOutput("a_color_genes_upload_insig_ui")),
                                      column(4, uiOutput("a_symbol_genes_upload_ui"))
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
                                           #column(1, br(), br(), br(), br(), plotOutput("FDR_colorbar_integrated", width = "50px")),
                                           column(12, plotlyOutput("Multi_VolcanoPlot")),
                                         )
                                    ),
                                    tabBox(
                                      title = tagList(img(src='icon_venn_a.png',width='22px'), 'Venn diagrams'),  width = 12, #status = 'success', collapsible = TRUE,
                                      tabPanel('InWeb',
                                        fluidRow(
                                          column(4, plotOutput('a_inweb_venn_ui', width = "220px", height = "220px")),
                                          column(5, br(), br(), br(), br(), uiOutput("a_inweb_venn_verbatim_ui")),
                                          column(3, shinyjs::hidden(myDownloadButton("a_inweb_venn_mapping_download", 'overlap', icon("download"))))
                                         ),
                                      ),
                                      tabPanel('GWAS catalog',
                                               fluidRow(
                                                 column(4, plotOutput('a_gwas_catalogue_venn_all_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(), uiOutput('a_gwas_catalogue_venn_verbatim_ui')),
                                                 column(3, shinyjs::hidden(myDownloadButton("a_gwas_catalogue_venn_mapping_download", 'overlap', icon("download"))))
                                               ),
                                      ),
                                      tabPanel('gnomAD',
                                               fluidRow(
                                                 column(4, plotOutput('a_gnomad_venn_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(), uiOutput("a_gnomad_venn_verbatim_ui")),
                                                 column(3, shinyjs::hidden(myDownloadButton("a_gnomad_venn_mapping_download", 'overlap', icon("download"))))
                                               )
                                      ),
                                      tabPanel('SNPs upload',
                                         fluidRow(
                                          column(4, plotOutput('a_snp_venn_ui', width = "220px", height = "220px")),
                                          column(5, br(), br(), br(), br(),
                                                    uiOutput('a_snp_venn_verbatim_ui')),
                                          column(3, uiOutput('a_select_venn_snp_ui'),
                                                    uiOutput('a_select_venn_snp_loci_ui'),
                                                    shinyjs::hidden(myDownloadButton("a_snp_venn_mapping_download", 'overlap', icon("download"))))
                                         )
                                      ),
                                      tabPanel('Genes upload',
                                               fluidRow(
                                                 column(4, plotOutput('a_genes_upload_venn_ui', width = "220px", height = "220px")),
                                                 column(5, br(), br(), br(), br(),
                                                        uiOutput('a_genes_upload_venn_verbatim_ui')),
                                                 column(3, uiOutput('a_select_venn_genes_upload_ui'),
                                                           shinyjs::hidden(myDownloadButton("a_genes_upload_venn_mapping_download", 'overlap', icon("download")))),
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
                                        #column(5, uiOutput("a_PF_sort_col")),
                                        #column(5, uiOutput("a_BPF_freq"))
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
                                      )
                                   )
                                 ),
                                 column(8,
                                        box(
                                          title = tagList("Volcano plot"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                            column(11, shinyjs::hidden(myDownloadButton("a_pathway_plot_download", 'Gene sets plot')))
                                          ),
                                          fluidRow(style = "padding-bottom:125px",
                                            column(12, plotlyOutput('VolcanoPlotPathway'))
                                          )
                                        ),
                                        box(
                                          title = tagList("Annotation table"), width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          fluidRow(
                                                   column(12, DT::dataTableOutput('a_pathway_data_table_ui'))
                                          )
                                        )
                                 )
                        )
            )
    ),
    
    tabItem(tabName = "widgets",
            shinyjs::useShinyjs(),
            tabsetPanel(id = "comparison",
                        tabPanel("Basic Plots", value = "3_p1",
                                 br(),
                                 fluidRow(
                                   column(3, uiOutput("c_prot_fam_db")),
                                   column(2, uiOutput("c_text_prot_fam_db")),
                                   column(3, uiOutput("c_color_theme_pf"))
                                 ),
                                 fluidRow(
                                   column(4, downloadButton("download_c1_pf_cleaned_input", "Remove selected PF from f1")),
                                   column(4, downloadButton("download_c2_pf_cleaned_input", "Remove selected PF from f2")),
                                   column(4, downloadButton("download_c3_pf_cleaned_input", "Remove selected PF from f3"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_color_setting_text"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_color_theme"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_color_theme_indv_sig")),
                                   column(4, uiOutput("c_color_theme_indv_insig"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, plotOutput("c_FDR_colorbar", height = "100px"))
                                 ),
                                 fluidRow(
                                   column(1, myDownloadButton("download_c_vp1_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp2_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp3_gg", "Volcano"))
                                 ),
                                 fluidRow(
                                   column(4, plotlyOutput("VolcanoPlot_c1")),
                                   column(4, plotlyOutput("VolcanoPlot_c2")),
                                   column(4, plotlyOutput("VolcanoPlot_c3"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(1, myDownloadButton("download_c_sp1_gg", "Scatter")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_sp2_gg", "Scatter")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_sp3_gg", "Scatter"))
                                 ),
                                 fluidRow(
                                   column(4, plotlyOutput("ScatterPlot_c1")),
                                   column(4, plotlyOutput("ScatterPlot_c2")),
                                   column(4, plotlyOutput("ScatterPlot_c3"))
                                 )
                        ),
                        tabPanel("Protein Comparison", value = "3_p2",
                                 br(),
                                 fluidRow(
                                   column(8, uiOutput("c_comparison_text")),
                                   column(4, uiOutput("c_pc_plot_button"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_comparison_text_f1")),
                                   column(4, uiOutput("c_comparison_text_f2")),
                                   column(4, uiOutput("c_comparison_text_f3"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_comparison1_FDR_slider")),
                                   column(4, uiOutput("c_comparison2_FDR_slider")),
                                   column(4, uiOutput("c_comparison3_FDR_slider"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_comparison1_pvalue_slider")),
                                   column(4, uiOutput("c_comparison2_pvalue_slider")),
                                   column(4, uiOutput("c_comparison3_pvalue_slider"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_comparison_logFC_slider1")),
                                   column(4, uiOutput("c_comparison_logFC_slider2")),
                                   column(4, uiOutput("c_comparison_logFC_slider3"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, plotlyOutput("comparison1")),
                                   column(4, plotlyOutput("comparison2")),
                                   column(4, plotlyOutput("comparison3")) #, width = "370px", height = "300px")
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, plotlyOutput("comparison1_scatter")),
                                   column(4, plotlyOutput("comparison2_scatter")),
                                   column(4, plotlyOutput("comparison3_scatter")) #, width = "370px", height = "300px")
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_VennDiagram_legend"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, plotOutput("c_VennDiagram", width = "220px", height = "220px")),
                                   column(8, tableOutput("c_unique"))
                                 )
                        ),
                        tabPanel("InWeb", value = "3_p3",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_bait_layer")),
                                   column(2, uiOutput("c_text_inweb")),
                                   column(2, uiOutput("c_button_inweb"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_color_setting_text_inweb"))
                                 ),
                                 fluidRow(
                                   column(2, uiOutput("c_color_theme_inweb_sig")),
                                   column(2, uiOutput("c_color_theme_inweb_insig")),
                                   column(2, uiOutput("c_color_theme_tab3")),
                                   column(2, uiOutput("c_marker_theme_inweb"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, plotOutput("c_inweb_colorbar", height = "100px"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(1, myDownloadButton("download_c_vp1_inweb_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp2_inweb_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp3_inweb_gg", "Volcano"))
                                 ),
                                 fluidRow(
                                   column(4, plotlyOutput("VolcanoPlot_c1_inweb")),
                                   column(4, plotlyOutput("VolcanoPlot_c2_inweb")),
                                   column(4, plotlyOutput("VolcanoPlot_c3_inweb"))
                                 ),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(4, plotOutput("c_VennDiagram_inweb", width = "220px", height = "220px")),
                                   column(8, tableOutput("c_unique_inweb"))
                                 )
                        ),
                        tabPanel("GOI", value = "3_p4",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_genes_file")),
                                   column(2, uiOutput("c_text_goi")),
                                   column(2, uiOutput("c_button_goi"))
                                 ),
                                 
                                 fluidRow(
                                   column(4, uiOutput("c_color_setting_text_goi"))
                                 ),
                                 fluidRow(
                                   column(2, uiOutput("c_color_theme_goi_sig")),
                                   column(2, uiOutput("c_color_theme_goi_insig")),
                                   column(2, uiOutput("c_color_theme_tab4")),
                                   column(2, uiOutput("c_marker_theme_goi"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, plotOutput("c_goi_colorbar", height = "100px"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(1, myDownloadButton("download_c_vp1_goi_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp2_goi_gg", "Volcano")),
                                   column(3),
                                   column(1, myDownloadButton("download_c_vp3_goi_gg", "Volcano"))
                                 ),
                                 fluidRow(
                                   column(4, plotlyOutput("VolcanoPlot_c1_goi")),
                                   column(4, plotlyOutput("VolcanoPlot_c2_goi")),
                                   column(4, plotlyOutput("VolcanoPlot_c3_goi"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_goi_num_inputs"))
                                 ),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(4, plotOutput("c_VennDiagram_goi", width = "220px", height = "220px")),
                                   column(8, tableOutput("c_unique_goi"))
                                 )
                        ),
                        tabPanel("Protein Family", value = "3_p6",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("c_pf_FDR_slider1")),
                                   column(4, uiOutput("c_pf_FDR_slider2")),
                                   column(4, uiOutput("c_pf_FDR_slider3"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_pf_pvalue_slider1")),
                                   column(4, uiOutput("c_pf_pvalue_slider2")),
                                   column(4, uiOutput("c_pf_pvalue_slider3"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("c_pf_logFC_slider1")),
                                   column(4, uiOutput("c_pf_logFC_slider2")),
                                   column(4, uiOutput("c_pf_logFC_slider3"))
                                 ),
                                 fluidRow(
                                   column(3, uiOutput("c_PF_marker_size")), 
                                   column(3, uiOutput("c_PF_sort_col")),
                                   column(3, uiOutput("c_PF_freq")),
                                   column(3, uiOutput("c_PF_button"))
                                 ),
                                 fluidRow(
                                   column(3, uiOutput("c_pf_loc_selection"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(12, plotlyOutput("comparison1_pf"))
                                 ),
                                 fluidRow(
                                   column(12, plotlyOutput("comparison2_pf"))
                                 ),
                                 fluidRow(
                                   column(12, plotlyOutput("comparison3_pf"))
                                 )
                        ),
                        tabPanel("PFE", value = "q1",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("b_PF_FDR_slider")),
                                   column(4, uiOutput("b_PF_logFC_slider")),
                                   column(4, uiOutput("b_PF_pvalue_slider"))
                                 ),
                                 fluidRow(
                                   column(3, uiOutput("b_PF_marker_size")), 
                                   column(3, uiOutput("b_PF_sort_col")),
                                   column(3, uiOutput("b_PF_freq"))
                                 ),
                                 fluidRow(column(3, uiOutput("b_pf_loc_selection")),
                                          column(3, uiOutput("b_PF_button"))
                                 ),
                                 fluidRow(
                                   column(12, plotlyOutput("Protein_Family"), height = "800px")
                                 )
                        ),
                        tabPanel("Download", value = "3_p7",
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c1_download_mapped_uniprot", "Converted identifiers f1")),
                                   column(3, downloadButton("c2_download_mapped_uniprot", "Converted identifiers f2")),
                                   column(3, downloadButton("c3_download_mapped_uniprot", "Converted identifiers f3"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c1_download_replications_calculated", "Calculated mod ttest f1")),
                                   column(3, downloadButton("c2_download_replications_calculated", "Calculated mod ttest f2")),
                                   column(3, downloadButton("c3_download_replications_calculated", "Calculated mod ttest f3"))
                                 ),
                                 # br(),
                                 # fluidRow(
                                 #   column(2, downloadButton("c_download_snp_to_genes", "SNP to gene"))
                                 # ),
                                 hr(),
                                 fluidRow(
                                   column(3, downloadButton("c_download_basic_plots", "Basic Plots"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c_download_protein_comparisons", "Protein comparisons"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c_download_inweb", "InWeb comparisons"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c_download_goi", "GOI comparisons"))
                                 ),
                                 # br(),
                                 # fluidRow(
                                 #   column(3, downloadButton("c_download_snp", "SNP comparisons"))
                                 # ),
                                 br(),
                                 fluidRow(
                                   column(3, downloadButton("c_download_protein_fams", "Protein families"))
                                 )
                        )
            )
    ),
    tabItem(tabName = "guide",
            
            # works in browser format
            tags$iframe(src = "welcome_guide_200415.pdf",
                                           style="width:100%;",  #frameborder="0"
                                           height = "3100px")
    )
  )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              menuItem("Welcome Guide", tabName = "guide", icon = icon("info-circle")),
              menuItem("Single File", tabName = "dashboard", icon = icon("file")),
              menuItem("Multi Files Comparison", icon = icon("copy"), tabName = "widgets"),
              conditionalPanel("input.sidebarmenu === 'dashboard'",
                               uiOutput("a_file"),
                               HTML('<hr style="border-color: #D6DBE0;">'),
                               uiOutput("a_bait_search"),
                               uiOutput("a_GOI_search"),
                               #uiOutput("a_color_scheme"),
                               uiOutput("a_color_style"),
                               uiOutput("a_file_color"),
                               uiOutput("a_significance_type_ui"),
                               uiOutput("FDR_thresh"),
                               uiOutput("PVal_thresh"),
                               uiOutput('a_logfc_direction_ui'),
                               uiOutput("logFC_thresh")
              ),
              conditionalPanel("input.sidebarmenu === 'widgets'",
                               uiOutput("c_file1"),
                               uiOutput("c_file2"),
                               uiOutput("c_file3"),
                               HTML('<hr style="border-color: #D6DBE0;">'),
                               uiOutput("c_GOI_search"),
                               uiOutput("c_color_scheme"),
                               uiOutput("c_color_style"),
                               uiOutput("c_file_color"),
                               uiOutput("c_FDR_thresh"),
                               uiOutput("c_PVal_thresh"),
                               uiOutput("c_logFC_thresh_combined")
              )
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "black",
              dashboardHeader(title = "Genoppi"),
              sidebar,
              body
)

