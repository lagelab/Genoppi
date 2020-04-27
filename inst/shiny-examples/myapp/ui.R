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
                        tabPanel("Protein Comparison", 
                                 br(),
                                 column(4,
                                        box(
                                          title = 'Protein Comparison I', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   uiOutput("b_file_1_significance_type_ui"),
                                                   uiOutput("b_file_1_FDR_thresh"),
                                                   uiOutput("b_file_1_PVal_thresh"),
                                                   uiOutput('b_file_1_logfc_direction_ui'),
                                                   uiOutput("b_file_1_logFC_thresh")
                                                 )
                                                )
                                        ),
                                        box(
                                          title = 'Volcano', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_1_volcano'),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'scatter plot', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_1_scatter'),
                                                 )
                                          )
                                        )
                                 ),
                                 column(4,
                                        box(
                                          title = 'Protein Comparison II', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   uiOutput("b_file_2_significance_type_ui"),
                                                   uiOutput("b_file_2_FDR_thresh"),
                                                   uiOutput("b_file_2_PVal_thresh"),
                                                   uiOutput('b_file_2_logfc_direction_ui'),
                                                   uiOutput("b_file_2_logFC_thresh")
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'Volcano', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_2_volcano'),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'Scatter plot', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_2_scatter'),
                                                 )
                                          )
                                        )
                                 ),
                                 column(4,
                                        box(
                                          title = 'Protein Comparison III', width = 12, solidHeader = TRUE, status = 'primary', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   uiOutput("b_file_3_significance_type_ui"),
                                                   uiOutput("b_file_3_FDR_thresh"),
                                                   uiOutput("b_file_3_PVal_thresh"),
                                                   uiOutput('b_file_3_logfc_direction_ui'),
                                                   uiOutput("b_file_3_logFC_thresh")
                                                 )
                                                )
                                        ),
                                        box(
                                          title = 'Volcano', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_3_volcano'),
                                                 )
                                          )
                                        ),
                                        box(
                                          title = 'Scatter plot', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 fluidRow(
                                                   plotlyOutput('b_file_3_scatter'),
                                                 )
                                          )
                                        )
                                )
                         
                        ),
                        tabPanel("Venn diagrams", 
                                 column(6,
                                   box(
                                     title = 'Venn diagram', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                     column(2, ''),
                                     column(8,
                                            fluidRow(
                                              plotOutput('b_file_comparison_venn_ui', width = "280px", height = "280px")
                                            ),
                                            fluidRow(
                                              uiOutput('b_file_comparison_venn_verbatim_ui')
                                            )
                                     ),
                                     column(2, '')
                                   )
                                 ),
                                 column(6,
                                        box(
                                          title = 'Explore overlap', width = 12, solidHeader = TRUE, status = 'success', collapsible = TRUE, collapsed = TRUE,
                                          column(12,
                                                 DT::dataTableOutput('b_file_comparison_data_table_ui')
                                          )
                                        )
                                        
                                 )
                                 
                                 
                                 #column(4, ),
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
              conditionalPanel("input.sidebarmenu == 'widgets'",
                               uiOutput("b_GOI_search"),
                               uiOutput("b_file_1_ui"),
                               uiOutput("b_file_2_ui"),
                               uiOutput("b_file_3_ui")
              )
                
              
  )
)

# Put them together into a dashboardPage
dashboardPage(skin = "black",
              dashboardHeader(title = "Genoppi"),
              sidebar,
              body
)

