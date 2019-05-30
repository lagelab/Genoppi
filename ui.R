body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "dashboard",
            shinyjs::useShinyjs(),
            tabsetPanel(id = "basic", #width = 12,
                        tabPanel("Basic Plots", value = "p1",
                                 br(),
                                 fluidRow(
                                   column(3, uiOutput("a_prot_fam_db")),
                                   column(2, uiOutput("a_text_prot_fam_db")),
                                   column(3, uiOutput("a_color_theme_pf")),
                                   column(3, downloadButton("download_pf_cleaned_input", "Remove selected PF from input"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_color_setting_indv_text"))
                                 ),
                                 fluidRow(
                                   column(2, uiOutput("a_color_theme_indv_sig")),
                                   column(2, uiOutput("a_color_theme_indv_insig"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_color_theme"))
                                 ),
                                 fluidRow(
                                   column(1),
                                   column(1, myDownloadButton("download_vp_gg", "Volcano")),
                                   column(5),
                                   column(3, uiOutput("VP_count_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(1, plotOutput("FDR_colorbar", width = "50px")),
                                   column(6, plotlyOutput("VolcanoPlot")), #, width = "550px", height = "550px"
                                   column(3, tableOutput("VP_count"))
                                 ),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 fluidRow(
                                   column(1),
                                   column(1, myDownloadButton("download_sp_gg", "Scatter"))
                                 ),
                                 fluidRow(
                                   column(1),
                                   column(8, plotlyOutput("ScatterPlot")) #, width = "550px", height = "550px"
                                 )
                        ),
                        tabPanel("Integrated Plots", value = "p2", 
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("a_SNP_file")),
                                   column(4, uiOutput("a_genes_file")),
                                   column(4, uiOutput("a_bait_layer"))
                                 ),
                                 fluidRow(
                                   column(4),
                                   column(2, uiOutput("a_text_label")),
                                   column(2, uiOutput("a_plot_button"))  
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_color_setting_text"))
                                 ),
                                 fluidRow(
                                   column(2, uiOutput("a_color_theme_multi_sig")), 
                                   column(2, uiOutput("a_color_theme_multi_insig")),
                                   column(2, uiOutput("a_color_theme_integrated")),
                                   column(2, uiOutput("a_color_theme_snp")),
                                   column(2, uiOutput("a_color_theme_goi")),
                                   column(2, uiOutput("a_color_theme_inweb"))
                                 ),
                                 fluidRow(
                                   column(1, myDownloadButton("download_multi_vp_gg", "Volcano")), 
                                   column(8),
                                   column(3, uiOutput("Multi_VP_count_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(1, plotOutput("multi_FDR_colorbar", width = "50px")),
                                   column(8, plotlyOutput("Multi_VolcanoPlot")),
                                   column(3, tableOutput("Multi_VP_count"))
                                 )
                        ),
                        tabPanel("Venn Diagrams", value = "p3",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("a_SNP_file_vennd")),
                                   column(4, uiOutput("a_genes_file_vennd")),
                                   column(4, uiOutput("a_bait_venndiagram"))
                                 ),
                                 fluidRow(
                                   column(4)
                                 ),
                                 fluidRow(
                                   column(8, uiOutput("a_VD_sig_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("a_FDR_slider")),
                                   column(4, uiOutput("a_pvalue_slider")),
                                   column(4, uiOutput("a_logFC_slider"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_vennd_button1", width = "50px")),
                                   column(4, uiOutput("a_vennd_button2", width = "50px")),
                                   column(4, uiOutput("a_vennd_button3", width = "50px"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, downloadButton("download_venn_diagram_SNP_genes", "Genes in venn diagram regions")),
                                   column(4, downloadButton("download_venn_diagram_GOI_genes", "Genes in venn diagram regions")),
                                   column(4, downloadButton("download_venn_diagram_inweb_genes", "Genes in venn diagram regions"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4),
                                   column(4, uiOutput("a_goi_num_inputs"))
                                 ),
                                 fluidRow(
                                   column(4, plotOutput("Venn_Diagram_SNP", width = "220px", height = "220px")),
                                   column(4, plotOutput("Venn_Diagram_GOI", width = "220px", height = "220px")),
                                   column(4, plotOutput("Venn_Diagram_bait", width = "220px", height = "220px"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_vd_SNP_text")),
                                   column(4, uiOutput("a_vd_GOI_text")),
                                   column(4, uiOutput("a_vd_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, plotOutput("Venn_Diagram_SNP_SGL", width = "220px", height = "220px"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_vd_SNP_SGL_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, plotOutput("Venn_Diagram_SNP_MGL", width = "220px", height = "220px"))
                                 ),
                                 fluidRow(
                                   column(4, uiOutput("a_vd_SNP_MGL_text"))
                                 )
                        ),
                        tabPanel("Protein Family", value = "p4",
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("a_pf_loc_selection")),
                                   column(4, uiOutput("a_pf_plot_selection")),
                                   column(4, plotlyOutput("Basic_Protein_Family_sp_prev", width = "200px", height = "200px"))
                                 ),
                                 fluidRow(
                                   column(8, uiOutput("a_BPF_text"))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("a_BPF_FDR_slider")),
                                   column(4, uiOutput("a_BPF_pvalue_slider")),
                                   column(4, uiOutput("a_BPF_logFC_slider"))
                                 ),
                                 fluidRow(
                                   column(3, uiOutput("a_BPF_marker_size")), 
                                   column(3, uiOutput("a_PF_sort_col")),
                                   column(3, uiOutput("a_BPF_freq")),
                                   column(3, uiOutput("a_BPF_button"))
                                 ),
                                 fluidRow(
                                   column(12, plotlyOutput("Basic_Protein_Family"), height = "800px")
                                 )
                        ),
                        tabPanel("Download", value = "p6",
                                 br(),
                                 downloadButton("download_mapped_uniprot", "Converted identifiers"),
                                 br(),
                                 br(),
                                 downloadButton("download_replications_calculated", "Calculated moderated t-statistic"),
                                 br(),
                                 br(),
                                 downloadButton("download_snp_to_genes", "SNP to gene"),
                                 br(),
                                 br(),
                                 downloadButton("download_goi_overlap", "GOI"),
                                 br(),
                                 br(),
                                 downloadButton("download_enriched_families_bpf", "Enriched protein families"),
                                 hr(),
                                 downloadButton("download_basic_plots", "QC report"),
                                 br(),
                                 br(),
                                 downloadButton("download_layered_plots", "Integrated plots report"),
                                 br(),
                                 br(),
                                 downloadButton("download_venn_diagram_plot", "Venn diagram - InWeb report"),
                                 br(),
                                 br(),
                                 downloadButton("download_venn_diagram_GOI_plot", "Venn diagram - GOI report"),
                                 br(),
                                 br(),
                                 downloadButton("download_venn_diagram_SNP_plot", "Venn diagram - SNP report"),
                                 br(),
                                 br(),
                                 downloadButton("download_bpf_vp_plot", "Protein family report")
                                 
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
                        # tabPanel("SNP", value = "3_p5",
                        #          br(),
                        #          fluidRow(
                        #            column(4, uiOutput("c_SNP_file")),
                        #            column(2, uiOutput("c_text_snp")),
                        #            column(2, uiOutput("c_button_snp"))
                        #          ),
                        #          fluidRow(
                        #            column(4)
                        #          ),
                        #          fluidRow(
                        #            column(4, uiOutput("c_color_setting_text_snp"))
                        #          ),
                        #          fluidRow(
                        #            column(2, uiOutput("c_color_theme_snp_sig")),
                        #            column(2, uiOutput("c_color_theme_snp_insig")),
                        #            column(2, uiOutput("c_color_theme_tab5")),
                        #            column(2, uiOutput("c_marker_theme_snp"))
                        #          ),
                        #          br(),
                        #          fluidRow(
                        #            column(3, plotOutput("c_snp_colorbar", height = "100px"))
                        #          ),
                        #          br(),
                        #          fluidRow(
                        #            column(1, myDownloadButton("download_c_vp1_snp_gg", "Volcano")),
                        #            column(3),
                        #            column(1, myDownloadButton("download_c_vp2_snp_gg", "Volcano")),
                        #            column(3),
                        #            column(1, myDownloadButton("download_c_vp3_snp_gg", "Volcano"))
                        #          ),
                        #          fluidRow(
                        #            column(4, plotlyOutput("VolcanoPlot_c1_snp")),
                        #            column(4, plotlyOutput("VolcanoPlot_c2_snp")),
                        #            column(4, plotlyOutput("VolcanoPlot_c3_snp"))
                        #          ),
                        #          br(),
                        #          fluidRow(
                        #            column(4, uiOutput("snp_num_inputs"))
                        #          ),
                        #          br(),
                        #          br(),
                        #          fluidRow(
                        #            column(4, plotOutput("c_VennDiagram_snp", width = "220px", height = "220px")),
                        #            column(8, tableOutput("c_unique_snp"))
                        #          )
                        # ),
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
            uiOutput("how_to_guide")
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
                               uiOutput("a_GOI_search"),
                               uiOutput("a_color_scheme"),
                               uiOutput("a_color_style"),
                               uiOutput("a_file_color"),
                               uiOutput("FDR_thresh"),
                               uiOutput("PVal_thresh"),
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

