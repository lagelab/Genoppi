#Created 9/10/16 by April Kim
#User interface file for Shiny app Genoppi
shinyUI(navbarPage("Genoppi",
                   tabPanel("Single File",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           uiOutput("a_file"),
                                           HTML('<hr style="border-color: #D6DBE0;">'),
                                           uiOutput("a_GOI_search"),
                                           uiOutput("a_color_scheme"),
                                           uiOutput("FDR_thresh"),
                                           uiOutput("PVal_thresh"),
                                           uiOutput("logFC_thresh")
                              ),
                              mainPanel(width = 9,
                                shinyjs::useShinyjs(),
                                tabsetPanel(id = "basic",
                                            tabPanel("Quality Control", value = "p1",
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
                                                       column(2, uiOutput("a_text_label")),
                                                       column(2, uiOutput("a_plot_button"))  
                                                     ),
                                                     fluidRow(
                                                       column(1, plotOutput("multi_FDR_colorbar", width = "50px")),
                                                       column(8, plotlyOutput("Multi_VolcanoPlot")),
                                                       column(3, tableOutput("Multi_VP_count"))
                                                     ),
                                                     fluidRow(
                                                       column(9),
                                                       column(3, uiOutput("Multi_VP_count_text"))
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
                                                       column(8, uiOutput("a_VD_sig_text"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("a_FDR_slider")),
                                                       column(4, uiOutput("a_logFC_slider")),
                                                       column(4, uiOutput("a_pvalue_slider"))
                                                     ),
                                                     fluidRow(
                                                       column(4, uiOutput("a_vennd_button1", width = "50px")),
                                                       column(4, uiOutput("a_vennd_button2", width = "50px")),
                                                       column(4, uiOutput("a_vennd_button3", width = "50px"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotOutput("Venn_Diagram_SNP")),
                                                       column(4, plotOutput("Venn_Diagram_GOI")),
                                                       column(4, plotOutput("Venn_Diagram_bait"))
                                                     ),
                                                     fluidRow(
                                                       column(4, uiOutput("a_vd_SNP_text")),
                                                       column(4, uiOutput("a_vd_GOI_text")),
                                                       column(4, uiOutput("a_vd_text"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotOutput("Venn_Diagram_SNP_SGL", width = "220px", height = "220px"))
                                                       #column(4, plotOutput("Venn_Diagram_GOI_1", width = "220px", height = "220px"))
                                                       ),
                                                     fluidRow(
                                                       column(4, uiOutput("a_vd_SNP_SGL_text"))
                                                       #column(4, uiOutput("a_vd_GOI_text_1"))
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
                                                       column(8, uiOutput("a_BPF_text"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("a_BPF_FDR_slider")),
                                                       column(4, uiOutput("a_BPF_logFC_slider")),
                                                       column(4, uiOutput("a_BPF_pvalue_slider"))
                                                     ),
                                                     fluidRow(
                                                       column(4, uiOutput("a_BPF_marker_size")), 
                                                       column(4, uiOutput("a_BPF_freq")),
                                                       column(4, uiOutput("a_BPF_button"))
                                                     ),
                                                     fluidRow(
                                                       column(12, plotlyOutput("Basic_Protein_Family"), height = "800px")
                                                     )
                                            ),
                                            tabPanel("Download", value = "p5",
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
                                                     downloadButton("download_enriched_families_bpf", "Enriched protein families"),
                                                     hr(),
                                                     downloadButton("download_basic_plots", "QC report"),
                                                     br(),
                                                     br(),
                                                     downloadButton("download_layered_plots", "Integrated plots report"),
                                                     br(),
                                                     br(),
                                                     downloadButton("download_venn_diagram_plot", "Venn diagram - InWeb_IM report"),
                                                     br(),
                                                     br(),
                                                     downloadButton("download_venn_diagram_GOI_plot", "Venn diagram - GOI report"),
                                                     br(),
                                                     br(),
                                                     downloadButton("download_venn_diagram_SNP_plot", "Venn diagram - SNP report"),
                                                     br(),
                                                     br(),
                                                     downloadButton("download_bpf_plot", "Protein family report")
                                                     
                                            )
                                )
                              )
                            )
                   ),
                   tabPanel("Multiple Files Comparisons",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           uiOutput("c_file1"),
                                           uiOutput("c_file2"),
                                           uiOutput("c_file3"),
                                           HTML('<hr style="border-color: #D6DBE0;">'),
                                           uiOutput("c_GOI_search"),
                                           uiOutput("c_color_scheme"),
                                           uiOutput("c_FDR_thresh"),
                                           uiOutput("c_PVal_thresh"),
                                           uiOutput("c_logFC_thresh_combined")
                              ),
                              mainPanel(width = 9,
                                shinyjs::useShinyjs(),
                                tabsetPanel(id = "comparison",
                                            tabPanel("Quality Control", value = "3_p1",
                                                     br(),
                                                     fluidRow(
                                                       column(3, plotOutput("c_FDR_colorbar", height = "100px"))
                                                     ),
                                                     fluidRow(
                                                       column(4, plotlyOutput("VolcanoPlot_c1")),
                                                       column(4, plotlyOutput("VolcanoPlot_c2")),
                                                       column(4, plotlyOutput("VolcanoPlot_c3"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotlyOutput("ScatterPlot_c1")),
                                                       column(4, plotlyOutput("ScatterPlot_c2")),
                                                       column(4, plotlyOutput("ScatterPlot_c3"))
                                                     )
                                            ),
                                            tabPanel("Protein Comparison", value = "3_p2",
                                                     br(),
                                                     fluidRow(
                                                       column(8, uiOutput("c_comparison_text"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(3, uiOutput("c_comparison_FDR_slider")),
                                                       column(4, uiOutput("c_comparison_pvalue_slider"))
                                                     ),
                                                     fluidRow(
                                                       column(3, uiOutput("c_comparison_logFC_slider1")),
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
                                                     br(),
                                                     fluidRow(
                                                       column(3, plotOutput("c_inweb_colorbar", height = "100px"))
                                                     ),
                                                     br(),
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
                                                     br(),
                                                     fluidRow(
                                                       column(3, plotOutput("c_goi_colorbar", height = "100px"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotlyOutput("VolcanoPlot_c1_goi")),
                                                       column(4, plotlyOutput("VolcanoPlot_c2_goi")),
                                                       column(4, plotlyOutput("VolcanoPlot_c3_goi"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("goi_num_inputs"))
                                                     ),
                                                     br(),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotOutput("c_VennDiagram_goi", width = "220px", height = "220px")),
                                                       column(8, tableOutput("c_unique_goi"))
                                                     )
                                                     ),
                                            tabPanel("SNP", value = "3_p5",
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("c_SNP_file")),
                                                       column(2, uiOutput("c_text_snp")),
                                                       column(2, uiOutput("c_button_snp"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(3, plotOutput("c_snp_colorbar", height = "100px"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotlyOutput("VolcanoPlot_c1_snp")),
                                                       column(4, plotlyOutput("VolcanoPlot_c2_snp")),
                                                       column(4, plotlyOutput("VolcanoPlot_c3_snp"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("snp_num_inputs"))
                                                     ),
                                                     br(),
                                                     br(),
                                                     fluidRow(
                                                       column(4, plotOutput("c_VennDiagram_snp", width = "220px", height = "220px")),
                                                       column(8, tableOutput("c_unique_snp"))
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
                                                     br(),
                                                     fluidRow(
                                                       column(9, plotlyOutput("comparison1_pf"))
                                                     ),
                                                     fluidRow(
                                                       column(9, plotlyOutput("comparison2_pf"))
                                                     ),
                                                     fluidRow(
                                                       column(9, plotlyOutput("comparison3_pf"))
                                                     )
                                                     ),
                                            # tabPanel("PF Search", value = "3_p7",
                                            #          br(),
                                            #          fluidRow(
                                            #            column(4, uiOutput("c_prot_fam_db")),
                                            #            column(4, uiOutput("c_text_prot_fam_db")),
                                            #            column(4, uiOutput("c_prot_fam_db_button"))
                                            #          ),
                                            #          br(),
                                            #          fluidRow(
                                            #            column(4, plotlyOutput("VolcanoPlot_c1_pf_db")),
                                            #            column(4, plotlyOutput("VolcanoPlot_c2_pf_db")),
                                            #            column(4, plotlyOutput("VolcanoPlot_c3_pf_db"))
                                            #          )
                                            #          ),
                                            tabPanel("Download", value = "3_p8",
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
                                                     br(),
                                                     fluidRow(
                                                       column(2, downloadButton("c_download_snp_to_genes", "SNP to gene"))
                                                     ),
                                                     hr(),
                                                     fluidRow(
                                                       column(3, downloadButton("c_download_basic_plots", "Quality control"))
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
                                                     br(),
                                                     fluidRow(
                                                       column(3, downloadButton("c_download_snp", "SNP comparisons"))
                                                     ),
                                                     br(),
                                                     fluidRow(
                                                       column(3, downloadButton("c_download_protein_fams", "Protein families"))
                                                     )
                                                     )
                                            )
                              )
                            )),
                   tabPanel("Advanced PFE",
                            sidebarLayout(
                              sidebarPanel(width = 3,
                                           fileInput('file_pfam1', 'Upload case 1 vs. control 1 file',
                                                     accept = c(
                                                       'text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv')
                                           ),
                                           fileInput('file_pfam2', 'Upload case 2 vs. control 2 file',
                                                     accept = c(
                                                       'text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv')
                                           ),
                                           fileInput('file_pfam3', 'Upload case 1 vs. case 2 file',
                                                     accept = c(
                                                       'text/csv',
                                                       'text/comma-separated-values',
                                                       'text/tab-separated-values',
                                                       'text/plain',
                                                       '.csv',
                                                       '.tsv')
                                           ),
                                           uiOutput("b_GOI_search")
                              ),
                              mainPanel(
                                tabsetPanel(id = "advance",
                                            tabPanel("Protein Family", value = "q1",
                                                     br(),
                                                     fluidRow(
                                                       column(4, uiOutput("b_PF_FDR_slider")),
                                                       column(4, uiOutput("b_PF_logFC_slider")),
                                                       column(4, uiOutput("b_PF_pvalue_slider"))
                                                     ),
                                                     fluidRow(
                                                       column(4, uiOutput("b_PF_marker_size")), 
                                                       column(4, uiOutput("b_PF_freq")),
                                                       column(4, uiOutput("b_PF_button"))
                                                     ),
                                                     fluidRow(
                                                       column(12, plotlyOutput("Protein_Family"), height = "800px")
                                                     )
                                            ),
                                            tabPanel("Download", value = "q2",
                                                     br(),
                                                     downloadButton("download_enriched_families_pf", "Advanced enriched fam"),
                                                     hr(),
                                                     br(),
                                                     downloadButton("download_pf_plot", "Advanced protein fam report"))
                                )
                              )
                            )
                   ),
                   tabPanel("Documentation",
                            imageOutput("documentation")
                   )
)
)