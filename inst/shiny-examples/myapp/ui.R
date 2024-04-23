# GENOPPI SHINY APP UI

# ----------------------------------------------------------------------------------------
# set up main body UI
body <- dashboardBody(

	tags$head(tags$link(rel='stylesheet', type='text/css', href='custom.css')),

	fluidRow(
	tabItems(

	# ------------------------------------------------------------------------------------
	# Single File tabs
	tabItem(tabName='dashboard', shinyjs::useShinyjs(),	
		tabsetPanel(id='basic',
			header=column(width=12,
			br(),
			
			# Statistics parameters box, present at top of all tabs
			statsParamsOptions('stats_input', collapsed=FALSE)),
			
			# ----------------------------------------------------------------------------
			# Single File: Basic plotting 
			tabPanel('Basic plotting', id='basicplot', value='p1',
				br(),
				
				# left column: basic plot options and data summary boxes
				column(width=4,
					basicPlotInputBox('basic_plot_inputs'),
					summaryBox('summary'), 
				),
				
				# right column: volcano plot and pairwise scatter plot
				column(width=8,
					drawVolcanoPlot('volcano_plot'),
					plotGGpairFrame('plot_ggpair'),
				),
			),

			# ----------------------------------------------------------------------------
			# Single File: Integrated plotting
			tabPanel('Integrated plotting', id='integratedplot', value='p2', 
				br(),
				
				# ------------------------------------------------------------------------
				# left column: external datasets to integrate
				column(width=4, 
				
					# --------------------------------------------------------------------
					# PPI databases
					# TODO: add STIRNG
					externalPPIBox('ppi_overlay'),
					
					# --------------------------------------------------------------------
					# GWAS catalog
					# TODO: check subsetting toggle is disabled correctly
					box(title='GWAS catalog',
						width=NULL, solidHeader=TRUE, status='primary',
						collapsible=TRUE, collapsed=TRUE,
						
						# search box for GWAS traits
						fluidRow(column(12, uiOutput('a_gwas_catalogue_ui'))),
						
						# toggles and mapping download button
						fluidRow(
							column(4, uiOutput('a_overlay_gwas_cat_ui')),
							column(4, uiOutput('a_label_gwas_cat_ui')),
							column(4,
								shinyjs::hidden(
									myDownloadButton('a_gwas_catalogue_mapping_download',
									'Mapping', img=icon('file-alt', lib= 'font-awesome'))
								)
							)
						),
						
						# DISABLE FOR NOW: subset entries by traits
						#fluidRow(column(12, uiOutput('a_gwas_subset_traits_by_data'))),

						# info button
						fluidRow(column(12, uiOutput('info_gwas_ui')))
					),

					# --------------------------------------------------------------------
					# gnomAD gene constraints
					# TODO: add LOEUF and MisZ options?           
					box(title='gnomAD gene constriants',
						width=NULL, solidHeader=TRUE, status='primary',
						collapsible=TRUE, collapsed=TRUE,
                        
                        # score threshold slider
                        fluidRow(
                        	column(12, uiOutput('a_slide_gnomad_pli_threshold_ui'),
								uiOutput('a_gnomad_colorscale_text_ui')
							)
						),
						
						# toggles and mapping download button
						fluidRow(
							column(4, uiOutput('a_overlay_gnomad_ui')),
							column(4, uiOutput('a_label_gnomad_ui')),
							column(4,
								shinyjs::hidden(
									myDownloadButton('a_gnomad_mapping_download',
									'Mapping', img=icon('file-alt', lib='font-awesome'))
								)
							)
						),
						
						# info button
						fluidRow(column(12, uiOutput('info_gnomad_ui')))
					),
					
					# --------------------------------------------------------------------
					# GTEx / HPA tissue expression
					box(title='GTEx / HPA tissue expression',
						width=NULL, solidHeader=TRUE, status='primary',
						collapsible=TRUE, collapsed=TRUE,
						
						# dropdown menu to select dataset + search box for tissues
						fluidRow(
							column(12, uiOutput('a_tissue_select_ui'),
								uiOutput('a_gtex_rna_tissue_ui'),
								uiOutput('a_gtex_protein_tissue_ui'),
								uiOutput('a_hpa_rna_tissue_ui')
							)
						),
						
						# toggles and mapping download button
						fluidRow(
							column(4, uiOutput('a_overlay_tissue_ui')),
							column(4, uiOutput('a_label_tissue_ui')),
							column(4,
								shinyjs::hidden(
									myDownloadButton('a_tissue_mapping_download',
									'Mapping', img=icon('file-alt', lib='font-awesome'))
								)
							)
						),
						
						# info button
						fluidRow(column(12, uiOutput('info_tissue_ui')))
                    ),
                    
                    # --------------------------------------------------------------------
                    # Custom SNP list upload
					box(title='Upload SNPs',
						width=NULL, solidHeader=TRUE, status='primary',
						collapsible=TRUE, collapsed=TRUE,
						
						# upload box
						fluidRow(column(12, uiOutput('a_SNP_file'))),
						
						# toggles and mapping download button
						fluidRow(
							column(4, uiOutput('a_overlay_snp_ui')),
							column(4, uiOutput('a_label_snp_ui')),
							column(4,
								shinyjs::hidden(
									myDownloadButton('a_snp_mapping_download',
									'Mapping', img=icon('file-alt', lib='font-awesome'))
								)
							)
						)
					),
					
					# --------------------------------------------------------------------
					# Custom gene list upload
					box(title='Upload genes',
						width=NULL, solidHeader=TRUE, status='primary',
						collapsible=TRUE, collapsed=TRUE,
						
						# upload box
						fluidRow(column(12, uiOutput('a_genes_file'))),

						# toggles and mapping download button
						fluidRow(
							column(4, uiOutput('a_overlay_genes_upload_ui')),
							column(4, uiOutput('a_label_genes_upload_ui')),
							column(4,
								shinyjs::hidden(
									myDownloadButton('a_gene_upload_mapping_download',
									'Mapping', img=icon('file-alt', lib='font-awesome'))
								)
							)
						)
					),
					
					# --------------------------------------------------------------------
					# Settings (colors and shapes for each dataset in volcano plot)
					box(title='Settings',
						width=NULL, solidHeader=TRUE, collapsible=TRUE, collapsed=FALSE,

						fluidRow(
							column(3, align='left', h5(strong('Overlay'))),
							column(3, align='center',  
								h5(strong(uiOutput('a_sig_text_ui')))),
							column(3, align='center',  
								h5(strong(uiOutput('a_insig_text_ui')))),
							column(3, align='center', h5(strong('Symbol'))),
						),
                        
						fluidRow(
							column(3, h5('PPI')),
							column(3, uiOutput('a_color_inweb_sig_ui')),
							column(3, uiOutput('a_color_inweb_insig_ui')),
							column(3, uiOutput('a_symbol_inweb_ui'))
						),
						
						fluidRow(
							column(3, h5('GWAS catalog')),
							column(3, uiOutput('a_color_gwas_cat_sig_ui')),
							column(3, uiOutput('a_color_gwas_cat_insig_ui')),
							column(3, uiOutput('a_symbol_gwas_cat_ui'))
						),
						
						fluidRow(
							column(3, h5('gnomAD')),
							column(3, uiOutput('a_color_gnomad_sig_ui')),
							column(3, uiOutput('a_color_gnomad_insig_ui')),
							column(3, uiOutput('a_symbol_gnomad_ui'))
						),
						
						fluidRow(
							column(3, h5('GTEx / HPA')),
							column(3, uiOutput('a_color_tissue_sig_ui')),
							column(3, uiOutput('a_color_tissue_insig_ui')),
							column(3, uiOutput('a_symbol_tissue_ui'))
						),
						
						fluidRow(
							column(3, h5('SNPs upload')),
							column(3, uiOutput('a_color_snp_sig_ui')),
							column(3, uiOutput('a_color_snp_insig_ui')),
							column(3, uiOutput('a_symbol_snp_ui'))
						),
						
						fluidRow(
							column(3, h5('Genes upload')),
							column(3, uiOutput('a_color_genes_upload_sig_ui')),
							column(3, uiOutput('a_color_genes_upload_insig_ui')),
							column(3, uiOutput('a_symbol_genes_upload_ui'))
						)
					) # close Settings box
				), # close left column
				
				# ------------------------------------------------------------------------
				# right column: volcano plot and venn diagrams
				column(width=8,
                
                	# --------------------------------------------------------------------
					# volcano plot
                	box(title=tagList(shiny::icon('chart-area'), 'Volcano plot'),
                		width=NULL, solidHeader=TRUE, status='success', collapsible=TRUE, 
						
						# download button
						fluidRow(
							column(12,
								shinyjs::hidden(myDownloadButton(
									'a_integrated_plot_download','Volcano plot'))
							)
						),
						
						# plot
						fluidRow(style='padding-bottom:150px',
							column(12, shinycssloaders::withSpinner(
								plotly::plotlyOutput('Multi_VolcanoPlot'), spinner_type)
							)
						)
						
						
					),
					
					# --------------------------------------------------------------------
					# venn diagram tabs
					tabBox(title=tagList(img(src='icon_venn_a.png', width='22px'),
						'Venn diagrams'), width=NULL,
						
						tabPanel('PPI',
							fluidRow(
								column(4, plotOutput('a_inweb_venn_ui',
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(),
									uiOutput('a_ppi_venn_verbatim_ui')),
								column(3, shinyjs::hidden(
									myDownloadButton('a_inweb_venn_mapping_download',
										'Genes', icon('download'))))
							)
						),
						
						tabPanel('GWAS catalog',
							fluidRow(
								column(4, plotOutput('a_gwas_catalogue_venn_all_ui',
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(), 
									uiOutput('a_gwas_catalogue_venn_verbatim_ui')),
								column(3, shinyjs::hidden(
									myDownloadButton(
										'a_gwas_catalogue_venn_mapping_download',
										'Genes', icon('download'))))
							)
						),

						tabPanel('gnomAD',
							fluidRow(
								column(4, plotOutput('a_gnomad_venn_ui', 
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(),
									uiOutput('a_gnomad_venn_verbatim_ui')),
								column(3, shinyjs::hidden(
									myDownloadButton('a_gnomad_venn_mapping_download', 
										'Genes', icon('download'))))
							)
						),

						tabPanel('GTEx / HPA',
							fluidRow(
								column(4, plotOutput('a_tissue_venn_ui',
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(), 
									uiOutput('a_tissue_venn_verbatim_ui')),
								column(3, shinyjs::hidden(
									myDownloadButton('a_tissue_venn_mapping_download',
										'Genes', icon('download'))))
							)
						),
						
						tabPanel('SNPs upload',
							fluidRow(
								column(4, plotOutput('a_snp_venn_ui',
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(),
										uiOutput('a_snp_venn_verbatim_ui')),
								column(3, uiOutput('a_select_venn_snp_ui'),
										uiOutput('a_select_venn_snp_loci_ui'),
										shinyjs::hidden(myDownloadButton(
											'a_snp_venn_mapping_download',
											'Genes', icon('download'))))
							)
						),
						
						tabPanel('Genes upload',
							fluidRow(
								column(4, plotOutput('a_genes_upload_venn_ui',
									width='220px', height='220px')),
								column(5, br(), br(), br(), br(),
									uiOutput('a_genes_upload_venn_verbatim_ui')),
								column(3, uiOutput('a_select_venn_genes_upload_ui'),
									shinyjs::hidden(myDownloadButton(
										'a_genes_upload_venn_mapping_download',
										'Genes', icon('download')))),
                            )
                        )
					) # close venn diagram tabBox
				) # close right column
			), # close Integrated plotting tabPanel

			# ----------------------------------------------------------------------------
			# Single File: Gene set annotations
			tabPanel('Gene set annotations', value='p3',
				br(),

				# ------------------------------------------------------------------------
				# left column: Settings
				column(width=4,
					box(title='Settings', width=NULL, solidHeader=TRUE,
						status='primary', collapsible = TRUE,
						
						# dropdown menu to select dataset
						fluidRow(column(12, uiOutput('a_pf_loc_selection'))),
						
						# slider to subset by frequency
						fluidRow(column(12,
							uiOutput('a_pathway_mapping_freq_slider_ui'))),
						
						# search box for gene sets
						fluidRow(column(12, uiOutput('a_pathway_mapping_search_ui'))),
						
						# option for sorting order
						fluidRow(column(12, uiOutput('a_pathway_mapping_type_sort_ui'))),
						
						# mapping download button
						fluidRow(
							column(8,''),
							column(4, shinyjs::hidden(myDownloadButton(
								'a_pathway_mapping_download', 'Mapping',
								img=icon('file-alt', lib='font-awesome'))))
						),
						
						# info button
						fluidRow(column(12, uiOutput('info_geneset_ui')))
					)
				),
				
				# ------------------------------------------------------------------------
				# right column: volcano plot and annotation table
				column(width=8,
				
					# volcano plot
					box(title=tagList(shiny::icon('chart-area'), 'Volcano plot'), 
						width=NULL, solidHeader=TRUE, status='success', collapsible=TRUE,
						
						# download buttons				
						fluidRow(
							column(3, shinyjs::hidden(myDownloadButton(
								'a_pathway_plot_download', 'Volcano plot'))),
							column(3, shinyjs::hidden(myDownloadButton(
								'a_pathway_plot_legend_download',
								'Volcano plot (legend)'))),
							column(6, '')
						),
						
						# plot
						fluidRow(style='padding-bottom:125px',
							column(12, shinycssloaders::withSpinner(
								plotly::plotlyOutput('VolcanoPlotPathway'), spinner_type))
						)
					),
					
					# annotation table
					box(title='Annotation table', width=NULL, solidHeader=TRUE,
						status='success', collapsible=TRUE,
						
						fluidRow(
							column(12, DT::dataTableOutput('a_pathway_data_table_ui'))
						)
                   	)
				)
			), # close Gene set annotations tabPanel

			# ----------------------------------------------------------------------------
			# Single File: Tissue enrichment
			tabPanel('Tissue enrichment', value='p4',
				br(),
				
				# ------------------------------------------------------------------------
				# left column: Settings
				column(width=4,
					box(title='Settings', width=NULL, solidHeader=TRUE,
						status='primary', collapsible = TRUE,
						
						# dropdown menus to select or upload dataset +
						# significance threshold slider +
						# x-axis and scientific notation formatting
						fluidRow(column(12, 
							uiOutput('a_tissue_select_source_ui'),
							uiOutput('a_tissue_enrichment_upload_ui'),
							uiOutput('a_tissue_enrichment_type_select_ui'),
							uiOutput('a_tissue_enrichment_slider_ui'),  
							uiOutput('a_tissue_enrichment_xaxis_ui'),
							uiOutput('a_tissue_enrichment_scientific_notation_ui'))
						),
						
						# render plot and download buttons
						fluidRow(
							column(6, uiOutput('a_button_plot_tissue_enrichment_ui')),
							column(6, shinyjs::hidden(myDownloadButton(
								'a_tissue_enrichment_download', 'Tissue enrichment',
								img=icon('file-alt', lib='font-awesome'))))
						),
						
						# info button
						fluidRow(column(12, uiOutput('info_tissue_enrichment_ui')))
                    )
				),
				
				# ------------------------------------------------------------------------
				# right column: enrichment bar plot
				column(width=8,
					box(title='Enrichment bar plot', width=NULL, solidHeader=TRUE,
						status='success', collapsible=TRUE, height=1000,
						
						fluidRow(column(12, shinycssloaders::withSpinner(
							plotly::plotlyOutput('a_tissue_enrichment_ui'), spinner_type))
						)
					)
				)
			) # close Tissue enrichment tabPanel
		) # close basic tabsetPanel
    ), # close Single Files tabItem

	# ------------------------------------------------------------------------------------
	# Multi Files Comparison tabs
    tabItem(tabName='widgets', shinyjs::useShinyjs(),
		tabsetPanel(id='comparison',

			# ----------------------------------------------------------------------------
			# Multi Files Comparison: Basic plotting 
			# up to 3 columsn of: Statistics parameters, Summary, Volcano plot, 
			# Basic plot options, Pairwise scatterplot
			tabPanel('Basic plotting', br(),
				shinyjs::hidden(uiOutput('multifile_column_1')),
				shinyjs::hidden(uiOutput('multifile_column_2')),
				shinyjs::hidden(uiOutput('multifile_column_3')),
			),
			
			# ----------------------------------------------------------------------------
			# Multi Files Comparison: Comparison
			# TODO: DEBUG!
			tabPanel('Comparison', br(),
				column(4, ComparisonPlotInputBox('logfc_comparison')),
				column(8, logFCCorrelation('logfc_comparison')),
				column(12, plotVennDiagram('multifile_venn'))
			),
        )
    ),

	# ------------------------------------------------------------------------------------
	# User guide
	# TODO: update guide to include updates after v1.0
    tabItem(tabName='guide',
		# works in browser format
		tags$iframe(src='welcome_guide_v1.0_210514.pdf',
			style='width:100%;', height='3100px')
    ),
    
    # ------------------------------------------------------------------------------------
	# Quick start
	tabItem(tabName='start',
		fluidRow(
			column(3, ''),
			column(6,
				fluidRow(style='padding-top:200px',
					column(12, align = 'center',
						h1(strong('Genoppi'), style='font-size:100px;'),
						br(),
						
						# box for input file upload
						inputFileWelcome('single_file'),
						
						# example files
						HTML(paste('Try a single',
							getExampleButton('single_file', 'example file'),
							'or',
							getExampleButton('multi_file', 'multiple example files'))),
						br(),
					)
				),
				
				fluidRow(
					br(),
					h5(HTML(paste('<b>Genoppi</b> is an open-source software for 
					performing quality control and analyzing quantitative proteomic data. 
					Genoppi streamlines the integration of proteomic data with external 
					datasets such as known protein-protein interactions in published 
					literature, data from genetic studies, gene set annotations, or other 
					user-defined inputs. See the',
					actionLink('tab_welcome','user guide'), 
					'for more details.')))
				),
			),
			column(3, '')
		)
	)#,
	
	# ------------------------------------------------------------------------------------
	# Data documentation
	#tabItem(tabName='documentation',
	#	fluidRow(
	#		column(1, ''),
	#		column(10,
	#			fluidRow(style='padding-top:100px',
	#				column(12, align='left',
	#					h1(strong('Data documentation'), style='font-size:30px;'),
	#					br(),
	#					uiOutput('documentation_ui')
	#				)
	#			)
	#		),
	#		column(1, '')
	#	)
	#)
    
	) # close tabItems
	) # close fluidRow
) # close dashboardBody

# ----------------------------------------------------------------------------------------
# set up sidebar UI
sidebar <- dashboardSidebar(
	sidebarMenu(id='sidebarmenu',
		menuItem('Quick Start', tabName='start', icon=icon('stream')),
		
		menuItem('User Guide', tabName='guide', icon=icon('info-circle')),
		
		#menuItem('Data Documentation', tabName='documentation',
		#	icon=icon('question-circle')),
			
		menuItem('Single File', tabName='dashboard', icon=icon('file')),
		
		menuItem('Multi Files Comparison', tabName='widgets', icon = icon('copy')),
		
		# Single File: upload box
		inputFileSideBar('single_file', 'dashboard'),
		
		# Multi Files Comparison: upload boxes
		multiFileName('multi_file1', 'widgets'),
		inputFileSideBar('multi_file1', 'widgets'),
		multiFileName('multi_file2', 'widgets'),
		inputFileSideBar('multi_file2', 'widgets'),
		multiFileName('multi_file3', 'widgets'),
		inputFileSideBar('multi_file3', 'widgets'),
		
		br(),
		
		# show Genoppi version at the bottom
		tags$footer(tags$p(paste0('Genoppi v',genoppi.ver), 
			style='color: grey; text-align: center;')
		)
	) # close sidebarMenu
) # close dashboardSidebar

# ----------------------------------------------------------------------------------------
# put body and sidebar together into a dashboardPage
dashboardPage(skin='black', dashboardHeader(title='Genoppi'), sidebar, body)

