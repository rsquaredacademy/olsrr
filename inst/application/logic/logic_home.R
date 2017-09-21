eda_menu <- eventReactive(input$click_descriptive, {

		fluidRow(

			column(12),

			br(),

			column(12, align = 'center',
				h5('What do you want to do?')
			),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate detailed descriptive statistics for a continuous variable: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_1',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate frequency distribution of a categorical variable: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_2',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate frequency distribution a continuous variable: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_3',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate two way table of categorical variables: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_4',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate multiple one way tables of categorical variables: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_5',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate multiple two way tables of categorical variables: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_6',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3),

			br(),
			br(),
			br(),

			column(3),

			column(4, align = 'left',
				h5('Generate grouped summary statistics: ')
			),

			column(2, align = 'left',
					actionButton(
						inputId = 'button_7',
						label = 'Click Here',
						width = '120px'
					)
			),

			column(3)

		)

	

})


output$eda_options <- renderUI({
	eda_menu()
})

observeEvent(input$click_descriptive, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_eda_home')
})

observeEvent(input$click_distributions, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_dist_home')
})

observeEvent(input$click_inference, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_infer_home')
})

observeEvent(input$click_model, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_model_home')
})

observeEvent(input$click_visualize, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_home')
	updateNavlistPanel(session, 'navlist_vizmenu', 'tab_home_viz')
})

# observeEvent(input$click_visualize, {
# 	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_lib')
# })

observeEvent(input$button_1, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_summary')
})

observeEvent(input$button_2, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_fqual')
})

observeEvent(input$button_3, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_fquant')
})

observeEvent(input$button_4, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_cross')
})

observeEvent(input$button_5, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_mult1')
})

observeEvent(input$button_6, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_mult2')
})

observeEvent(input$button_7, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_eda')
	updateNavlistPanel(session, 'navlist_eda', 'tab_gsummary')
})

observeEvent(input$button_dist_home_1, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_dist')
	updateNavlistPanel(session, 'navlist_dist', 'tab_norm')
})

observeEvent(input$button_dist_home_2, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_dist')
	updateNavlistPanel(session, 'navlist_dist', 'tab_t')
})

observeEvent(input$button_dist_home_3, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_dist')
	updateNavlistPanel(session, 'navlist_dist', 'tab_chisq')
})

observeEvent(input$button_dist_home_4, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_dist')
	updateNavlistPanel(session, 'navlist_dist', 'tab_binom')
})

observeEvent(input$button_dist_home_5, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_dist')
	updateNavlistPanel(session, 'navlist_dist', 'tab_f')
})


observeEvent(input$button_infer_home_1, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_infer1_home')
})

observeEvent(input$button_infer_home_2, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_infer2_home')
})

observeEvent(input$button_infer_home_3, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_home_analyze')
	updateNavlistPanel(session, 'navlist_home', 'tab_infer3_home')
})

# links for inferential statistics
observeEvent(input$inf_menu_1_t, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_ttest')
})

observeEvent(input$inf_menu_1_var, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_osvartest')
})

observeEvent(input$inf_menu_1_prop, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_osproptest')
})

observeEvent(input$inf_menu_1_chi, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_chigof')
})

observeEvent(input$inf_menu_1_runs, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_runs')
})

observeEvent(input$inf_menu_2_it, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_indttest')
})

observeEvent(input$inf_menu_2_pt, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_indttest')
})

observeEvent(input$inf_menu_2_binom, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_binomtest')
})

observeEvent(input$inf_menu_2_var, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_tsvartest')
})

observeEvent(input$inf_menu_2_prop, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_tsproptest')
})

observeEvent(input$inf_menu_2_chi, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_chict')
})

observeEvent(input$inf_menu_2_mcnemar, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_mcnemar')
})

observeEvent(input$inf_menu_3_anova, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_anova')
})

observeEvent(input$inf_menu_3_levene, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_levtest')
})

observeEvent(input$inf_menu_3_cochran, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_infer')
	updateNavlistPanel(session, 'navlist_infer', 'tab_cochran')
})

## visulization links

observeEvent(input$click_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_home')
	updateNavlistPanel(session, 'navlist_vizmenu', 'tab_viz_base')
})

observeEvent(input$click_ggplot2, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_home')
	updateNavlistPanel(session, 'navlist_vizmenu', 'tab_viz_gg')
})

observeEvent(input$click_prh, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_viz_home')
	updateNavlistPanel(session, 'navlist_vizmenu', 'tab_viz_others')
})

## link viz libraries to tabs
observeEvent(input$click_bar_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_bar')
})

observeEvent(input$click_bar2_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_bar2')
})

observeEvent(input$click_box_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_box')
})

observeEvent(input$click_box2_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_box2')
})

observeEvent(input$click_line_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_line')
})

observeEvent(input$click_scatter_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_scatter')
})

observeEvent(input$click_hist_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_hist')
})

observeEvent(input$click_pie_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_pie')
})

observeEvent(input$click_pie2_base, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_base')
	updateNavlistPanel(session, 'navlist_base', 'tab_pie3d')
})


## ggplot2 
observeEvent(input$click_bar_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gbar')
})

observeEvent(input$click_bar2_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gbar2')
})

observeEvent(input$click_box_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gbox')
})

observeEvent(input$click_box2_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gbox2')
})

observeEvent(input$click_line_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gline1')
})

observeEvent(input$click_scatter_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gscatter')
})

observeEvent(input$click_hist_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_ghist')
})

observeEvent(input$click_pie_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gpie')
})

observeEvent(input$click_line2_gg, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_gg')
	updateNavlistPanel(session, 'navlist_gg', 'tab_gline2')
})


## others
observeEvent(input$click_bar_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_bar_plot_1')
})

observeEvent(input$click_bar2_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_bar_plot_2')
})

observeEvent(input$click_box_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_box_plot_1')
})

observeEvent(input$click_box2_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_box_plot_2')
})

observeEvent(input$click_line_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_line_prh')
})

observeEvent(input$click_scatter_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_scatter_prh')
})

observeEvent(input$click_hist_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_hist_prh')
})

observeEvent(input$click_pie_others, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_others')
	updateNavlistPanel(session, 'navlist_others', 'tab_pie_prh')
})

## model links
observeEvent(input$model_reg_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_regress')
})

observeEvent(input$model_varsel_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_var_select')
})

observeEvent(input$model_resdiag_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_res_diag')
})

observeEvent(input$model_het_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_hetero')
})

observeEvent(input$model_coldiag_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_regcollin')
})

observeEvent(input$model_infl_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_inflobs')
})

observeEvent(input$model_fit_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_mfit')
})

observeEvent(input$model_varcontrib_click, {
	updateNavbarPage(session, 'mainpage', selected = 'tab_reg')
	updateNavlistPanel(session, 'navlist_reg', 'tab_regvarcont')
})
