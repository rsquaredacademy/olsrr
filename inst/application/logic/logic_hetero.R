# Breusch Pagan Test
d_het_bp <- eventReactive(input$submit_het_bp, {
	# validate(need((input$het_bp_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

mnames <- eventReactive(input$submit_regress, {
    k <- colnames(model.matrix(model()$model)[, -1])
    k
})


het_bp_mod <- eventReactive(input$submit_het_bp, {
	k <- lm(input$het_bp_fmla, data = d_het_bp())
  k
})

bpvars <- eventReactive(input$submit_het_bp, {
    k <- colnames(model.matrix(het_bp_mod())[, -1])
    k
})

observe({
  if (input$het_bp_fv == FALSE) {
    if (input$bp_use_prev) {
      updateSelectInput(session, inputId = "het_bp_vars", choices = mnames(),
          selected = mnames()[1])
    } else {
      updateSelectInput(session, inputId = "het_bp_vars", choices = bpvars(),
          selected = bpvars()[1])
    }
  } else {
    if (input$bp_use_prev) {
      updateSelectInput(session, inputId = "het_bp_vars", choices = mnames(),
          selected = '')
    } else {
      updateSelectInput(session, inputId = "het_bp_vars", choices = bpvars(),
          selected = '')
    }
  }
})

result_bp <- eventReactive(input$submit_het_bp, {
  if (input$het_bp_fv == FALSE) {
    if (input$bp_use_prev) {
      ols_test_breusch_pagan(all_use_n(), as.logical(input$het_bp_fv), as.logical(input$het_bp_rhs),
        as.logical(input$het_bp_mult), as.character(input$het_bp_padj), input$het_bp_vars)
    } else {
      ols_test_breusch_pagan(het_bp_mod(), as.logical(input$het_bp_fv), as.logical(input$het_bp_rhs),
        as.logical(input$het_bp_mult), as.character(input$het_bp_padj), input$het_bp_vars)
    }
  } else {
    if (input$bp_use_prev) {
      ols_test_breusch_pagan(all_use_n(), as.logical(input$het_bp_fv), as.logical(input$het_bp_rhs),
        as.logical(input$het_bp_mult), as.character(input$het_bp_padj))
    } else {
      ols_test_breusch_pagan(het_bp_mod(), as.logical(input$het_bp_fv), as.logical(input$het_bp_rhs),
        as.logical(input$het_bp_mult), as.character(input$het_bp_padj))
    }
  }
})

output$het_bp_out <- renderPrint({
  result_bp()
})

# f test for heteroskedasticity
d_het_f <- eventReactive(input$submit_het_f, {
	# validate(need((input$het_f_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

het_f_mod <- eventReactive(input$submit_het_f, {
	k <- lm(input$het_f_fmla, data = d_het_f())
  k
})

fvars <- eventReactive(input$submit_het_f, {
    k <- colnames(model.matrix(het_f_mod())[, -1])
    k
})

observe({
  if (input$het_f_fv == FALSE) {
    if (input$f_use_prev) {
      updateSelectInput(session, inputId = "het_f_vars", choices = mnames(),
          selected = mnames()[1])
    } else {
      updateSelectInput(session, inputId = "het_f_vars", choices = fvars(),
          selected = fvars()[1])
    }
  } else {
    if (input$f_use_prev) {
      updateSelectInput(session, inputId = "het_f_vars", choices = mnames(),
          selected = '')
    } else {
      updateSelectInput(session, inputId = "het_f_vars", choices = fvars(),
          selected = '')
    }
  }
})

result_f <- eventReactive(input$submit_het_f, {
  if (input$het_f_fv == FALSE) {
    if (input$f_use_prev) {
      ols_test_f(all_use_n(), as.logical(input$het_f_fv), as.logical(input$het_f_rhs),
        input$het_f_vars)
    } else {
      ols_test_f(het_f_mod(), as.logical(input$het_f_fv), as.logical(input$het_f_rhs),
        input$het_f_vars)
    }
  } else {
    if (input$f_use_prev) {
      ols_test_f(all_use_n(), as.logical(input$het_f_fv), as.logical(input$het_f_rhs))
    } else {
      ols_test_f(het_f_mod(), as.logical(input$het_f_fv), as.logical(input$het_f_rhs))
    }
  }
})


output$het_f_out <- renderPrint({
  result_f()
})


# score test
d_het_score <- eventReactive(input$submit_het_score, {
	# validate(need((input$het_score_fmla != ''), 'Please specify model'))
    data <- final_split$train
})

het_score_mod <- eventReactive(input$submit_het_score, {
	k <- lm(input$het_score_fmla, data = d_het_score())
  k
})

scorevars <- eventReactive(input$submit_het_score, {
    k <- colnames(model.matrix(het_score_mod())[, -1])
    k
})

observe({
  if (input$het_score_fv == FALSE) {
    if (input$score_use_prev) {
      updateSelectInput(session, inputId = "het_score_vars", choices = mnames(),
          selected = mnames()[1])
    } else {
      updateSelectInput(session, inputId = "het_score_vars", choices = scorevars(),
          selected = scorevars()[1])
    }
  } else {
    if (input$score_use_prev) {
      updateSelectInput(session, inputId = "het_score_vars", choices = mnames(),
          selected = '')
    } else {
      updateSelectInput(session, inputId = "het_score_vars", choices = scorevars(),
          selected = '')
    }
  }
})

result_score <- eventReactive(input$submit_het_score, {
  if (input$het_score_fv == FALSE) {
    if (input$score_use_prev) {
      ols_test_score(all_use_n(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs),
        input$het_score_vars)
    } else {
      ols_test_score(het_score_mod(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs),
        input$het_score_vars)
    }
  } else {
    if (input$score_use_prev) {
      ols_test_score(all_use_n(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs))
    } else {
      ols_test_score(het_score_mod(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs))
    }
  }
})

output$het_score_out <- renderPrint({
  result_score()
})


# output$het_score_out <- renderPrint({
#   if (input$het_score_fv == FALSE) {
#     if (input$score_use_prev) {
#       ols_score_test(all_use_n(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs),
#         input$het_score_vars)
#     } else {
#       ols_score_test(het_score_mod(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs),
#         input$het_score_vars)
#     }
#   } else {
#     if (input$score_use_prev) {
#       ols_score_test(all_use_n(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs))
#     } else {
#       ols_score_test(het_score_mod(), as.logical(input$het_score_fv), as.logical(input$het_score_rhs))
#     }
#   }
# })


# bartlett test
observe({
  updateSelectInput(session,inputId = "var_bartest",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_bartestg1",
    choices = names(data()), selected = '')
  updateSelectInput(session,inputId = "var_bartestg2",
    choices = names(data()), selected = '')
})

observeEvent(input$button_split_no, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_bartest',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_bartestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_bartest',
              choices = '', selected = '')
             updateSelectInput(session, 'var_bartestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_bartest', choices = names(num_data))
             updateSelectInput(session, 'var_bartestg1', choices = names(num_data))
        }

    if (is.null(dim(f_data))) {
            k <- final_split$train %>% map(is.factor) %>% unlist()
            j <- names(which(k == TRUE))
            fdata <- tibble::as_data_frame(f_data)
            colnames(fdata) <- j
            updateSelectInput(session, 'var_bartestg2',
              choices = names(fdata), selected = names(fdata))
        } else if (ncol(f_data) < 1) {
             updateSelectInput(session, 'var_bartestg2',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_bartestg2', choices = names(f_data))
        }
})

observeEvent(input$submit_part_train_per, {
    f_data <- final_split$train[, sapply(final_split$train, is.factor)]
    num_data <- final_split$train[, sapply(final_split$train, is.numeric)]
    if (is.null(dim(num_data))) {
            k <- final_split$train %>% map(is.numeric) %>% unlist()
            j <- names(which(k == TRUE))
            numdata <- tibble::as_data_frame(num_data)
            colnames(numdata) <- j
            updateSelectInput(session, 'var_bartest',
              choices = names(numdata), selected = names(numdata))
            updateSelectInput(session, 'var_bartestg1',
              choices = names(numdata), selected = names(numdata))
        } else if (ncol(num_data) < 1) {
             updateSelectInput(session, 'var_bartest',
              choices = '', selected = '')
             updateSelectInput(session, 'var_bartestg1',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_bartest', choices = names(num_data))
             updateSelectInput(session, 'var_bartestg1', choices = names(num_data))
        }

    if (is.null(dim(f_data))) {
            k <- final_split$train %>% map(is.factor) %>% unlist()
            j <- names(which(k == TRUE))
            fdata <- tibble::as_data_frame(f_data)
            colnames(fdata) <- j
            updateSelectInput(session, 'var_bartestg2',
              choices = names(fdata), selected = names(fdata))
        } else if (ncol(f_data) < 1) {
             updateSelectInput(session, 'var_bartestg2',
              choices = '', selected = '')
        } else {
             updateSelectInput(session, 'var_bartestg2', choices = names(f_data))
        }
})

d_bartest <- eventReactive(input$submit_bartest, {
	# validate(need((input$var_bartest != ''), 'Please select variables'))
  req(input$var_bartest)
  data <- final_split$train[, c(input$var_bartest)]
  ols_test_bartlett(data)
})

output$bartest_out <- renderPrint({
  d_bartest()
})

d_bartestg <- eventReactive(input$submit_bartestg, {
	# validate(need((input$var_bartestg1 != '' & input$var_bartestg2 != ''), 'Please select variables'))
  req(input$var_bartestg1)
  req(input$var_bartestg2)
  data <- final_split$train[, c(input$var_bartestg1, input$var_bartestg2)]
  k <- ols_test_bartlett(data[, 1], group_var = data[, 2])
  k
})

output$bartestg_out <- renderPrint({
  d_bartestg()
})


d_bartmod <- eventReactive(input$submit_bartestf, {
	# validate(need((input$bartest_fmla != ''), 'Please specify a model.'))
	data <- final_split$train
  k <- lm(input$bartest_fmla, data = data)
  ols_test_bartlett(k)
})

# bartmod <- reactive({

# 	k
# })

output$bartestf_out <- renderPrint({
  d_bartmod()
})
