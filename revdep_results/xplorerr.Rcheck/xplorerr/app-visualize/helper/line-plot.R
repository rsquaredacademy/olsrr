# line graph
line_graph <- function(x, y, linetype = 1, linewidth = 1,
	colour = "black", title = NA, subtitle = NA, xlabel = NA, ylabel = NA,
	add_points = FALSE, shape, size, point_col, point_bg, ylim_l = NULL,
	ylim_u = NULL, extra_lines = NULL, extra_vars = NULL, extra_cols = NULL,
	ltys = NULL, lwds = NULL, extra_p = FALSE, pcolors = NULL, pbgcolors = NULL, pshapes = NULL,
	psizes = NULL, colmain = "black", colsub = "black", colaxis = "black",
	collab = "black", fontmain = 1, fontsub = 1, fontaxis = 1, fontlab = 1,
	cexmain = 1, cexsub = 1, cexaxis = 1, cexlab = 1, text_p = NA,
	text_x_loc = NA, text_y_loc = NA, text_col = "black", text_font = NA,
	text_size = NA, m_text = NA, m_side = 3, m_line = 0.5, m_adj = 0.5,
	m_col = "black", m_font = 1, m_cex = 1, leg = FALSE, leg_x, leg_y, legend,
	leg_line,
	leg_point, leg_colour, leg_boxtype, leg_boxcol, leg_boxlty, leg_boxlwd,
	leg_boxborcol, leg_boxxjust, leg_boxyjust, leg_textcol, leg_textfont,
	leg_textcolumns, leg_texthoriz, leg_title, leg_titlecol, leg_textadj) {

	# empty plot
	plot(y, type = 'n', xaxt = 'n', main = title, sub = subtitle, xlab = xlabel,
	     ylab = ylabel, ylim =c(ylim_l, ylim_u), col.main = colmain, col.sub = colsub,
	   	col.axis = colaxis, col.lab = collab, font.main = fontmain,
	   	font.sub = fontsub, font.axis = fontaxis, font.lab = fontlab,
	   	cex.main = cexmain, cex.sub = cexsub, cex.axis = cexaxis,
	   	cex.lab = cexlab
	)

	# add lines
	lines(x = y, lty = linetype, lwd = linewidth, col = colour)

	# axis labels
	axis(1, at = seq_len(length(x)), labels = x)

	# add points
	if (add_points) {
		points(x = y, pch = shape, cex = size, col = point_col, bg = point_bg)
	}

	# additional lines and points
	if (!is.null(extra_lines)) {
	    for (i in seq_len(extra_lines)) {
	        lines(x = extra_vars[, i], lty = ltys[i], lwd = lwds[i],
	              col = extra_cols[i]
	        )
					if (extra_p) {
						points(x = extra_vars[i], pch = pshapes[i], cex = psizes[i],
							col = pcolors[i], bg = pbgcolors[i])
					}
	    }
	}

	if (leg == TRUE) {
	    legend(leg_x, leg_y,
	           legend = legend, lty = leg_line, pch = leg_point, col = leg_colour,
	           bty = leg_boxtype, bg = leg_boxcol,
	           box.lty = leg_boxlty, box.lwd = leg_boxlwd,
	           box.col = leg_boxborcol, xjust = leg_boxxjust,
	           yjust = leg_boxyjust, text.col = leg_textcol,
	           text.font = leg_textfont, ncol = leg_textcolumns,
	           horiz = leg_texthoriz, title = leg_title,
	           title.col = leg_titlecol, title.adj = leg_textadj)
	}

	# add text inside the plot
	text(text_x_loc, text_y_loc, text_p, font = text_font, col = text_col,
	     cex = text_size)

	# add text on the margins of the plot
	mtext(m_text, side = m_side, line = m_line, adj = m_adj,
	      col = m_col, font = m_font, cex = m_cex)

}
