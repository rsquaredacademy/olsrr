# bivariate
bar_plotb <- function(counts, horizontal = FALSE, color = 'blue', border = "black",
	besides = FALSE, title = NA, xlab = NA, labels = NA, space = NA, width = 1,
	axes = TRUE, axislty = 0, offset = 0,
	ylab = NA, colmain = "black", colaxis = "black", collab = "black",
	fontmain = 1, fontaxis = 1, fontlab = 1, cexmain = 1, cexaxis = 1, cexlab = 1,
	leg = FALSE, leg_x, leg_y, legend, leg_point, leg_colour, leg_boxtype,
	leg_boxcol, leg_boxlty, leg_boxlwd, leg_boxborcol, leg_boxxjust, leg_boxyjust,
	leg_textcol, leg_textfont, leg_textcolumns, leg_texthoriz, leg_title,
	leg_titlecol, leg_textadj, text_p = NA, text_x_loc = NA, text_y_loc = NA,
	text_col = "black", text_font = NA, text_size = NA, m_text = NA, m_side = 3,
	m_line = 0.5, m_adj = 0.5, m_col = "black", m_font = 1, m_cex = 1) {

	if (leg == TRUE) {
		legtext <- NULL
	} else {
		legtext <- rownames(counts)
	}

	# bar plot
	barplot(height = counts, horiz = horizontal, col = color, border = border,
	beside = besides, legend = legtext, main = title, xlab = xlab, width = width,
	density = NULL, angle = 45, axes = axes, axis.lty = axislty,
	offset = offset, ylab = ylab, col.main = colmain, col.axis = colaxis,
	col.lab = collab, font.main = fontmain, font.axis = fontaxis,
	font.lab = fontlab, cex.main = cexmain, cex.axis = cexaxis, cex.lab = cexlab)

	if (is.null(leg_colour)) {
		pcol <- 'blue'
	} else {
		pcol <- leg_colour
	}

  	# legend
  	if (leg == TRUE) {
  		legend(leg_x, leg_y,
               legend = legend, pch = leg_point, col = pcol,
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

	# add text on the mar-gins of the plot
   	mtext(m_text, side = m_side, line = m_line, adj = m_adj,
  		col = m_col, font = m_font, cex = m_cex)
}
