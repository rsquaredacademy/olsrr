# box plots
# univariate
box_plotu <- function(x, title = NA, xlabel = NA, ylabel = NA, colour = 'blue', borders = 'black',
	horiz = FALSE, notches = FALSE, ranges = 1.5, outlines = TRUE, varwidths = FALSE,
	colmain = "black", colsub = "black", colaxis = "black", collab = "black", 
	fontmain = 1, fontsub = 1, fontaxis = 1, fontlab = 1, 
	cexmain = 1, cexsub = 1, cexaxis = 1, cexlab = 1, text_p = NA,
	text_x_loc = NA, text_y_loc = NA, text_col = "black", text_font = NA,
	text_size = NA, m_text = NA, m_side = 3, m_line = 0.5, m_adj = 0.5,
	m_col = "black", m_font = 1, m_cex = 1) {

	boxplot(x, main = title, xlab = xlabel, ylab = ylabel, col = colour, border = borders,
	horizontal = horiz, notch = notches, range = ranges, outline = outlines,
	varwidth = varwidths, col.main = colmain, col.sub = colsub,
  	col.axis = colaxis, col.lab = collab, font.main = fontmain,
  	font.sub = fontsub, font.axis = fontaxis, font.lab = fontlab,
  	cex.main = cexmain, cex.sub = cexsub, cex.axis = cexaxis,
  	cex.lab = cexlab)

  	# add text inside the plot
   	text(text_x_loc, text_y_loc, text_p, font = text_font, col = text_col,
  		cex = text_size)
  
	# add text on the mar-gins of the plot
   	mtext(m_text, side = m_side, line = m_line, adj = m_adj,
  		col = m_col, font = m_font, cex = m_cex)

}
