#  plotting functions
# scatter plot
scatter_plot <- function(x, y, title = NA, sub = NA, xlab = NA, 
	ylab = NA, colours = "black", fill = NULL, shape = 18, xlim_min = NULL,
	xlim_max = NULL, ylim_min = NULL, ylim_max = NULL,
	size = 1, colmain = "black", colsub = "black", colaxis = "black",
	collab = "black", fontmain = 1, fontsub = 1, fontaxis = 1, fontlab = 1, 
	cexmain = 1, cexsub = 1, cexaxis = 1, cexlab = 1, text_p = NA,
	text_x_loc = NA, text_y_loc = NA, text_col = "black", text_font = NA,
	text_size = NA, m_text = NA, m_side = 3, m_line = 0.5, m_adj = 0.5,
	m_col = "black", m_font = 1, m_cex = 1, fitline = FALSE, col_abline = 'black',
  lty_abline = 1, lwd_abline = 1) {
  
  # basic plot			
  plot(x = x, y = y, type = 'p', main = title, sub = sub, xlab = xlab, ylab = ylab,
  	xlim = c(xlim_min, xlim_max), ylim = c(ylim_min, ylim_max),
  	col = colours, bg = fill, pch = shape, cex = size,
  	col.main = colmain, col.sub = colsub,
  	col.axis = colaxis, col.lab = collab, font.main = fontmain,
  	font.sub = fontsub, font.axis = fontaxis, font.lab = fontlab,
  	cex.main = cexmain, cex.sub = cexsub, cex.axis = cexaxis,
  	cex.lab = cexlab)
  
  # add text inside the plot
  text(text_x_loc, text_y_loc, text_p, font = text_font, col = text_col,
  	cex = text_size)
  
  # add text on the margins of the plot
  mtext(m_text, side = m_side, line = m_line, adj = m_adj,
  	col = m_col, font = m_font, cex = m_cex)
  
  # fit a regression line
  if (fitline) {
    abline(lm(y ~ x), col = col_abline, lty = lty_abline, lwd = lwd_abline)
  }
  

}

