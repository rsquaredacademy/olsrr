library(plotrix)

# 3D pie chart
pie3_plot <- function(x, lab = NULL, edg = NA, title = NULL, rad = 1,
 	bord = NULL, colors = NULL, high = 0.1, begin = 0, labpos = NULL, 
 	labcol = NULL, labcex = 1.5, labrad = 1.25, explo = 0, shd = 0.8, 
  	colmain = 'black', fontmain = 1, cexmain = 1) {
  
  # basic plot          
  pie3D(x, labels = lab, edges = edg, main = title, radius = rad, 
  	border = bord, col = colors, height = high, start = begin, 
  	labelpos = labpos, labelcol = labcol, labelcex = labcex,
  	labelrad = labrad, explode = explo, shade = shd, col.main = colmain, 
  	font.main = fontmain, cex.main = cexmain)
  
}