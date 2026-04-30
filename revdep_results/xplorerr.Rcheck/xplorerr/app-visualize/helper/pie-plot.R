#  plotting functions
# scatter plot
pie_plot <- function(x, lab, edg = 200, title = NULL, clock = FALSE, rad = 0.8, initangle = 45,
  ang = 45, bord = NULL, colors = NULL, den = NULL, ltype = NULL,
  colmain = 'black', fontmain = 1, cexmain = 1) {
  
  # basic plot          
  pie(x, labels = lab, edges = edg, main = title, clockwise = clock, radius = rad, init.angle = initangle,
    density = den, angle = ang, border = bord, col = colors, 
    lty = ltype, col.main = colmain, font.main = fontmain,
    cex.main = cexmain)
  
}