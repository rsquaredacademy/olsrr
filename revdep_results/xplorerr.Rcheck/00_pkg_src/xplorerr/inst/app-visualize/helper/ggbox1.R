ggbox1 <- function(data, y, notch = FALSE, fill = 'blue', col = 'black',
                  o_col = 'red', o_fill = 'yellow', o_shape = 22, theme = "Default",
                  o_alpha = 0.8, o_size = 2, add_jitter = FALSE, 
                  j_width = 0.1, j_height = 0.1, j_fill = 'blue',
                  j_col = 'black', j_shape = 22, j_size = 2, j_alpha = 0.8,
                  horizontal = FALSE, yaxlimit = FALSE, y1 = NA, y2 = NA, 
                  title = NULL, xlab = NULL, ylab = NULL, sub = NULL,
                  title_col = 'black', title_vjust = 0.5,
                  title_fam = 'serif', title_face = 'plain', 
                  title_size = 10, title_hjust = 0.5, 
                  sub_col = 'black', sub_fam = 'serif', sub_face = 'plain', 
                  sub_size = 10, sub_hjust = 0.5, sub_vjust = 0.5,
                  xax_col = 'black', xax_fam = 'serif', 
                  xax_face = 'plain', xax_size = 10, 
                  xax_hjust = 0.5, xax_vjust = 0.5,
                  yax_col = 'black', yax_fam = 'serif', 
                  yax_face = 'plain', yax_size = 10, 
                  yax_hjust = 0.5, yax_vjust = 0.5,
                  remove_xax = TRUE, remove_yax = FALSE,
                  add_text = FALSE, xloc = NA, yloc = NA, 
                  label = NA, tex_color = NA, tex_size = NA) {
  
  p <- ggplot(data, aes(x = factor(1), y = .data[[y]])) + 
    geom_boxplot(notch = notch, fill = fill, color = col,
                 outlier.color = o_col, outlier.fill = o_fill,
                 outlier.shape = o_shape, outlier.alpha = o_alpha,
                 outlier.size = o_size) 
  
  if (add_jitter) {
    p <- p + geom_jitter(width = j_width, height = j_height, alpha = j_alpha,
      fill = j_fill, col = j_col, shape = j_shape, size = j_size)
  }
    
  
  p <- p + ggtitle(title) + xlab(xlab) + ylab(ylab) +
    theme(
      plot.title = element_text(color = title_col, family = title_fam,
                                face = title_face, size = title_size, hjust = title_hjust,
                                vjust = title_vjust),
      plot.subtitle = element_text(color = sub_col, family = sub_fam,
                                face = sub_face, size = sub_size, hjust = sub_hjust,
                                vjust = sub_vjust),
      axis.title.x = element_text(color = xax_col, family = xax_fam,
                                  face = xax_face, size = xax_size, hjust = xax_hjust,
                                  vjust = xax_vjust),
      axis.title.y = element_text(color = yax_col, family = yax_fam,
                                  face = yax_face, size = yax_size, hjust = yax_hjust,
                                  vjust = yax_vjust)
    )
  
  if (yaxlimit) {
    p <- p + ylim(y1, y2)
    p
  }
  
  
  if(remove_xax) {
    p <- p + theme(
      axis.title.x = element_blank()
    )
    p
  }
  
  if(remove_yax) {
    p <- p + theme(
      axis.title.y = element_blank()
    )
    p
  }
  
  if (horizontal) {
    p <- p + coord_flip()
  }
  
  if(add_text) {
    p <- p + annotate("text", x = xloc, y = yloc, label = label, 
                      color = tex_color, size = tex_size)
    p
  }

  if (theme == "Classic Dark") {
    p <- p + theme_bw()
  } else if (theme == "Light") {
    p <- p + theme_light()
  } else if (theme == "Minimal") {
    p <- p + theme_minimal()
  } else if (theme == "Dark") {
    p <- p + theme_dark()
  } else if (theme == "Classic") {
    p <- p + theme_classic()
  } else if (theme == "Empty") {
    p <- p + theme_void()
  }
  
  p

}


