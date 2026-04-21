library(ggplot2)
library(dplyr)
library(scales)

ggpie <- function(data, x, title = NULL, xlab = NULL, ylab = NULL, 
                  title_col = 'black', title_vjust = 0.5,
                  title_fam = 'serif', title_face = 'plain', 
                  title_size = 10, title_hjust = 0.5, 
                  xax_col = 'black', xax_fam = 'serif', 
                  xax_face = 'plain', xax_size = 10, 
                  xax_hjust = 0.5, xax_vjust = 0.5,
                  yax_col = 'black', yax_fam = 'serif', 
                  yax_face = 'plain', yax_size = 10, 
                  yax_hjust = 0.5, yax_vjust = 0.5,
                  add_text = FALSE, xloc = NA, yloc = NA, 
                  label = NA, tex_color = NA, tex_size = NA) {
  
  da <- data %>%
    select(x)
  
  df <- tibble::as_data_frame(table(da))
  colnames(df) <- c('group', 'value')
  
  p <- ggplot(df, aes(x = '', y = value, fill = group)) +
    geom_bar(width = 1, stat = 'identity') + xlab('') + ylab('') +
    coord_polar(theta = 'y', start = 0) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme_void() +
    scale_fill_discrete(guide = guide_legend(title = x)) +
    geom_text(aes(label = scales::percent(value/sum(value))), 
              position = position_stack(vjust = 0.5))
  
  p <- p + ggtitle(title) + xlab(xlab) + ylab(ylab) +
    theme(
      plot.title = element_text(color = title_col, family = title_fam,
                                face = title_face, size = title_size, hjust = title_hjust,
                                vjust = title_vjust),
      axis.title.x = element_text(color = xax_col, family = xax_fam,
                                  face = xax_face, size = xax_size, hjust = xax_hjust,
                                  vjust = xax_vjust),
      axis.title.y = element_text(color = yax_col, family = yax_fam,
                                  face = yax_face, size = yax_size, hjust = yax_hjust,
                                  vjust = yax_vjust)
    )
  
  if(add_text) {
    p <- p + annotate("text", x = xloc, y = yloc, label = label, 
                      color = tex_color, size = tex_size)
    p
  }
  
  p
  
}

