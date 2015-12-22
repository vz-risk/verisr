#' Returns a simple bar plot of enumeration data.table
#' 
#' given an enumeration data table and title, will plot a simple
#' bar chart, returning the ggplot object.
#' 
#' @param enumdf enumeration data table
#' @param title the title/label for the plot
#' @param solidfill a color to fill the bars with
#' @param ... other arguments ignored (for compatibility with generic)
#' @import ggplot2
#' @export
simplebar <- function(enumdf, title=NULL, solidfill="steelblue", ...) {
  if ('data.table' %in% class(enumdf)) {
    enumdf <- as.data.frame(enumdf)
  }
  # requires "enum", "x", "n" and "freq" in df
  yexp <- max(enumdf$freq)*1.4
  enumdf$reallabel <- paste0(round(enumdf$freq, 2)*100, "%")
  enumdf$reallabel[enumdf$reallabel=="0%"] <- "<1%"
  theme_vbar <- theme(
    axis.line = element_line(colour = "black", size = 0.2),
    axis.line.x = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color="black", size=12),
    legend.title=element_blank(),
    axis.text.x = element_blank(),
    ...)
  gg <- ggplot(enumdf, aes(x=enum, y=freq, label=reallabel))
  if (!is.null(title)) {
    gg <- gg + ggtitle(title)
  }
  gg <- gg + geom_bar(width=0.90, stat="identity", fill=solidfill)
  gg <- gg + geom_text(hjust=-0.1,color="black", size=4) + coord_flip()
  gg <- gg + scale_y_continuous(expand = c(0, 0), limits=c(0, yexp))
  gg <- gg + theme_vbar
  gg
}

#' Given a verisr object it will create a ggplot of the a4 grid
#' 
#' This will create the a4 grid (see Verizon's Data Breach Inves 2012) and return a ggplot object.
#' 
#' @param veris the verisr object
#' @param title the title in the plot
#' @param freq logical, display frequency on the plot
#' @import grid
#' @import ggplot2
#' @export
plota4 <- function(veris, title="A4 Grid", freq=T) {
  # intended to be pdf on letter size
  a4 <- getenum(veris, c("action", "asset.variety", "actor", "attribute"))
  a4$enum <- factor(a4$enum, levels=rev(levels(a4$enum)))
  a4$enum1 <- factor(a4$enum1, levels=rev(levels(a4$enum1)))
  a4$enum2 <- factor(a4$enum2, levels=rev(levels(a4$enum2)))
  a4$label=""
  colorlab <- a4[a4$x>0, ]
  if(freq) {
    colorlab$label <- round(colorlab$freq, 2)
    colorlab$label <- ifelse(colorlab$label>0, paste0(colorlab$label*100, "%"), "-")
  } else {
    colorlab$label <- format(colorlab$x, big.mark=",", scientific=F)
  }
  ggplot(a4, aes(enum, enum3, fill=freq, label=label)) +
    facet_grid(enum1~enum2) +
    geom_tile(fill="white", color="gray95") +
    scale_y_discrete(expand=c(0,0)) + scale_x_discrete(expand=c(0,0)) +
    geom_tile(data=colorlab, color="gray95") + 
    ggtitle(paste0(title, ", n=", format(a4$n[1], big.mark=",", scientific=F))) +
    scale_fill_gradient2(low="gray95", mid="steelblue", high="purple", midpoint=mean(range(a4$freq))) +
    geom_text(data=colorlab, size=2) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.3),
          axis.ticks=element_blank(),
          legend.position="none",
          panel.margin=unit(0, "cm"),
          axis.title=element_blank(),
          panel.border=element_rect(fill=NA, color="gray90"),
          strip.text.y=element_text(angle=90),
          strip.background=element_blank())
}

