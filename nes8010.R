# Please source this script before running the NES8010 multivariate examples
# The script provides additional functions to make using the analyses simpler,
# and change the default plots to a format compatible with ggplot2

# grid package needs installing for ordi_identify
my_packages <- c("grid")   # Specify extra packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)       
library(grid)

#' Interactive identify
#'
#' Interactive identify ggvegan species
#' @param plotname Name of a plot created with \code{\link{ordi_plot}}
#' @param size Font size of labels (default = 3)
#' @param ... Other optional parameters
#'
#' @details
#' This function is designed to be run interactively. First create a standard
#' ordination using \code{\link{ordi_pca}}, \code{\link{ordi_rda}},
#' \code{\link{ordi_ca}}, \code{\link{ordi_cca}} or \code{\link{ordi_nmds}}.
#' Then call \code{\link{ordi_plot}} but make sure that the plot results is
#' stored in an R object. Then apply this function to that object, and hit the
#' \emph{Esc} key to exit function.
#' \strong{Note:} In RStudio only the most recently displayed plot can be
#' labelled with this function, so avoid using the back arrow keys in the RStudio
#' plot window. Labelling may not always occur on first click, and is more
#' difficult on constrained ordination plots.
#'
#' @return The original ordiname is modified with labels
#'
#' @author Roy Sanderson, School of Natural & Environmental Science, Newcastle
#' University roy.sanderson@newcastle.ac.uk
#'
#' @examples
#' if(interactive()){
#'
#' # Unconstrained ordination
#' data(dune)
#' data(dune.env)
#' dune_pca <- ordi_pca(dune)
#' dune_plt <- ordi_plot(dune_pca, layers="species", geom="point") # defaults to sites and species
#' dune_plt  # Display the plot
#' ordi_identify(dune_plt) # Hit Esc key to exit
#'
#' # Constrained ordination
#' dune_rda <- ordi_rda(dune ~ A1 + Management, data=dune.env)
#' # displays spp and constraints.
#' # Constraints are "biplot" for continuous and "centroids" for categorical
#' dune_plt <- ordi_plot(dune_rda, layers=c("species", "biplot", "centroids"), geom="point")
#' dune_plt  # Display the plot
#' ordi_identify(dune_plt) # Hit Esc key to exit
#'
#' }
#' @import grid
#' @import mosaic
#' @import vegan
#' @export
ordi_identify <- function(plotname, size=3, ...){
  print("Click on plot to label points; hit Esc key to exit")
  plot_data <- plotname[["layers"]][[1]]$data
  depth <- downViewport('panel.7-5-7-5')
  x <- plot_data[,3]
  y <- plot_data[,4]
  labels <- plot_data[,2]
  pushViewport(dataViewport(x,y))
  pick <- grid.locator('in')
  while(!is.null(pick)){
    tmp <- grid.locator('in')
    tmp.n <- as.numeric(tmp)
    tmp2.x <- as.numeric(convertX( unit(x,'native'), 'in' ))
    tmp2.y <- as.numeric(convertY( unit(y,'native'), 'in' ))
    
    w <- which.min( (tmp2.x-tmp.n[1])^2 + (tmp2.y-tmp.n[2])^2 )
    popViewport(n=1)
    upViewport(depth)
    print(last_plot() + annotate("text", label=labels[w], x = x[w], y = y[w],
                                 size = size, hjust=0.5, vjust=-0.5))
    depth <- downViewport('panel.7-5-7-5')
    pushViewport(dataViewport(x,y))
    pick <- grid.locator('in')
  }
  return(last_plot())
}

#' Principal components analysis
#'
#' Wrapper function with vegan for PCA
#' @param spp_data Dataframe of attributes (columns) by samples (rows)
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_pca <- function(spp_data, ...){
  spp_data_pca <- rda(spp_data, ...)
  class(spp_data_pca) <- c("rda", "cca", "pca")
  spp_data_pca
}

#' Redundancy analysis
#'
#' Wrapper function with vegan for RDA
#' @param formula Dataframe of attributes (columns) by samples (rows) as response
#' and one or more explanatory variables from a second dataframe
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_rda <- {
  rda
}

#' Correspondence analysis
#'
#' Wrapper function with vegan for CA
#' @param spp_data Dataframe of attributes (columns) by samples (rows)
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_ca <- function(spp_data, ...){
  spp_data_ca <- cca(spp_data, ...)
  class(spp_data_ca) <- c("rda", "cca", "ca")
  spp_data_ca
}

#' Canonical correspondence analysis
#'
#' Wrapper function with vegan for CCA
#' @param formula Dataframe of attributes (columns) by samples (rows) as response
#' and one or more explanatory variables from a second dataframe
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_cca <- {
  cca
}

#' Non-metric multidimensional analysis
#'
#' Wrapper function with vegan for metaMDS
#' @param spp_data Dataframe of attributes (columns) by samples (rows)
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_nmds <- function(spp_data, ...){
  spp_data_ca <- metaMDS(spp_data, ...)
}

#' Ordination scores from constrained or unconstrained ordination
#'
#' Wrapper function with ggvegan for fortify
#' @param ordi_object Result of ordination
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_scores <- function(ordi_object, ...){
  fortify(ordi_object, ...)
}

#' Stepwise selection of constrained ordination
#'
#' Wrapper function with vegan for ordistep
#' @param ordi_object Either a cca or rda object
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
ordi_step <- function(ordi_object, ...){
  ordistep(ordi_object, ...)
}

#' Multiple plot function
#'
#' Display plot objects in multiple columns, rows, or other combinations
#' @param ... ggplot (or gf_ plot) objects
#' @param plotlist alternative input as a list of ggplot objects
#' @param cols Number of columns in layout
#' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' @details
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#'
#' @return Displays multi-way plot, but returns NULL
#'
#' @examples
#' # Create a couple of normal distributions of different sample sizes
#' small_normal <- rnorm(25)
#' medium_normal   <- rnorm(100)
#' big_normal   <- rnorm(100000)
#'
#' # Plot their frequency histograms, but store rather than display
#' small_normal_plt <- gf_histogram(~ small_normal)
#' medium_normal_plt <- gf_histogram(~ medium_normal)
#' big_normal_plt   <- gf_histogram(~ big_normal)
#'
#' # Display two plots side-by-side
#' multi_plot(small_normal_plt, big_normal_plt, cols=2)
#'
#' # Display two plots one above the other
#' multi_plot(small_normal_plt, big_normal_plt, cols=1)
#'
#' # Display three plots in a grid
#' # Note use of layout 1, 2, 3, 3 coding to put
#' # the big_normal_plt (third named one) across the bottom
#' multi_plot(small_normal_plt, medium_normal_plt, big_normal_plt,
#'            layout=matrix(c(1,2,3,3), nrow=2, byrow=TRUE))
#'
#' @import grid
#' @import mosaic
#' @export
multi_plot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Type 3 Sums of squares
#'
#' Wrapper function with car for Anova
#' @param lm_mod Results of lm function
#' @param ... Other options to function
#'
#' @details To be written
#'
#' @export
anova3 <- function(lm_mod, ...){
  Anova(lm_mod, ...)
}



##' @title Fortify a \code{"cca"} object.
##'
##' @description
##' Fortifies an object of class \code{"cca"} to produce a
##' data frame of the selected axis scores in long format, suitable for
##' plotting with \code{\link[ggplot2]{ggplot}}.
##'
##' @details
##' TODO
##'
##' @param model an object of class \code{"cca"}, the result of a call to
##' \code{\link[vegan]{cca}}, \code{\link[vegan]{rda}}, or
##' \code{\link[vegan]{capscale}}.
##' @param data currently ignored.
##' @param axes numeric; which axes to extract scores for.
##' @param display numeric; the scores to extract in the fortified object.
##' @param ... additional arguments passed to \code{\link[vegan]{scores.cca}},
##'   and \code{\link[vegan]{scores.rda}}.
##' @return A data frame in long format containing the ordination scores.
##' The first two components are the axis scores.
##' @author Gavin L. Simpson
##'
##' @method fortify cca
##' @export
##'
##' @examples
##' require(vegan)
##' data(dune)
##' data(dune.env)
##'
##' sol <- cca(dune ~ A1 + Management, data = dune.env)
##' head(fortify(sol))
`fortify.cca` <- function(model, data, axes = 1:6,
                          display = c("sp", "wa", "lc", "bp", "cn"), ...) {
  ## extract scores
  scrs <- scores(model, choices = axes, display = display, ...)
  ## handle case of only 1 set of scores
  if (length(display) == 1L) {
    scrs <- list(scrs)
    nam <- switch(display,
                  sp = "species",
                  species = "species",
                  wa = "sites",
                  sites = "sites",
                  lc = "constraints",
                  bp = "biplot",
                  cn = "centroids",
                  stop("Unknown value for 'display'"))
    names(scrs) <- nam
  }
  miss <- vapply(scrs, function(x ) all(is.na(x)), logical(1L))
  scrs <- scrs[!miss]
  nams <- names(scrs)
  nr <- vapply(scrs, FUN = NROW, FUN.VALUE = integer(1))
  df <- do.call('rbind', scrs)
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df <- cbind(Score = factor(rep(nams, times = nr)),
              Label = unlist(lapply(scrs, rownames), use.names = FALSE),
              df)
  df
}


##' @title Fortify a \code{"metaMDS"} object.
##'
##' @description
##' Fortifies an object of class \code{"metaMDS"} to produce a
##' data frame of the selected axis scores in long format, suitable for
##' plotting with \code{\link[ggplot2]{ggplot}}.
##'
##' @details
##' TODO
##'
##' @param  model an object of class \code{"metaMDS"}, the result of a call
##' to \code{\link[vegan]{metaMDS}}.
##' @param data currently ignored.
##' @param ... additional arguments passed to
##' \code{\link[vegan]{scores.metaMDS}}. Note you can't use \code{display}.
##' @return A data frame in long format containing the ordination scores.
##' The first two components are the axis scores.
##' @author Gavin L. Simpson
##'
##' @method fortify metaMDS
##' @export
##'
##' @importFrom ggplot2 fortify
##' @importFrom vegan scores
##'
##' @examples
##' ## load example data
##' require(vegan)
##' data(dune)
##'
##' ord <- metaMDS(dune)
##' head(fortify(ord))
# `fortify.metaMDS` <- function(model, data, ...) {
#   samp <- scores(model, display = "sites", ...)
#   spp <- tryCatch(scores(model, display = "species", ...),
#                   error = function(c) {NULL})
#   if (!is.null(spp)) {
#     df <- rbind(samp, spp)
#     df <- as.data.frame(df)
#     df <- cbind(Score = factor(rep(c("sites","species"),
#                                    c(nrow(samp), nrow(spp)))),
#                 Label = c(rownames(samp), rownames(spp)),
#                 df)
#   } else {
#     df <- data.frame(Score = factor(rep("sites", nrow(df))),
#                      Label = rownames(samp),
#                      samp)
#   }
#   rownames(df) <- NULL
#   df
# }

# `fortify.metaMDS` <- function(model, data, axes = 1:2,
#                           display = c("sites"), ...) {
#   ## extract scores
#   scrs <- scores(model, choices = axes, display = display, ...)
#   ## handle case of only 1 set of scores
#   if (length(display) == 1L) {
#      scrs <- list(scrs)
#      nam <- switch(display,
#                   sp = "species",
#                   species = "species",
#                   si = "sites",
#                   sites = "sites",
#                   stop("Unknown value for 'display'"))
#     names(scrs) <- nam
#   }
#   miss <- vapply(scrs, function(x ) all(is.na(x)), logical(1L))
#   scrs <- scrs[!miss]
#   nams <- names(scrs)
#   nr <- vapply(scrs, FUN = NROW, FUN.VALUE = integer(1))
#   df <- do.call('rbind', scrs)
#   rownames(df) <- NULL
#   df <- as.data.frame(df)
#   df <- cbind(Score = factor(rep(nams, times = nr)),
#               Label = unlist(lapply(scrs, rownames), use.names = FALSE),
#               df)
#   df
# }
# 




##' @title ggplot-based plot for objects of class \code{"cca"}
##'
##' @description
##' Produces a multi-layer ggplot object representing the output of objects produced by \code{\link[vegan]{cca}}, or \code{\link[vegan]{capscale}}.
##'
##' @details
##' TODO
##'
##' @param object an object of class \code{"cca"}, the result of a call to \code{\link[vegan]{cca}} or \code{\link[vegan]{capscale}}.
##' @param axes numeric; which axes to plot, given as a vector of length 2.
##' @param geom character; which geoms to use for the layers. Can be a
##' vector of length equal to \code{length(display)}, in which case the
##' \emph{i}th element of \code{type} refers to the \emph{i}th element
##' of \code{display}.
##' @param layers character; which scores to plot as layers
##' @param legend.position character or two-element numeric vector; where to position the legend. See \code{\link[ggplot2]{theme}} for details. Use \code{"none"} to not draw the legend.
##' @param xlab character; label for the x-axis
##' @param ylab character; label for the y-axis
##' @param title character; subtitle for the plot
##' @param subtitle character; subtitle for the plot
##' @param caption character; caption for the plot
##' @param ... Additional arguments passed to \code{\link{fortify.cca}}.
##' @return Returns a ggplot object.
##' @author Gavin L. Simpson
##'
##' @method ordi_plot cca
##' @export
##'
##' @importFrom grid arrow unit
##' @importFrom ggplot2 autoplot ggplot geom_point geom_text geom_segment labs coord_fixed aes_string
##'
##' @examples
##' require(vegan)
##' data(dune)
##' data(dune.env)
##'
##' sol <- ordi_cca(dune ~ A1 + Management, data = dune.env)
##' ordi_plot(sol)
`ordi_plot.cca` <- function(object, axes = c(1,2), geom = c("point", "text"),
                            layers = c("species", "sites", "biplot", "centroids"),
                            legend.position = "none",
                            title = NULL, subtitle = NULL, caption = NULL,
                            ylab, xlab, ...) {
  axes <- rep(axes, length.out = 2L)
  obj <- fortify(object, axes = axes, ...)
  LAYERS <- levels(obj$Score)
  ## sort out x, y aesthetics
  vars <- getDimensionNames(obj)
  ## match the geom
  geom <- match.arg(geom)
  point <- TRUE
  if (isTRUE(all.equal(geom, "text"))) {
    point <- FALSE
  }
  ## subset out the layers wanted
  ### need something here first to match acceptable ones?
  ### or just check that the layers selected would return a df with
  ### at least 1 row.
  obj <- obj[obj$Score %in% layers, , drop = FALSE]
  ## skeleton layer
  plt <- ggplot()
  ## add plot layers as required
  want <- obj$Score %in% c("species", "sites")
  if (point) {
    plt <- plt +
      geom_point(data = obj[want, , drop = FALSE ],
                 aes_string(x = vars[1], y = vars[2], shape = 'Score',
                            colour = 'Score'))
  } else {
    plt <- plt +
      geom_text(data = obj[want, , drop = FALSE ],
                aes_string(x = vars[1], y = vars[2], label = 'Label',
                           colour = 'Score'))
  }
  ## remove biplot arrows for centroids if present
  if(all(c("biplot","centroids") %in% LAYERS)) {
    want <- obj$Score == "biplot"
    tmp <- obj[want, ]
    obj <- obj[!want, ]
    bnam <- tmp[, "Label"]
    cnam <- obj[obj$Score == "centroids", "Label"]
    obj <- rbind(obj, tmp[!bnam %in% cnam, , drop = FALSE])
  }
  if(any(want <- obj$Score == "constraints")) {
    if (point) {
      plt <- plt + geom_point(data = obj[want, , drop = FALSE ],
                              aes_string(x = vars[1], y = vars[2]))
    } else {
      plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                             aes_string(x = vars[1], y = vars[2],
                                        label = 'Label'))
    }
  }
  if(any(want <- obj$Score == "biplot")) {
    if (length(layers) > 1) {
      mul <- arrowMul(obj[want, vars, drop = FALSE],
                      obj[!want, vars, drop = FALSE])
      obj[want, vars] <- mul * obj[want, vars]
    }
    col <- "navy"
    plt <- plt +
      geom_segment(data = obj[want, , drop = FALSE ],
                   aes_string(x = 0, y = 0, xend = vars[1], yend = vars[2]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   colour = col)
    obj[want, vars] <- 1.1 * obj[want, vars]
    plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                           aes_string(x = vars[1], y = vars[2], label = 'Label'))
  }
  if(any(want <- obj$Score == "centroids")) {
    plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                           aes_string(x = vars[1], y = vars[2], label = 'Label'),
                           colour = "navy")
  }
  if(missing(xlab)) {
    xlab <- vars[1]
  }
  if(missing(ylab)) {
    ylab <- vars[2]
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                    caption = caption)
  ## add equal scaling
  plt <- plt + coord_fixed(ratio = 1)
  ## do we want a legend
  plt <- plt + theme(legend.position = legend.position)
  plt
}

## `aplot.cca` <- `ordi_plot.cca`




##' @title ggplot-based plot for objects of class \code{"metaMDS"}
##'
##' @description
##' Produces a multi-layer ggplot object representing the output of
##' objects produced by \code{\link[vegan]{metaMDS}}.
##'
##' @details
##' TODO
##'
##' @param object an object of class \code{"metaMDS"}, the result of a call
##' to \code{\link[vegan]{metaMDS}}.
##' @param axes numeric; which axes to plot, given as a vector of length 2.
##' @param geom character; which geoms to use for the layers. Can be a
##' vector of length equal to \code{length(display)}, in which case the
##' \emph{i}th element of \code{type} refers to the \emph{i}th element
##' of \code{display}.
##' @param layers character; which scores to plot as layers
##' @param legend.position character or two-element numeric vector; where to position the legend. See \code{\link[ggplot2]{theme}} for details. Use \code{"none"} to not draw the legend.
##' @param xlab character; label for the x-axis
##' @param ylab character; label for the y-axis
##' @param title character; subtitle for the plot
##' @param subtitle character; subtitle for the plot
##' @param caption character; caption for the plot
##' @param ... Additional arguments passed to \code{\link{fortify.metaMDS}}.
##'
##' @return Returns a ggplot object.
##'
##' @author Gavin L. Simpson
##'
##' @method ordi_plot metaMDS
##' @export
##'
##' @importFrom grid arrow unit
##' @importFrom ggplot2 autoplot ggplot geom_point geom_text labs coord_fixed aes_string
##'
##' @examples
##' ## load example data
##' require(vegan)
##' data(dune)
##'
##' sol <- ordi_nmds(dune)
##' ordi_plot(sol)
`ordi_plot.metaMDS` <- function(object, axes=c(1,2), geom = c("point", "text"),
                                layers = c("species", "sites"),
                                legend.position = "none",
                                title = NULL, subtitle = NULL, caption = NULL,
                                ylab, xlab, ...) {
  axes <- rep(axes, length.out = 2L)
  display <- layers
  obj <- fortify.metaMDS(object, ...)
  obj <- obj[obj$Score %in% layers, ]
  ## sort out x, y aesthetics
  vars <- getDimensionNames(obj)
  ## skeleton layer
  plt <- ggplot()
  geom <- match.arg(geom)
  point <- TRUE
  if (isTRUE(all.equal(geom, "text"))) {
    point <- FALSE
  }
  if (point) {
    plt <- plt + geom_point(data = obj,
                            aes_string(x = vars[1], y = vars[2], shape = 'Score',
                                       colour = 'Score'))
  } else {
    plt <- plt + geom_text(data = obj,
                           aes_string(x = vars[1], y = vars[2], label = 'Label',
                                      colour = 'Score'))
  }
  if(missing(xlab)) {
    xlab <- vars[1]
  }
  if(missing(ylab)) {
    ylab <- vars[2]
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                    caption = caption)
  ## add equal scaling
  plt <- plt + coord_fixed(ratio = 1)
  ## do we want a legend
  plt <- plt + theme(legend.position = legend.position)
  plt
}




##' @title ggplot-based plot for objects of class \code{'rda'}
##'
##' @description
##' Produces a multi-layer ggplot object representing the output of objects produced by \code{\link[vegan]{rda}}.
##'
##' @details
##' TODO
##'
##' @param object an object of class \code{"rda"}, the result of a call to \code{\link[vegan]{rda}}
##' @param axes numeric; which axes to plot, given as a vector of length 2.
##' @param geom character; which geoms to use for the layers. Can be a vector of
##'   up to length 2, in which case, the first element of \code{geom} will be
##'   used for any site scores (both weighted sum or linear combination scores),
##'   and the second element will be used for species scores. The latter will be
##'   ignored if \code{arrows = TRUE}.
##' @param layers character; which scores to plot as layers
##' @param arrows logical; represent species (variables) using vectors?
##' @param legend.position character or two-element numeric vector; where to position the legend. See \code{\link[ggplot2]{theme}} for details. Use \code{"none"} to not draw the legend.
##' @param xlab character; label for the x-axis
##' @param ylab character; label for the y-axis
##' @param title character; subtitle for the plot
##' @param subtitle character; subtitle for the plot
##' @param caption character; caption for the plot
##' @param const General scaling constant to \code{rda} scores. See
##'   \code{\link[vegan]{scores.rda}} for details.
##' @param ... Additional arguments passed to \code{\link{fortify}}.
##'
##' @return Returns a ggplot object.
##'
##' @author Gavin L. Simpson
##'
##' @examples
##' require(vegan)
##' data(dune)
##'
##' pca <- ordi_rda(dune)
##' ordi_plot(pca)
##'
##' ## Just the species scores
##' ordi_plot(pca, layers = "species")
##' @method ordi_plot rda
##' @export
##'
`ordi_plot.rda` <- function(object, axes = c(1,2), geom = c("point", "text"),
                            layers = c("species", "sites", "biplot", "centroids"),
                            arrows = FALSE, legend.position = "none",
                            title = NULL, subtitle = NULL, caption = NULL,
                            ylab, xlab, const, ...) {
  ## determine which layers to plot
  valid <- valid_layers(object)       # vector of valid layers
  ok_layers <- check_user_layers(layers, valid, message = TRUE)
  layers <- layers[ok_layers]         # subset user-supplied layers
  draw_list <- layer_draw_list(valid, layers) # what are we drawing
  
  ## fix-up axes needed to plot
  laxes <- length(axes)
  if (laxes != 2L) {
    if (laxes > 2L) {
      axes <- rep(axes, length.out = 2L)  # shrink to required length
    } else {
      stop("Need 2 ordination axes to plot; only 1 was given.",
           call. = FALSE)
    }
  }
  
  obj <- fortify(object, axes = axes, const = const, ...) # grab some scores
  available <- levels(obj[["Score"]])
  draw_list <- layer_draw_list(valid, layers, available) # what are we drawing
  layer_names <- names(draw_list)[draw_list]
  
  ## sort out x, y aesthetics
  vars <- getDimensionNames(obj)
  
  ## process geom arg
  geom <- match.arg(geom, several.ok = TRUE)
  geom <- unique(geom)    # simplify geom if elements are the same
  
  ## subset out the layers wanted
  obj <- obj[obj[["Score"]] %in% layer_names, , drop = FALSE]
  
  ## skeleton layer
  plt <- ggplot()
  
  ## draw sites, species, constraints == lc site scores
  if (any(draw_list[c("species","sites","constraints")])) {
    plt <- add_spp_site_scores(obj, plt, vars, geom, draw_list, arrows)
  }
  
  ## remove biplot arrows for centroids if present
  if(all(draw_list[c("biplot","centroids")])) {
    want <- obj[["Score"]] == "biplot"
    tmp <- obj[want, ]
    obj <- obj[!want, ]
    bnam <- tmp[, "Label"]
    cnam <- obj[obj[["Score"]] == "centroids", "Label"]
    obj <- rbind(obj, tmp[!bnam %in% cnam, , drop = FALSE])
  }
  
  if(isTRUE(draw_list["biplot"])) {
    want <- obj[["Score"]] == "biplot"
    if (length(layer_names) > 1) {
      mul <- arrowMul(obj[want, vars, drop = FALSE],
                      obj[!want, vars, drop = FALSE])
      obj[want, vars] <- mul * obj[want, vars]
    }
    col <- "navy"
    plt <- plt +
      geom_segment(data = obj[want, , drop = FALSE ],
                   aes_string(x = 0, y = 0,
                              xend = vars[1], yend = vars[2]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   colour = col)
    obj[want, vars] <- 1.1 * obj[want, vars]
    plt <- plt + geom_text(data = obj[want, , drop = FALSE ],
                           aes_string(x = vars[1], y = vars[2],
                                      label = 'Label'))
  }
  
  if(isTRUE(draw_list["centroids"])) {
    want <- obj[["Score"]] == "centroids"
    plt <- plt +
      geom_text(data = obj[want, , drop = FALSE],
                aes_string(x = vars[1], y = vars[2], label = 'Label'),
                colour = "navy")
  }
  
  if(missing(xlab)) {
    xlab <- vars[1]
  }
  if(missing(ylab)) {
    ylab <- vars[2]
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle,
                    caption = caption)
  ## add equal scaling
  plt <- plt + coord_fixed(ratio = 1)
  ## do we want a legend
  plt <- plt + theme(legend.position = legend.position)
  plt
}




##' @title Scale Vectors to Data
##' @description Scale vector arrows to \code{fill} proportion of the data.
##' @param arrows a two-column matrix-like object containing coordinates for the arrows/vectors on x and y axes.
##' @param data a two-column matrix-like object containing coordinates of the data on the x and y axes.
##' @param at numeric vector of length 2; location of the origin of the arrows.
##' @param fill numeric; what proportion of the range of the data to fill
##' @return a numeric multiplier that will scale the arrows
##' @author Gavin L. Simpson
`arrowMul` <- function(arrows, data, at = c(0, 0), fill = 0.75) {
  u <- c(range(data[,1], range(data[,2])))
  u <- u - rep(at, each = 2)
  r <- c(range(arrows[, 1], na.rm = TRUE), range(arrows[, 2], na.rm = TRUE))
  rev <- sign(diff(u))[-2]
  if (rev[1] < 0)
    u[1:2] <- u[2:1]
  if (rev[2] < 0)
    u[3:4] <- u[4:3]
  u <- u/r
  u <- u[is.finite(u) & u > 0]
  fill * min(u)
}

##' @title Number of scores
##' @description Returns the number of scores returns in object \code{x}.
##'
##' @param x The object whose number of scores is required.
##'
##' @return a numeric vector of length 1 with the number of scores.
##'
##' @author Gavin L. Simpson
`scoresLength` <- function(x) {
  obs <- NROW(x)
  if (is.null(obs))
    obs <- 0
  obs
  
}

##' @title Extract the names of the dimensions to plot as a character vector
##'
##' @description Find the character vector of names for the two dimensions of data to be plotted.
##' @param object a fortified ordination object.
##' @return A length 2 character vector of dimension names.
##' @author Gavin L. Simpson
`getDimensionNames` <- function(object) {
  names(object)[-c(1,2)]
}

##' @title Adds a label layer using one of a set of common geoms
##'
##' @description Adds labels to a plot using one of \code{geom_label}, \code{geom_text}, \code{geom_label_repel} or \code{geom_text_repel}.
##'
##' @param data data frame; data set to use for the label layer. Must contain a variable \code{Label} containing the strings to use as labels.
##' @param geom character; which geom to use for labelling.
##' @param vars character; vector of names of variables to ass to the \code{x} and \code{y} aesthetics of the chosen geom.
##'
##' @author Gavin L. Simpson
##'
`label_fun` <- function(data,
                        geom = c("label", "text", "label_repel", "text_repel"),
                        vars) {
  ll <- switch(geom,
               label =
                 geom_label(data = data,
                            mapping = aes_string(x = vars[1],
                                                 y = vars[2],
                                                 label = 'Label')),
               text =
                 geom_text(data = data,
                           mapping = aes_string(x = vars[1],
                                                y = vars[2],
                                                label = 'Label')),
               label_repel =
                 geom_label_repel(data = data,
                                  mapping = aes_string(x = vars[1],
                                                       y = vars[2],
                                                       label = 'Label')),
               text_repel =
                 geom_text_repel(data = data,
                                 mapping = aes_string(x = vars[1],
                                                      y = vars[2],
                                                      label = 'Label'))
  )
  ll
}

##' @title Valid layers for vegan objects
##'
##' @param object An R object.
##' @param ... Additional arguments passed to methods.
##'
##' @rdname valid_layers
##' @export
`valid_layers` <- function(object, ...) {
  UseMethod('valid_layers')
}

##' @rdname valid_layers
##' @export
`valid_layers.rda` <- function(object, ...) {
  c("species", "sites", "constraints", "biplot", "centroids", "regression")
}
##' @rdname valid_layers
##' @export
`valid_layers.cca` <- function(object, ...) {
  c("species", "sites", "constraints", "biplot", "centroids", "regression")
}

##' @title ordination plots
##'
##' @param ... Additional arguments
##'
#' @rdname ordi_plot
#' @export
`ordi_plot` <- function(...){
  UseMethod('ordi_plot')
}

# ##' @rdname ordi_plot
# ##' @export
# `ordi_plot.rda` <- function(...){
#     UseMethod('ordi_plot')
# }
#
# ##' @rdname ordi_plot
# ##' @export
# `ordi_plot.cca` <- function(...){
#     UseMethod('ordi_plot')
# }

##' @title Check user-supplied layers against list of valid layers
##'
##' @param user character; vector of user supplied layer names.
##' @param valid character; vector of valid layer names.
##' @param message logical; should a message be raised in the case of invalid
##'   user-supplied layer names.
`check_user_layers` <- function(user, valid, message = FALSE) {
  ok <- user %in% valid
  
  if (isTRUE(message) && any(!ok)) {
    msg <- "Invalid (ignored) layers for this object:"
    invalid <- paste(user[!ok], collapse = ', ')
    message(paste(msg, invalid, sep = " "))
  }
  
  ok
}

##' @title List of layers to draw for a given vegan object
##'
##' @param valid character; vector of valid layer names
##' @param layers character; a vector of layer names for \code{object} that has
##'   already been filtered for validity.
##' @param available charecter; what layers are actually available
##'
##' @importFrom stats setNames
`layer_draw_list` <- function(valid, layers = NULL, available = NULL) {
  l <- setNames(rep(TRUE, length(valid)), valid)
  if (!is.null(layers)) {
    if (!is.null(available)) {
      layers <- layers[layers %in% available]
    }
    i <- valid %in% layers
    l[!i] <- FALSE
  }
  
  l
}

##' @title Adds species and site score layers to an existing plot
##'
##' @param object an ordination object.
##' @param plt a ggplot object.
##' @param vars character; length 2 vector of dimension names.
##' @param geom character; vector of length 1 or 2 indicating which geoms will
##'   be used ofr the species or site scores.
##' @param draw_list logical; vector of types of scores indicating which are
##'   available and requested for plotting.
##' @param arrows logical; length 1 vector indicating if species scores should
##'   be drawn using arrows.
##'
`add_spp_site_scores` <- function(object, plt, vars, geom, draw_list, arrows) {
  wanted <- names(draw_list[c("species","sites","constraints")])
  ## if we're plotting species by arrows, drop species if in list
  if (isTRUE(arrows)) {
    wanted <- wanted[wanted != "species"]
  }
  
  ## if still something to draw, draw it
  if (length(wanted) > 0L) {
    ## case of a single geom
    if (length(geom) == 1L) {
      take <- object[["Score"]] %in% wanted
      if (geom == "point") {
        plt <- plt +
          geom_point(data = object[take, , drop = FALSE],
                     aes_string(x = vars[1], y = vars[2],
                                shape = 'Score', colour = 'Score'))
      } else {
        plt <- plt +
          geom_text(data = object[take, , drop = FALSE ],
                    aes_string(x = vars[1], y = vars[2],
                               label = 'Label', colour = 'Score'),
                    size = 3)
      }
    } else {
      ## we have to plot species and sites/constraints separately
      if ("species" %in% wanted) {
        take <- object[["Score"]] == "species"
        if (geom[2L] == "point") {
          plt <- plt +
            geom_point(data = object[take, , drop = FALSE],
                       aes_string(x = vars[1], y = vars[2],
                                  shape = 'Score',
                                  colour = 'Score'))
          
        } else {
          plt <- plt +
            geom_text(data = object[take, , drop = FALSE ],
                      aes_string(x = vars[1],
                                 y = vars[2],
                                 label = 'Label',
                                 colour = 'Score'),
                      size = 3)
          
        }
      }
      if (any(c("sites","constraints") %in% wanted)) {
        take <- object[["Score"]] %in% c("sites","constraints")
        if (geom[1L] == "point") {
          plt <- plt +
            geom_point(data = object[take, , drop = FALSE],
                       aes_string(x = vars[1], y = vars[2],
                                  shape = 'Score',
                                  colour = 'Score'))
          
        } else {
          plt <- plt +
            geom_text(data = object[take, , drop = FALSE ],
                      aes_string(x = vars[1],
                                 y = vars[2],
                                 label = 'Label',
                                 colour = 'Score'),
                      size = 3)
        }
      }
    }
  }
  
  ## now check if species should be added as arrows
  if (isTRUE(arrows) && draw_list["species"]) {
    take <- object[["Score"]] == "species"
    pdat <- object[take, , drop = FALSE]
    col <- "black"
    plt <- plt +
      geom_segment(data = pdat,
                   aes_string(x = 0, y = 0,
                              xend = vars[1], yend = vars[2]),
                   arrow = arrow(length = unit(0.2, "cm")),
                   colour = col)
    pdat[, vars] <- 1.1 * pdat[, vars, drop = FALSE]
    plt <- plt + geom_text(data = pdat,
                           aes_string(x = vars[1], y = vars[2],
                                      label = 'Label'), size = 4)
  }
  
  ## return
  plt
}

##' @title Fortify a \code{"cca"} object.
##'
##' @description
##' Fortifies an object of class \code{"cca"} to produce a
##' data frame of the selected axis scores in long format, suitable for
##' plotting with \code{\link[ggplot2]{ggplot}}.
##'
##' @details
##' TODO
##'
##' @param model an object of class \code{"cca"}, the result of a call to
##' \code{\link[vegan]{cca}}, \code{\link[vegan]{rda}}, or
##' \code{\link[vegan]{capscale}}.
##' @param data currently ignored.
##' @param axes numeric; which axes to extract scores for.
##' @param display numeric; the scores to extract in the fortified object.
##' @param ... additional arguments passed to \code{\link[vegan]{scores.cca}},
##'   and \code{\link[vegan]{scores.rda}}.
##' @return A data frame in long format containing the ordination scores.
##' The first two components are the axis scores.
##' @author Gavin L. Simpson
##'
##' @method fortify cca
##' @export
##'
##' @examples
##' require(vegan)
##' data(dune)
##' data(dune.env)
##'
##' sol <- cca(dune ~ A1 + Management, data = dune.env)
##' head(fortify(sol))
`fortify.cca` <- function(model, data, axes = 1:6,
                          display = c("sp", "wa", "lc", "bp", "cn"), ...) {
  ## extract scores
  scrs <- scores(model, choices = axes, display = display, ...)
  ## handle case of only 1 set of scores
  if (length(display) == 1L) {
    scrs <- list(scrs)
    nam <- switch(display,
                  sp = "species",
                  species = "species",
                  wa = "sites",
                  sites = "sites",
                  lc = "constraints",
                  bp = "biplot",
                  cn = "centroids",
                  stop("Unknown value for 'display'"))
    names(scrs) <- nam
  }
  miss <- vapply(scrs, function(x ) all(is.na(x)), logical(1L))
  scrs <- scrs[!miss]
  nams <- names(scrs)
  nr <- vapply(scrs, FUN = NROW, FUN.VALUE = integer(1))
  df <- do.call('rbind', scrs)
  rownames(df) <- NULL
  df <- as.data.frame(df)
  df <- cbind(Score = factor(rep(nams, times = nr)),
              Label = unlist(lapply(scrs, rownames), use.names = FALSE),
              df)
  df
}

##' @title Fortify a \code{"metaMDS"} object.
##'
##' @description
##' Fortifies an object of class \code{"metaMDS"} to produce a
##' data frame of the selected axis scores in long format, suitable for
##' plotting with \code{\link[ggplot2]{ggplot}}.
##'
##' @details
##' TODO
##'
##' @param  model an object of class \code{"metaMDS"}, the result of a call
##' to \code{\link[vegan]{metaMDS}}.
##' @param data currently ignored.
##' @param ... additional arguments passed to
##' \code{\link[vegan]{scores.metaMDS}}. Note you can't use \code{display}.
##' @return A data frame in long format containing the ordination scores.
##' The first two components are the axis scores.
##' @author Gavin L. Simpson
##'
##' @method fortify metaMDS
##' @export
##'
##' @importFrom ggplot2 fortify
##' @importFrom vegan scores
##'
##' @examples
##' ## load example data
##' require(vegan)
##' data(dune)
##'
##' ord <- metaMDS(dune)
##' head(fortify(ord))
`fortify.metaMDS` <- function(model, display="sites") {
  samp <- scores(model, display = "sites")
  # spp <- tryCatch(scores(model, display = display),
  #                 error = function(c) {NULL})
  spp <- scores(model, display="species")
  if("sites" %in% display && "species" %in% display){
    #print("sites and species")
    df <- rbind(samp, spp)
    df <- as.data.frame(df)
    df <- cbind(Score = factor(rep(c("sites","species"),
                                   c(nrow(samp), nrow(spp)))),
                Label = c(rownames(samp), rownames(spp)),
                df)
  } else if ("sites" %in% display){
    #print("sites only")
    df <- as.data.frame(samp)
    df <- data.frame(Score = factor(rep("sites", nrow(df))),
                     Label = rownames(samp),
                     samp)
  } else {
    #print("spp only")
    df <- as.data.frame(spp)
    df <- data.frame(Score = factor(rep("species", nrow(df))),
                     Label = rownames(spp),
                     spp)
  }
  # if (!is.null(spp)) {
  #   df <- rbind(samp, spp)
  #   df <- as.data.frame(df)
  #   df <- cbind(Score = factor(rep(c("sites","species"),
  #                                  c(nrow(samp), nrow(spp)))),
  #               Label = c(rownames(samp), rownames(spp)),
  #               df)
  # } else {
  #   df <- data.frame(Score = factor(rep("sites", nrow(df))),
  #                    Label = rownames(samp),
  #                    samp)
  # }
  rownames(df) <- NULL
  df
}


# Function for convex hull
StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)
geom_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

