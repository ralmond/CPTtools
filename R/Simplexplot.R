## simplexplot
## This is borrowed from the archetypes project, but stripping out the archetypes overhead as we already have simplexes.

simplex_vertex_projection <- function( nvert, r=1) {
  phi <- seq(-pi,pi,length.out=nvert+1)[-1]
  cbind(x=r*cos(phi), y=r*sin(phi))
}

plotsimplex <-
function (data, radius = 1, order = NULL, labels_cex = 1, 
          labels = colnames(data), show_labels = TRUE, points_col = "#00000044", 
          points_pch = 19, points_cex = 1, label_offset=.9,
          projection_vertexes = simplex_vertex_projection(ncol(data),radius), 
          show_points = TRUE, show_circle = TRUE, circle_col = "lightgray", 
          show_edges = TRUE, edges_col = "lightgray", ...) 
{
   k <- ncol(data)
   n <- nrow(data)
  if (is.null(order)) 
    order <- 1:k
  if (is.null(labels)) 
    labels <- sprintf("A%s", order)
  if (length(points_col) == 1) 
    points_col <- rep(points_col, n)
  if (length(points_cex) == 1) 
    points_cex <- rep(points_cex, n)
  coefs <- data[, order]
  proj_z <- projection_vertexes
  proj_h <- coefs %*% proj_z
  proj_labels <- proj_z
  t <- cbind(x = acos(proj_z[, "x"]*label_offset), 
             y = asin(proj_z[, "y"]*label_offset))
  proj_labels <- cbind(x = radius * cos(t[, "x"]), y = radius * 
                         sin(t[, "y"]))
  proj_circle <- list(center = cbind(x = 0, y = 0), radius = radius - 
                        1)
  proj_edges <- proj_z[as.integer(combn(1:k, 2)), ]

  plot(proj_z, type = "n", asp = TRUE, xlim = c(-radius, radius), 
       ylim = c(-radius, radius), axes = FALSE, xlab = "", ylab = "")
  if (show_circle) {
    symbols(proj_circle$center, circles = radius - 1, inches = FALSE, 
            add = TRUE, asp = TRUE, fg = circle_col)
  }
  if (show_edges) {
    lines(proj_edges, col = edges_col)
  }
  if (show_labels) {
    text(proj_labels, labels = labels, cex = labels_cex)
  }
  if (show_points) {
    points(proj_h, col = points_col, pch = points_pch, cex = points_cex)
  }
  ret <- list(proj_z = proj_z, proj_h = proj_h, proj_labels = proj_labels, 
              proj_circle = proj_circle, 
              proj_edges = proj_edges)
  class(ret) <- "simplexplot"
  invisible(ret)
}