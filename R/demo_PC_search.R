#'
#' Demonstrate the Search for a Principal Component
#'
#'
#' @param x_center Numeric. Center of ellipse along x-axis.
#' @param y_center Numeric. Center of ellipse along y-axis.
#' @param x_range Numeric. The length of the major axis of the ellipse.
#' @param y_range Numeric. The length of the minor axis of the ellipse.
#' @param rot_ellipse Numeric.  Angle in degrees to rotate the initial ellipse. Rotation is
#'        counter-clockwise relative to the positive x-axis.
#' @param n Integer.  Number of data points to generate.
#' @param seed Integer.  The seed to use for random number generation.
#' @param rot_axes Numeric.  The angle to rotate the test axes.
#' @param show_all_PC1 Logical.  If `TRUE` all the projections from the data onto the
#'        first proposed principal axis are shown.  If `FALSE`, projections are shown
#'        from one selected data point onto each proposed axis.
#' @param shiny Logical.  If `TRUE` certain plot annotations are supressed for use in Shiny.
#'
#' @return None. Side effect is a plot.
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'

.demo_PC_search <- function(x_center = 0, y_center = 0, x_range = 10, y_range = 4,
                             rot_ellipse = 30, n = 20, rot_axes = 30, seed = 13,
                             show_all_PC1 = FALSE, shiny = FALSE) {

  set.seed(seed) # set inside here so same data set is used in each plot

  # define colors (pPC1 = proposed PC1)
  pPC1_col <- "#3db7ed" # light blue
  pPC2_col <- "#f748a5" # pink
  proj_col <- "#d55e00" # dark orange

  res <- .generate_2D_ellipse(
    x_center = x_center, y_center = y_center,
    x_range = x_range, y_range = y_range,
    angle = rot_ellipse, n = n,
    plot = FALSE, showOrig = FALSE
  )
  res2 <- .define_new_axes(res, rot_axes, plot = FALSE)
  res3 <- .project_data_onto_axes(res, res2, plot = FALSE)

  # Construct the base plot manually.
  # Stretch limits by 50% to accommodate the projections when the test axes are rotated.
  # The interactions between parameters may lead to the points being predominately
  #   spread along either axis, and then when rot_ellipse is changed they may suddenly
  #   be spread along the other axis.  Depending upon the value of rot_axes the
  #   projections may be off the screen, so 50% is a compromise.
  # However, the appearance of the plot is dominated by asp = 1 so that the physical
  #   distance for a 1 unit change is the same on each axis.
  xl <- range(res$x) + diff(range(res$x)) * c(-1, 1) * 0.5
  yl <- range(res$y) + diff(range(res$y)) * c(-1, 1) * 0.5
  plot(xl, yl,
    type = "n", asp = 1,
    xlab = "x", ylab = "y"
  )
  if (!shiny) title(main = paste("Axes Angle =", rot_axes, sep = " "))

  # draw rotated ellipse
  points(res$x, res$y, pch = 19)

  # draw reference lines
  abline(v = 0.0, h = 0.0, col = "gray90", lty = 2)
  abline(h = res$yc, col = pPC1_col, lty = 2)
  abline(v = res$xc, col = pPC2_col, lty = 2)

  # draw axes of ellipse
  npnp <- .check_npnp(res2)
  if (npnp["npnp"]) {
    abline(coef = c(res2[2], res2[1]), col = pPC1_col)
    abline(coef = c(res2[4], res2[3]), col = pPC2_col)
  }
  if (!npnp["npnp"]) {
   if (rot_axes == 0.0 | rot_axes == -0.0 | rot_axes == 180.0 | rot_axes == -180.0) {
     abline(v = res$xc, col = pPC2_col)
     abline(h = res$yc, col = pPC1_col)
   }
   if (rot_axes == 90.0 | rot_axes == -90.0) {
     abline(v = res$xc, col = pPC1_col)
     abline(h = res$yc, col = pPC2_col)
   }
  }
  # add projected points
    points(res3$axis_1_projection[1, ], res3$axis_1_projection[2, ], pch = 20, col = pPC1_col)
    points(res3$axis_2_projection[1, ], res3$axis_2_projection[2, ], pch = 20, col = pPC2_col)
 
  # either show all projections onto PC1 or just show the one
  if (show_all_PC1) {
    segments(res$x, res$y, res3$axis_1_projection[1,], res3$axis_1_projection[2,], col = "orange")
    # segments(res$x, res$y, res3$axis_2_projection[1,], res3$axis_2_projection[2,], col = "orange")
  }

  if (!show_all_PC1) {
    spc <- res$sp
    segments(res$x[spc], res$y[spc],
      res3$axis_1_projection[1, spc], res3$axis_1_projection[2, spc],
      col = proj_col
    )
    segments(res$x[spc], res$y[spc],
      res3$axis_2_projection[1, spc], res3$axis_2_projection[2, spc],
      col = proj_col
    )
  }

  # compute and display the variance
  var_total <- var(res$xe) + var(res$ye)
  var_axis1 <- var(res3$axis_1_projection[1, ]) + var(res3$axis_1_projection[2, ])
  var_axis2 <- var(res3$axis_2_projection[1, ]) + var(res3$axis_2_projection[2, ])
  percent_axis1 <- round(100 * var_axis1 / var_total, 1)
  percent_axis2 <- round(100 * var_axis2 / var_total, 1)
  leg1 <- paste("var PC1:", as.character(percent_axis1), sep = " ")
  leg2 <- paste("var PC2:", as.character(percent_axis2), sep = " ")
  legend("topleft", legend = c(leg1, leg2), bty = "n", text.col = c(pPC1_col, pPC2_col))
}
