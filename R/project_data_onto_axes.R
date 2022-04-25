#'
#' Project Data Points Onto Axes
#'
#' @param data List.  Output of `generate_2D_ellipse`.
#' @param axes Named numeric vector.  Output of `define_new_axis`.
#' @param plot Logical. If `TRUE` an *existing plot* is annotated to help understand
#'             the logic of the function.  The plot annnotations only apply to axis 1.
#' @return A list of two data frames, containing the x,y projections onto each axis.
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'
.project_data_onto_axes <- function(data = NULL, axes = NULL, plot = FALSE) {
  x <- data$x
  y <- data$y
  xc <- data$xc
  yc <- data$yc

  raw <- matrix(c(x, y, rep(1, length(x))), nrow = 3, byrow = TRUE)

  # trap for angles that will be problematic
  npnp <- .check_npnp(axes)

  if (npnp["npnp"]) {
    # get one point on each axis to express the axis as a vector passing through 0,0
    # use the y-intercept, then the corresponding x is 0.0 by definition
    pt_ax_1 <- matrix(c(0.0, axes[2], 1), nrow = 3)
    pt_ax_2 <- matrix(c(0.0, axes[4], 1), nrow = 3)

    # translate the point found to the origin
    pt_ax_1t <- .trans_mat_2D(0.0, -axes[2]) %*% pt_ax_1
    pt_ax_2t <- .trans_mat_2D(0.0, -axes[4]) %*% pt_ax_2

    if (plot) {
      points(pt_ax_1[1, ], pt_ax_1[2, ], col = "blue", pch = 8)
      text(pt_ax_1[1, ], pt_ax_1[2, ], "pt_ax_1", pos = 4, cex = 0.75, col = "blue")
      points(pt_ax_1t[1, ], pt_ax_1t[2, ], col = "blue", pch = 8)
      text(pt_ax_1t[1, ], pt_ax_1t[2, ], "pt_ax_1t", pos = 4, cex = 0.75, col = "blue")
      abline(coef = c(0.0, axes[1]), lty = 2)
    }

    # translate raw data cloud to new axis passing through 0,0
    dy1 <- pt_ax_1[1, 1] - axes[2]
    proj_1a <- .trans_mat_2D(0.0, dy1) %*% raw
    if (plot) points(proj_1a[1, ], proj_1a[2, ])
    dy2 <- pt_ax_2[1, 1] - axes[4]
    proj_2a <- .trans_mat_2D(0.0, dy2) %*% raw

    # describe translated axis as a vector passing through 0,0
    # at pt_ax_1t, b = 0, slope is axes[1]; choose x = 1, hence y = m*1 + 0, y = m
    axis_1t <- matrix(c(1.0, axes[1]), nrow = 2)
    axis_2t <- matrix(c(1.0, axes[3]), nrow = 2)

    # compute the projections
    proj_1b <- .project_data_onto_line(axis_1t, proj_1a[1:2, ])
    proj_2b <- .project_data_onto_line(axis_2t, proj_2a[1:2, ])

    if (plot) {
      points(proj_1b[1, ], proj_1b[2, ], col = "orange")
      segments(proj_1a[1, ], proj_1a[2, ], proj_1b[1, ], proj_1b[2, ], col = "orange")
    }

    # augment proj_1b, then translate back to original position
    proj_1b <- rbind(proj_1b, rep(1.0, ncol(proj_1b)))
    proj_1c <- .trans_mat_2D(0.0, -dy1) %*% proj_1b
    proj_2b <- rbind(proj_2b, rep(1.0, ncol(proj_2b)))
    proj_2c <- .trans_mat_2D(0.0, -dy2) %*% proj_2b

    if (plot) {
      points(proj_1c[1, ], proj_1c[2, ], col = "orange")
      segments(raw[1, ], raw[2, ], proj_1c[1, ], proj_1c[2, ], col = "orange")
    }
  } # end of npnp (not perpendicular nor parallel)

  if (!npnp["npnp"]) { # special cases; construct projection manually
    x_fixed <- rep(xc, length(x))
    y_fixed <- rep(yc, length(y))
    if (npnp["perp"]) {
      proj_1c <- matrix(c(x_fixed, raw[2, ]), nrow = 2, byrow = TRUE)
      proj_2c <- matrix(c(raw[1, ], y_fixed), nrow = 2, byrow = TRUE)
      if (plot) points(proj_1c[1, ], proj_1c[2, ], col = "orange")
    }
    if (npnp["para"]) {
      proj_1c <- matrix(c(raw[1, ], y_fixed), nrow = 2, byrow = TRUE)
      proj_2c <- matrix(c(x_fixed, raw[2, ]), nrow = 2, byrow = TRUE)
      if (plot) points(proj_1c[1, ], proj_1c[2, ], col = "orange")
    }
  }

  return(list(
    "axis_1_projection" = proj_1c[1:2, ],
    "axis_2_projection" = proj_2c[1:2, ]
  ))
} # end of project_data_onto_axes
