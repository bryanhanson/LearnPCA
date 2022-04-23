#'
#' Check if the Test Axes are Rotated to a Problematic Angle
#'
#' Check if the test axes are rotated to an angle where the slopes or intercepts
#' are undefined.  If this is the case, separate handling is needed when projecting
#' the data points onto the text axes.
#'
#' @param axes Named numeric vector.  Output of `define_new_axis`.
#' @return Named logical vector. First element is `TRUE` if we are at any problematic
#'         angle.  Second element is `TRUE` if the test axis is parallel to the x,y
#'         axis system.  Third element is `TRUE` if the test axis is perpendicular
#'         to the x,y axis system.
#'
#' @author David T. Harvey, Bryan A. Hanson
#' @noRd
#'
.check_npnp <- function(axes = NULL) {
  # npnp = not perpendicular nor parallel
  perp <- FALSE # 90 or -90
  para <- FALSE # 0, -0, 180, -180
  npnp <- FALSE # not perpendicular nor parallel to x,y axis means no special handling needed
  if (axes[1] == Inf) perp <- TRUE
  if (axes[3] == Inf) para <- TRUE
  if (!perp & !para) npnp <- TRUE
  ans <- c(npnp, para, perp)
  names(ans) <- c("npnp", "para", "perp")
  return(ans)
}
