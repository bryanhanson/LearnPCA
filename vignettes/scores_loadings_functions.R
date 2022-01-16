# function to generate data for scores and loading vignette

generate_data = function(x_center = 0, y_center = 0, angle = 30, 
                         x_range = 10, y_range = 2, size = 10, 
                         seed = 13, show_plot = FALSE){

# set coordinates for center of ellipse
x0 = x_center
y0 = y_center

# set dimensions of ellipsoid relative to center
xa = x_range
yb = y_range

# generate random points within the ellipsoid's boundaries by first
# generating random points within rectangle that encompasses  ellipse
set.seed(seed)
x = runif(size, min = -xa, max = xa)
y = runif(size, min = -yb, max = yb)

# determine which points have (x,y) values inside the ellipse
# using equation for ellipse; negative check means point
# is inside of ellipse; flag these as id
check = (x - x0)^2/xa^2 + (y - y0)^2/yb^2 - 1
id = which(check < 0)

# extract sets of (x,y) points inside of ellipse
xe = x[id]
ye = y[id]

# rotate points so not aligned on x-axis
# change angle from degrees to radians
angle_rad = angle * pi/180
xrot = xe * cos(angle_rad) - ye * sin(angle_rad)
yrot = xe * sin(angle_rad) + ye * cos(angle_rad)

# option to plot original data to evaluate usefulness
if(show_plot == TRUE){
plot(xrot, yrot, pch = 19, col = "black", cex = 0.75, asp = 1)
}

# define output of function
out = list(
  "x_center" = x_center,
  "y_center" = y_center,
  "x_range" = x_range,
  "y_range" = y_range,
  "size" = size,
  "seed" = seed,
  "angle" = angle,
  "xrot" = xrot,
  "yrot" = yrot,
  "xorg" = xe,
  "yorg" = ye
)
}

# function to rotate axes for use in scores and loadings vignette
# file is object generated using generate_data

rot_axes = function(angle = 15, file){

  # limit angle to 0° to 180°
  # if (angle < 0 | angle > 180){
  #   stop("Angle must be in the range of 0° to 180°.")
  # }
  # 
  # pull (x,y) data from file
  x = file$xrot
  y = file$yrot
  
  # angles of 0°, 90°, 180° will throw errors when projecting
  # data onto axes, so handled separately here
  
  if(angle == 0 | angle == 180){
    axis1_newx = rep(0,length(x))
    axis1_newy = y
    axis2_newx = x
    axis2_newy = rep(0,length(y))
    
    out = list(
      "angle" = angle,
      "x_original" = x,
      "y_original" = y,
      "x_range" = file$x_range,
      "y_range" = file$y_range,
      "axis1_x" = axis1_newx,
      "axis1_y" = axis1_newy,
      "axis2_x" = axis2_newx,
      "axis2_y" = axis2_newy
    )
  } else if(angle == 90){
      axis1_newx = x
      axis1_newy = rep(0,length(y))
      axis2_newx = rep(0,length(x))
      axis2_newy = y
      
      out = list(
        "angle" = angle,
        "x_original" = x,
        "y_original" = y,
        "x_range" = file$x_range,
        "y_range" = file$y_range,
        "axis1_x" = axis1_newx,
        "axis1_y" = axis1_newy,
        "axis2_x" = axis2_newx,
        "axis2_y" = axis2_newy
      )
      
  } else {
  
  # calculations for projecting points onto axes after rotation 
  # slope for first axis (y) given by tangent of angle
  # slope for second axis (x) is -1/slope of first axis
  axis1_intercept = 0
  axis1_slope = tan((90 - angle) * pi/180)
  axis2_intercept = 0
  axis2_slope = -1/axis1_slope
  
  # project (x,y) points onto first and second rotated axes 
  # slope of projection is -1/slope of axis
  # intercept of projection is y - slope of projection * x
  # solve simultaneous equations for lines through shared (x,y)
  # equation 1: y = -(x/slope) + int of point
  # equation 2: y = int of line + slope*x
  # which gives
  # x_projected = (int of point - int of line)/(slope + 1/slope)
  # y projected = int of line + slope*x_projected
  axis1_new_intercept = y + x/axis1_slope
  axis1_newx = (axis1_new_intercept - axis1_intercept)/(axis1_slope + 1/axis1_slope)
  axis1_newy = axis1_intercept + axis1_slope * axis1_newx
  axis2_new_intercept = y + x/axis2_slope
  axis2_newx = (axis2_new_intercept - axis2_intercept)/(axis2_slope + 1/axis2_slope)
  axis2_newy = axis2_intercept + axis2_slope * axis2_newx
  
  # define output of function
  out = list(
    "angle" = angle,
    "x_original" = x,
    "y_original" = y,
    "x_range" = file$x_range,
    "y_range" = file$y_range,
    "axis1_x" = axis1_newx,
    "axis1_y" = axis1_newy,
    "axis2_x" = axis2_newx,
    "axis2_y" = axis2_newy,
    "axis1_intercept" = axis1_intercept,
    "axis1_slope" = axis1_slope,
    "axis2_intercept" = axis2_intercept,
    "axis2_slope" = axis2_slope
  )
  }
}

# function to plot scores and loadings data
# file is object created using rot_axes

plot_rot_axes = function(file, show_rotated = TRUE,
                         show_projections = TRUE,
                         show_simple_legend = TRUE,
                         show_full_legend = TRUE,
                         show_full_axes_legend = FALSE,
                         show_title = TRUE,
                         show_scores = TRUE, 
                         show_loadings = TRUE){
  
  # set colors
  axis2_col = "#3db7ed"
  axis2_colname = "light blue"
  axis1_col = "#f748a5"
  axis1_colname = "pink"
  loadings_col = "#359b73"
  loadings_colname = "green"
  origdata_col = "#000000"
  origdata_colname = "black"
  
  # determine maximum value for axis limits
  axis_max = max(c(file$x_range, file$y_range))
  
  # plot original data
  plot(x = file$x_original, y = file$y_original, type = "p", asp = 1,
       xlim = c(-axis_max, axis_max), ylim = c(-axis_max, axis_max),
       xlab = "variable 2", ylab = "variable 1", pch = 19, col = origdata_col, 
       cex = 1)
  
  # add original axes as dashed lines
  abline(v = 0, lwd = 1, lty = 2, col = axis1_col)
  abline(h = 0, lwd = 1, lty = 2, col = axis2_col)

  # add rotated axes
  if(show_rotated == TRUE){
  if(file$angle == 0 | file$angle == 180){
    abline(h = 0, lwd = 2, lty = 1, col = axis2_col)
    abline(v = 0, lwd = 2, lty = 1, col = axis1_col)
  } else if(file$angle == 90){
    abline(h = 0, lwd = 2, lty = 1, col = axis1_col)
    abline(v = 0, lwd = 2, lty = 1, col = axis2_col)
  } else {
  abline(a = file$axis1_intercept, b = file$axis1_slope, 
         lwd = 1, col = axis1_col)
  abline(a = file$axis2_intercept, b = file$axis2_slope, 
         lwd = 1, col = axis2_col)
  }
  }
  
  # add projections of points onto rotated axes
  if (show_projections == TRUE){
  points(x = file$axis1_x, y = file$axis1_y, pch = 19, col = axis1_col, cex = 0.75)
  points(x = file$axis2_x, y = file$axis2_y, pch = 19, col = axis2_col, cex = 0.75)
  }
  
  # calculate variances
  var_total = var(file$x_original) + var(file$y_original)
  var_axis1 = var(file$axis1_x) + var(file$axis1_y)
  var_axis2 = var(file$axis2_x) + var(file$axis2_y)
  percent_axis1 = round(100 * var_axis1/var_total,1)
  percent_axis2 = round(100 * var_axis2/var_total,1)
  
  # add legend with just axis names
  if(show_simple_legend == TRUE & show_full_legend == FALSE){
  legend(x = "topleft", legend = c("axis 1", "axis 2"), 
         col = c(axis1_col, axis2_col), lwd = 2, lty = 2, bty = "o", 
         box.col = "white", bg = "white", box.lwd = 0.1) 
  }
  
  # add legend with information on variances for axes
  if(show_full_axes_legend == TRUE){
    axis1_text = paste0("axis A: ", round(var_axis1,1), " (", percent_axis1,"%)")
    axis2_text = paste0("axis B: ", round(var_axis2,1), " (", percent_axis2,"%)")
    legend(x = "topleft", legend = c("variances",axis1_text,axis2_text), 
           bty = "o", box.col = "white", bg = "white", box.lwd = 0.1,
           text.col = c("black",axis1_col, axis2_col))
  }
  
  # add legend with information on variances for principal components
  if(show_full_legend == TRUE){
  axis1_text = paste0("PC 1: ", round(var_axis1,1), " (", percent_axis1,"%)")
  axis2_text = paste0("PC 2: ", round(var_axis2,1), " (", percent_axis2,"%)")
  legend(x = "topleft", legend = c("variances",axis1_text,axis2_text), 
        bty = "o", box.col = "white", bg = "white", box.lwd = 0.1,
         text.col = c("black",axis1_col, axis2_col))
  }
  
  # add main title with angle
  if(show_title == TRUE){
  title(main = paste0("axes rotated by ", file$angle, "°"))
  }
  
  # show scores for one extreme point
  # if(show_scores == TRUE){
  # min_id = which.min(file$y_original)
  # lines(x = c(file$x_original[min_id], file$axis1_x[min_id]),
        # y = c(file$y_original[min_id], file$axis1_y[min_id]),
        # lty = 3, lwd = 2, col = axis1_col)
  # lines(x = c(file$x_original[min_id], file$axis2_x[min_id]),
        # y = c(file$y_original[min_id], file$axis2_y[min_id]),
        # lty = 3,lwd = 2, col = axis2_col)
  # }
  
  # show scores for one extreme point BAH grey lines version
  if(show_scores == TRUE){
  min_id = which.min(file$y_original)
  lines(x = c(file$x_original[min_id], file$axis1_x[min_id]),
        y = c(file$y_original[min_id], file$axis1_y[min_id]),
        lty = 1, lwd = 2, col = "gray80")
  lines(x = c(file$x_original[min_id], file$axis2_x[min_id]),
        y = c(file$y_original[min_id], file$axis2_y[min_id]),
        lty = 1,lwd = 2, col = "gray80")
  }

  # show loadings for first axis
  if(show_loadings == TRUE){
  library(plotrix)
  draw.arc(x = 0, y = 0, radius = axis_max/1, deg1 = 90, deg2 = 90 - file$angle, 
           lwd = 2, col = axis1_col, lty = 1)
  points(x = axis_max, y = 0.1, pch = -9660, col = axis1_col)
  arctext(x = "\U0398", center = c(0,0), cex = 1.25, col = axis1_col,
          radius=axis_max/0.9, middle = (180 - file$angle)/2 * (pi/180),
          stretch = 1, clockwise = TRUE)
  draw.arc(x = 0, y = 0, radius = axis_max/1, deg1 = 0, deg2 = 90 - file$angle, 
           lwd = 2, col = axis1_col, lty = 1)
  points(x = 0.1, y = axis_max, pch = -9668, col = axis1_col)
  arctext(x = "\U03A6", center = c(0,0), cex = 1.25, col = axis1_col,
          radius=axis_max/0.9, middle = (90 - file$angle)/2 * (pi/180),
          stretch = 1, clockwise = TRUE)
  legend(x = "bottomright", legend = c("loadings",paste0("\U0398: cos(",-file$angle,") = ",round(cos(-file$angle * pi/180),digits = 3)), paste0("\U03A6: cos(",90-file$angle,") = ",round(cos((90-file$angle) * pi/180),digits = 3))), bty = "o", box.col = "white", bg = "white", box.lwd = 0.1, text.col = c("black",axis1_col, axis1_col))
  }
}
