

### Angle to rotate test axes

The test axes are a proposed new axis system (basis) for the first and second principal components.  As you change this value the test axes rotate and the corresponding variance is updated.

The proposed first axis is in light blue, and the proposed second axis is pink. Positive rotations are degrees CCW relative to the positive x-axis.  When the variance for test axis one is maximized, that axis is now principal component axis one, and the other axis is principal component axis two.

### Show all projections on PC1

Toggle showing all projections on test axis one or projections from a single point onto test axes one and two.

---

The following parameters apply to the generation of random data for the intial ellipse.  The default values provide a decent example, and you may wish to leave them alone and focus on the test axes.  Note that data is generated randomly and then trimmed to create the ellipse. As a result the values you provide guide the generation of the data, but one is not guaranteed to have `n` data points, nor does the angle the initial ellipse is rotated correspond to the ideal position for the first principal component.

### Ellipse center x/y value

The center of the ellipse to be generated.

### Ellipse x/y range

The range for each variable.

### Angle to rotate initial ellipse

The angle to rotate the initial ellipse.  Positive values are degrees CCW relative to the positive x-axis.

### Seed

The seed for random data generation.

### No. of data points

The number of data points to use.
