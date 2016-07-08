# marketview 0.1.2.9001

## What's New
* Added a `NEWS.md` file to track changes to the package.

## Bug Fixes
* `bounding_box()` works now, and added the option not to draw the border.
* `save_plot()` works, had a misnamed variable.
* `save_table()` coerces to data.frame first, so it'll work in a dplyr chain that returns a `tibble`/`tbl`.
* removed the whitespace at the bottom of the plot in `theme_mvl()`
