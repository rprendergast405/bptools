# bptools 0.5.9

## What's New
- added mcda_process() for Australian IEO data
- New mvl_a4_template.pptx for A4 PPT reports. It doesn't have very many slide layouts yet.
- create_rmd() will import the templates to the project directory if they don't already exist
- new themes etc
- Added nztm_to_latlon() and latlon_to_nztm()

## Small Changes
- added the 'real-world' sales and quantities to tld_customer_data() output
- updated the banner in the default rmd script
- added bold facet titles back into theme_mvl()
- Updates to the mcd_post_campaign() report defaults

## Bug Fixes
- fixed bug in add_water()

# bptools 0.5.8

## What's New
- scale_reorder(), from David Robinson
- create_rmd() function for making a new markdown script with the default headers etc.

## Changes
- Updates to create_project(): added a /templates directory and updated the default markdown to be a bit fancier
- option in make_cau_df and make_mb_df to aggregate the polygons - should be useful for making catchment outlines
- BUNCH_NAMEX is now treated with mcd_process()
- theme_mvl() update, no longer uses a bold font as the default

## Bug Fixes
- data_import() doesn't crash any more if there is no processing script or data. Just prints a message instead
- tld_customer_data() drops tables on.exit(), so shouldn't litter the DB with tables if it crashes

# bptools 0.5.7

## What's New
- flextable_positives()
- added more labelling functions such as mdollar_change etc.

## Changes
- "mcd" colour palette has been updated as per Nik's feeback
- add_placenames() uses the shadowtext package if it's available. Would recommend installing it
- data_import() has had an overhaul, it's not more verbose and should be more flexible
- added an option to set the tolerance in make_mb_df()
- added more default options to the initialise script from create_project(), which should help with consistency

## Bug Fixes
- updated tld_customer_data() to avoid the strange Oracle bug

# bptools 0.5.6

## What's New
- added "fssi" colour palette to scale_mvl()
- new add_placenames() function for adding city/town/suburb names to a map

## Changes
- New default packages in create_project()
- cleaned up the default directories in create_project()
- updated mcd_post_campaign() with Jeremy's feedback
- improved labels for tld_comparison_plot()
- safer tablenames in tld_customer_data()
- added an option for tld_sales_summary() to get promo_week sales or Monday-Sunday sales

## Bug Fixes
- catchment_picker() was not correctly filling polygons - fixed
- fixed a bug with comparing the same item in multiple time periods in mcd_post_campaign()

# bptools 0.5.5

## What's New
- Several new tld_() functions. There could still be some bugs in these so let me know if anything goes awry

## Changes
- catchment_picker() has had a facelift! And should also run a bit quicker and have a cleaner environment
- Updated TLD section in mcd_post_campaign()

# bptools 0.5.4

## What's New
- file_spellchecker() will check a script to see if character elements contain spelling errors
- mcd_post_campaign() similar to create_project: makes a new McDonald's post-campaign project with standard scripts for the report
- create_script() function to make a new script with headers and data_import() etc.

## Changes
- theme_map_minimal() is now called theme_map(), and theme_map() becomes theme_map_old(). I never used theme_map, so it is annoying to have to type _minimal() every time. Sorry for any broken code

# bptools 0.5.3

## What's New
- Added case_fwhen() function to give ordered factor results when using case_when() statements
- parse_seqmonth() converts integer SEQMONTH attributes to Date

## Bug Fixes
- Fixed a bad join in bnz_spend_origin() which was duplicating some spending (whoops)

## Small Changes
- add_basemap() will now plot holes. Still not ideally implemented to deal with islands within lakes ie rotorua
- Improved the way that the bounding box is defined in zoom_definiton() with ratio != 1
- default legend.box in theme_mvl() etc. is now "vertical"
- mcd_process() now deals with data from the mcd_bp_final_card_dataset2 table
- pal_mvl() gives greys when it runs out of qualitative colours

# bptools 0.5.2

## Small Changes
- Slight update to diverging colour palettes in pal_mvl()
- catchment_picker() can now run without spending or location
- Done away with the base_dir argument in scripts made by create_project()
- Made the plot.caption in theme_mvl() a bit smaller and standard font weight
- create_project() sets default ggplot colour scales using pal_mvl()
- create_project() no longer uses the base_dir argument in script templates
- zoom_definition() no longer needs to be added as a function to a ggplot object ie p = p + zoom_definition(shp_dat)() is now p = p + zoom_definition(shp_dat)

# bptools 0.5.1

## What's New
- zoom_place() for map boundaries based on place names
- percent_change() and dollar_change() for representing growth figures
- added ieo_radius_locator(): an IEO-specific version of merch_radius_locator()
- added mb_radius_locator(): finds all meshblocks within radius of a point (based on meshblock centroid)
- Added ft_() aliases for flextable_() functions

## Small Changes
- get_merchant_location() uses the imputed location if exact coordinates are not available (with a warning)
- slight changes on merch_radius_locator()
- major gridlines in theme_mvl() are lighter
- default for flextable_negatives() is all columns
- added capability in mcd_process() to treat bunch_namez
- more colour palettes in scale_mvl()
- Default for flextable_cell_bold() is to highlight the final row

## Bug Fixes
- fixed a bug in make_mb_df() & make_cau_df() where duplicate polygons could be introduced


# bptools 0.5

## What's New
- Added scale_mvl() functions for creating using MVL branded colour/fill scales
- Data objects are now found in the new mvldata package

## Small Changes
- Additional MVL colours: mvl_fire, mvl_royal2, & mvl_fuchsia
- Added mvl_text colour for text in reports
- Default text colour for plats and flextables is now mvl_text
- Added cagr() for Compound Annual Growth Rate
- theme_mvl() now has y axis title by default
- catchment_picker() now has support for Primary, Secondary, and Tertiary catchment areas
- A few updates to the default scripts in create_project()
- flextable_joined() now adds .$vars to the object
- make_footnote() now refers to the new logo by default


## Bug Fixes
- Missing bracket in the code templates from create_project()
- mcd_process() will only process columns that are character variables, and leaves factors unchanged
- better theme_mcd()

# bptools 0.4.0

## What's New:
* Added mvl_process() to treat the new standard analytics atributes
* Added merch_radius_locator() to find all Fozzie merchants within a given radius
* Added vec_sql_string() to convert vectors into SQL lists for queries
* Added flextable_negatives() to highlight negative values in red
* flextable_joined() for merged flextables


## Small changes:
* Updated mcd_process() to deal with the AGEX attribute, if it exists
* Changed default font sizes in mvl ggplot themes
* Added comma formatting to *dollar() functions
* Adjusted the treatment for negative values in *dollar() functions
* Updates to the default packages imported in create_project()
* Added base_dir() to the processing script in create_project()
* Upadted the default title slide values in the report script from create_project()
* Tidied up the create_project() initialisation script
* FlexTables created by any of the flextable_*() functions now have an additional value: .$vals, which contains the original data used to build the flextable

## Bug Fixes:
* Fixed the QSR flag in mcd_process()
* Changed the attribute type in 06 CAU data frames from character to integer
* Fixed bugs with make_cau_df() and make_mb_df()

# bptools 0.3.0

## What's New:
* Updated to reflect the company re-brand:
    - Additional colours added to the palette
    - Added flextable_leaf() and flextable_black()
    - Updated ggplot themes to use Helvetica Neue fonts (the former theme_mvl() is available as theme_mvl_old())
    - create_project() now imports the new PowerPoint template (as well as the old template as mvl_template_old.pptx). It should work as per the old template, but the ordering oof some of the content might have changed in some slide masters, so just be sure to double-check that everything is added in the right order.

# bptools 0.2.3

## What's New:
* Added get_meshblock_data() to import meshblock shapefiles
* Added make_cau_df() to create a data frame of area units
* Added make_mb_df() to create a data frame of meshblocks
* Added flextable_dark() for a dark-themed flextable
* Added inland_water.df to plot lakes 'n shit on maps
* Added water.df to plot the sea on maps
* Added data_import() to tidy up importing processed data for an R project
* Added percent() function which gives better control over the decimal places than scales::percent()
* Added source_dir() function to source all scripts in a directory (this has been added to the initialisation scripts in create_project())

## Updates
* Updated flextable_row_highlight() to be able to highlight particular cells, rather than entire rows only
* Scripts created by create_project() now use import_data() rather than a big mess of code

# bptools 0.2.2

## What's New
* Added some wrappers for ReporteRs functions which make dealing with FlexTables easier
* Added kdollar() - for labelling in thousands of dollars
* Added output_archive() - a function to archive the existing results in a project directory
* Added trim_ws() - a function to remove leading/trailing whitespace from character values

## Updates
* Added more parameters to zoom_definition() - now possible to specify margins and the aspect ratio of the plot
* create_project() has had an overhaul:
  - results.R now loads processed_data.RData and gives a warning about the processing date, rather than sourcing the processing script when required
  - report.R is now created, for saving results directly as a powerpoint presentation via ReporteRs
  - creates a RUN SCRIPT which will run all the project scripts in sequence
  - imports mvl_template.pptx to the base directory for use with ReporteRs
  - output_archive is now moved to its own function, rather than ad-hoc code

# bptools 0.2.1

## What's New
* Added zoom_definition() to make selecting plot areas in maps easier

## Small Changes
* Removed underscores from file names in create_project()
* Processing script created by create_project() now saves the workspace by default
* More consistency in mdollar() and bdollar()
* Added regional aggregation and a QSR indicator to mcd_process()


# bptools 0.2.0

## What's new
* Added catchment_picker() - a shiny app to identify and save a catchment for a merchant (requires shiny, leaflet, sp, rgeos, rgdal)
* Added bnz_spend_origin() - to get the spending by CAU for a given merchant + time period
* Added get_merch_location() - a function to get the coordinates of a merchant from Fozzie
* Added bdollar() function - use is the same as mdollar()

## Other Changes
* mcd_process imputes "" when CUST_TYPE is null
* renamed the attributes of nz_cau_13.spdf & nz_cau_06.spdf to be consistent

# bptools 0.1.6

## What's new
* Updated theme_mvl to include more distinct grid lines - the version without them is theme_mvl_old
* Added NZ TLA shapefiles and data.frames
* Added mcd_process, to perform common processing on raw McDonald's data

## Other Changes
* Default mvl_foot for ppt_png is now FALSE
* save_plot now has an argument to select wide/square plots for ppt

## Bug fixes
* Fixed the theme_mcd

# bptools 0.1.5

## What's new
* Added CAU and MB maps for Tauranga
* Added Stats NZ CAU population projections
* Added 2013 meshblock data for Tauranga and Wellington

## Other stuff
* Small updates to create_project


# bptools 0.1.4

## What's New
* Added shapefiles for NZ CAUs because I had to use that data this morning.

## Bug Fixes
* Bug fixes on `ppt_png`
* `create_project` has been updated to create scripts that are ready to use with the correct project name and directories.


# bptools 0.1.3

## What's New
* Added a `NEWS.md` file to track changes to the package.

## Bug Fixes
* `bounding_box()` works now, and added the option not to draw the border.
* `save_plot()` works, had a misnamed variable.
* `save_table()` coerces to data.frame first, so it'll work in a dplyr chain that returns a `tibble`/`tbl`.
* removed the whitespace at the bottom of the plot in `theme_mvl()`
