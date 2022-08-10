# CAPA Guide

## Welcome to the Conflict Affected Populations App (CAPA)

This app has a number of features for exploring the number of people affected by conflict over various spatial and temporal combinations.

This is done by combining data on conflict events from the UCDP GED database and gridded population data from the GPW data set

UCDP GED records entries for armed conflict events with the date on which they occurred, a latitude and longitude of the event, and an estimate of the number of fatalities that occurred. GPW provides global population estimates at a resolution of 2.5 arc minutes (roughly 5km\^2 at the equator). The GPW data is available from 1990-2020 in 5 year increments, so intermediate years are linearly interpolated.

The geoBoundaries CGAZ data set is used for level 1 administrative boundaries (province/state/etc.) and allows sub-national groupings of populated grid-cells

These data are combined by buffering GED events by 25, 50, and 100 kilometers and counting the intersections for each population grid-cell on a monthly temporal resolution to allow for differing frameworks on the spatial effect of conflict. Each population grid-cell receives a monthly "score" based on the number of event intersections that is determined by multiplying these counts by a weighting framework.

To use the app, begin with the *Weights* tab.

### Weights

Here you can select whether to use 25, 50, or 100km as the radius for a conflict event's effect, as well as 3 different event categories. Events are counted separately as low, medium, or high fatality events depending on how many deaths occurred during that single event (1-9, 10-24, 25+ respectively). You can also adjust the values weight higher-fatality events more or apply weights that will emphasize closer-proximity events over more distant events. The default framework is a count of all events that took place within 100km of a population grid-cell and is considered "unweighted" because there is no distinction made between low and high fatality-level events nor close and far proximity events.

There is the option to use the raw number of battle-related deaths to generate the score for each grid-cell at any given proximity instead of counts of categorized events by setting the first 3 rows to 0 and adjusting the values of the final "B-deaths" row. These "B-death" weights should not be mixed with the count weights and should therefore be set to 0 when using counts and vice versa.

Please note that all weight values must be whole numbers. Instead of something like 0.25, 0.5, and 1 - use 1, 2, and 4. Scores can then be normalized to any value after the analysis has run.

The weights you set here will be applied in all subsequent calculations in other tabs

### Conflict Exposure

Here you can generate a table that details the population at risk of conflict above a specified "score" threshold over a temporal period at either the country-level or ADM1-level.
The workflow is as follows:
1. Select any numbers of countries or regions from the "Select Country" selection
2. Define the range of years for which you want results
3. Select whether you want the temporal unit of analysis to be aggregated by months, quarters, halves, or years
4. Choose ADM0 for country level aggregation or ADM1 for Admin 1 level aggregation
5. Supply an "Intensity threshold". This corresponds to the grid-cell "score" and is applied at the grid-cell level. Only populations in grid-cells that have "scores" equal to or higher than the threshold will be included and counted as "at risk".

### Exposure Map

This calculation is the exact same as the *Conflict Exposure* tab, but outputs a map rather than a table. One key difference is that you must select 1 year at a time to map rather than a range of years for a table. If you select a period size less than "yearly", you can supply which half/quarter/month of the selected year you wish to map.

The map will be sized according to the window size of the app and any changes to the legend font or key size can be applied by re-submitting. The plot can either be right-click-saved as a .png or downloaded as a .pdf using the "Download Plot" button.

### Score Map

This tab will show you the actual non-spatially-aggregated grid-cell "scores" temporally-aggregated over a given period. Typically you should set the year range to a single year as all selected year-scores will be added together to create a single score for plotting. If you would like to look at the score from June 2020 to July 2021, you can set the year range from 2020:2021 and set the start month to "6" and the end month to "7". The generated map will show the country outline, but you can include the ADM1 boundaries for reference by selected the check box. The GED conflict events from that period can also be added to the map by checking the "show conflict events" box.

### Conflict Duration

This tab uses similar parameters to the *Conflict Exposure* and *Score Map* tabs, but is used for measuring the duration of consecutive conflict over a specified period. This means that only grid-cells that experienced yearly/biannually/quarterly/monthly "scores" above the "Intensity Threshold" for every single period in the time-range will be counted as population at risk. If you select ADM1-level aggregation you will also have the option to change the output from a table to a map.

### Conflict Frequency

Similar to duration, conflict frequency is how many periods over a time-range a population was affected by conflict - irrespective to whether it was consecutive exposure. 

