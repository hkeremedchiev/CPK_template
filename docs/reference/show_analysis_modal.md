# Show Detailed Process Analysis Modal

Triggers a popup containing a trend plot with specification limits and a
distribution histogram based on a table cell click.

## Usage

``` r
show_analysis_modal(click, res, output)
```

## Arguments

- click:

  The `input$table_cell_clicked` object from DT.

- res:

  The `processed_info()` reactive list containing the data matrix.

- output:

  The Shiny output object to render the plots.
