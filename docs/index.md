# CpkTemplate: Process Capability Analysis Tool

An interactive Shiny application designed to streamline the calculation
of **Process Capability Indices (Cpk)** from raw manufacturing data.

## Key Features

- **Dynamic Ingestion**: Upload CSV files and automatically map
  measurement columns.
- **Smart Filtering**: Exclude specific rows using Regex-based numeric
  filtering to remove outliers or setup data.
- **Visual Insights**: Real-time table highlighting for values below Cpk
  thresholds (Red \< 1.0, Yellow \< 1.33).
- **Professional Export**: Generate stylized Excel reports with
  color-coded results ready for management review.

## Calculation Logic

The tool calculates Cpk based on the standard formula:
``` math
Cpk = \min\left( \frac{USL - \mu}{3\sigma}, \frac{\mu - LSL}{3\sigma} \right)
```
where $`\mu`$ is the mean and $`\sigma`$ is the standard deviation of
the selected data range.
