# Process Capability Analyzer Server

The engine that powers the application. It handles reactive data
ingestion, statistical modeling (Cpk, sigma), and the generation of
formatted outputs.

## Data Ingestion

Parses semicolon-separated CSVs. Handles Row ID exclusion and specific
value filtering while preserving the critical 4-row metadata header.

## Selection Observers

Automatically maps CSV column indices to user-friendly labels. Provides
"Select All" functionality to streamline analysis for high-channel
datasets.

## Processing Logic

Calculates statistical metrics. Dynamically truncates datasets to the
first 50 samples when the "Gauge Limit" is active to satisfy standard
study requirements.

## Excel Export

Bridges the gap between web analysis and desktop reporting by generating
\`.xlsx\` files that retain the structured summary data.

## Table Rendering

Injects custom JavaScript into the DataTables display to provide
real-time visual heatmapping based on \$Cpk\$ thresholds (\$\<1.0,
1.0-1.33, \>1.33\$).
