#'Process Capability Analyzer UI
#'
#' @description 
#' Defines the interface for the Cpk Analyzer, including sidebar inputs for file 
#' uploads and parameter selection, and a main panel for data visualization.
#'
#' @section UI Components:
#' 
#' This layout utilizes a Sidebar-Main structure. The sidebar handles data 
#' ingestion and filtering, while the main panel hosts the interactive 
#' DataTables and summary statistics.
#'
#' @rdname ui
#' @name ui
NULL

#' Process Capability Analyzer Server
#'
#' @description
#' The engine that powers the application. It handles reactive data ingestion,
#' statistical modeling (Cpk, sigma), and the generation of formatted outputs.
#'
#' @section Data Ingestion:
#' 
#' Parses semicolon-separated CSVs. Handles Row ID exclusion and specific value 
#' filtering while preserving the critical 4-row metadata header.
#'
#' @section Selection Observers:
#' 
#' Automatically maps CSV column indices to user-friendly labels. Provides 
#' "Select All" functionality to streamline analysis for high-channel datasets.
#'
#' @section Processing Logic:
#' 
#' Calculates statistical metrics. Dynamically truncates datasets to the first 
#' 50 samples when the "Gauge Limit" is active to satisfy standard study requirements.
#'
#' @section Excel Export:
#' 
#' Bridges the gap between web analysis and desktop reporting by generating 
#' `.xlsx` files that retain the structured summary data.
#'
#' @section Table Rendering:
#' 
#' Injects custom JavaScript into the DataTables display to provide real-time 
#' visual heatmapping based on $Cpk$ thresholds ($<1.0, 1.0-1.33, >1.33$).
#'
#' @rdname server
#' @name server
NULL