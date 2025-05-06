#' Generate MSE Output Plots in PNG, HTML, or PDF Format
#'
#' This function creates a comprehensive set of plots to visualize MSE (Management Strategy Evaluation)
#' results from a list of model simulations. Outputs can be saved as individual PNG files or as a single
#' interactive HTML or PDF report.
#'
#' @param mods A list of model outputs from multiple simulations and estimation models.
#' @param main_dir The main directory path where results should be saved.
#' @param output_dir Folder name inside main_dir where plots/reports will be saved (default: "Report").
#' @param output_format Output type: "png", "html", or "pdf".
#' @param width Width of each plot in inches.
#' @param height Height of each plot in inches.
#' @param dpi Resolution of saved plots in dots per inch.
#' @param col.opt Color palette option for `viridis`-based scales.
#' @param new_model_names Optional character vector of custom model names.
#' @param base.model Model name used as a baseline for relative difference plots.
#' @param start.years The first year used in short-term performance summaries.
#' @param use.n.years.first Number of years to summarize at the beginning of the time series.
#' @param use.n.years.last Number of years to summarize at the end of the time series.
#'
#' @return Saves output plots/reports to disk; returns nothing explicitly.
#' @export
plot_mse_output <- function(mods,
                            main_dir,
                            output_dir = "Report",
                            output_format = c("png", "html", "pdf"),
                            width = 10, height = 7, dpi = 300,
                            col.opt = "D",
                            new_model_names = NULL,
                            base.model = "Model1",
                            start.years = 1,
                            use.n.years.first = 5,
                            use.n.years.last = 5) {
  
  # Match output format
  output_format <- match.arg(output_format)
  
  # Build full path
  full_output_dir <- file.path(main_dir, output_dir)
  if (!dir.exists(full_output_dir)) dir.create(full_output_dir, recursive = TRUE)
  
  # Check if it's multi-simulation
  is.nsim <- if (!is.list(mods[[1]][[1]][[1]])) FALSE else TRUE
  
  if (output_format == "png") {
    cat("\nGenerating PNG plots...\n")
    
    # List of plot functions
    plot_calls <- list(
      function() plot_ssb_time_series(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names),
      function() plot_fbar_time_series(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names),
      function() plot_catch_time_series(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names),
      function() plot_relative_trajectories(mods, is.nsim, main_dir, output_dir, base.model, new_model_names, width, height, dpi, col.opt),
      function() plot_ssb_performance(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.last),
      function() plot_ssb_performance(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model),
      function() plot_fbar_performance(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.last),
      function() plot_fbar_performance(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model),
      function() plot_catch_performance(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.last),
      function() plot_catch_performance(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model),
      function() plot_ssb_performance2(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years),
      function() plot_ssb_performance2(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model),
      function() plot_fbar_performance2(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years),
      function() plot_fbar_performance2(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model),
      function() plot_catch_performance2(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years),
      function() plot_catch_performance2(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model),
      function() plot_catch_variation(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names),
      function() plot_catch_variation(mods, is.nsim, main_dir, output_dir, "Catch", width, height, dpi, col.opt, new_model_names, base.model),
      function() plot_ssb_variation(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names),
      function() plot_ssb_variation(mods, is.nsim, main_dir, output_dir, "SSB", width, height, dpi, col.opt, new_model_names, base.model),
      function() plot_fbar_variation(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names),
      function() plot_fbar_variation(mods, is.nsim, main_dir, output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, base.model),
      function() plot_ssb_status(mods, is.nsim, main_dir, output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.last),
      function() plot_ssb_status(mods, is.nsim, main_dir, output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model),
      function() plot_ssb_status2(mods, is.nsim, main_dir, output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years),
      function() plot_ssb_status2(mods, is.nsim, main_dir, output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model),
      function() plot_fbar_status(mods, is.nsim, main_dir, output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.last),
      function() plot_fbar_status(mods, is.nsim, main_dir, output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model),
      function() plot_fbar_status2(mods, is.nsim, main_dir, output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years),
      function() plot_fbar_status2(mods, is.nsim, main_dir, output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model),
      function() plot_kobe_status(mods, is.nsim, main_dir, output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = FALSE),
      function() plot_kobe_status(mods, is.nsim, main_dir, output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = TRUE),
      function() plot_model_performance_radar(mods, is.nsim, main_dir, output_dir, width, height, dpi, col.opt, use.n.years.first, use.n.years.last, start.years, new_model_names),
      function() plot_model_performance_triangle(mods, is.nsim, main_dir, output_dir, width, height, dpi, new_model_names, col.opt, use.n.years.first, use.n.years.last, start.years),
      function() plot_model_performance_bar(mods, is.nsim, main_dir, output_dir, new_model_names, width, height, dpi, col.opt, use.n.years.first, use.n.years.last, start.years),
      function() plot_mean_rec_par(mods, is.nsim, main_dir, output_dir, width, height, dpi, col.opt, new_model_names),
      function() plot_NAA_sigma_par(mods, is.nsim, main_dir, output_dir, width, height, dpi, col.opt, new_model_names)
    )
    
    # Run all plot functions
    for (call in plot_calls) {
      tryCatch(call(), error = function(e) message("Error in plot: ", e$message))
    }
    
  } else if (output_format %in% c("html", "pdf")) {
    cat(paste0("\nGenerating ", toupper(output_format), " report...\n"))
    
    rmd_path <- file.path(full_output_dir, "mse_report.Rmd")
    report_file <- paste0("mse_report.", output_format)
    report_path <- file.path(full_output_dir, report_file)
    
    # Rmd content
    rmd_lines <- c(
      "---",
      paste0("title: \"MSE Output Report\""),
      paste0("output: ", ifelse(output_format == "html", "html_document", "pdf_document")),
      "params:",
      "  mods: NULL",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "library(ggplot2); library(dplyr); library(tidyr); library(fmsb)",
      "library(ggpubr); library(gridExtra); library(ggtern)",
      "mods <- params$mods",
      "is.nsim <- if (!is.list(mods[[1]][[1]][[1]])) FALSE else TRUE",
      paste0("output_dir <- '.'"),
      paste0("width <- ", width, "; height <- ", height, "; dpi <- ", dpi, "; col.opt <- '", col.opt, "'"),
      paste0("new_model_names <- ", if (is.null(new_model_names)) "NULL" else deparse(new_model_names)),
      paste0("base.model <- '", base.model, "'"),
      paste0("start.years <- ", start.years),
      paste0("use.n.years.first <- ", use.n.years.first),
      paste0("use.n.years.last <- ", use.n.years.last),
      "```",
      "",
      "```{r all-plots}",
      "plot_relative_trajectories1(mods, is.nsim, '.', output_dir, base.model, new_model_names, width, height, dpi, col.opt)",
      "plot_relative_trajectories2(mods, is.nsim, '.', output_dir, base.model, new_model_names, width, height, dpi, col.opt)",
      "plot_ssb_time_series(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names)",
      "plot_fbar_time_series(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names)",
      "plot_catch_time_series(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names)",
      "plot_ssb_performance(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names, use.n.years.last)",
      "plot_ssb_performance(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)",
      "plot_fbar_performance(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names, use.n.years.last)",
      "plot_fbar_performance(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)",
      "plot_catch_performance(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names, use.n.years.last)",
      "plot_catch_performance(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)",
      "plot_ssb_performance2(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)",
      "plot_ssb_performance2(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)",
      "plot_fbar_performance2(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)",
      "plot_fbar_performance2(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)",
      "plot_catch_performance2(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)",
      "plot_catch_performance2(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)",
      "plot_catch_variation(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names)",
      "plot_catch_variation(mods, is.nsim, '.', output_dir, 'Catch', width, height, dpi, col.opt, new_model_names, base.model)",
      "plot_ssb_variation(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names)",
      "plot_ssb_variation(mods, is.nsim, '.', output_dir, 'SSB', width, height, dpi, col.opt, new_model_names, base.model)",
      "plot_fbar_variation(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names)",
      "plot_fbar_variation(mods, is.nsim, '.', output_dir, 'Fbar', width, height, dpi, col.opt, new_model_names, base.model)",
      "plot_ssb_status(mods, is.nsim, '.', output_dir, 'SSB_status', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model = NULL)",
      "plot_ssb_status(mods, is.nsim, '.', output_dir, 'SSB_status', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model, plot_prob = FALSE)",
      "plot_ssb_status2(mods, is.nsim, '.', output_dir, 'SSB_status', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model = NULL)",
      "plot_ssb_status2(mods, is.nsim, '.', output_dir, 'SSB_status', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model, plot_prob = FALSE)",
      "plot_fbar_status(mods, is.nsim, '.', output_dir, 'Fbar_status', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model = NULL)",
      "plot_fbar_status(mods, is.nsim, '.', output_dir, 'Fbar_status', width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model, plot_prob = FALSE)",
      "plot_fbar_status2(mods, is.nsim, '.', output_dir, 'Fbar_status', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model = NULL)",
      "plot_fbar_status2(mods, is.nsim, '.', output_dir, 'Fbar_status', width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model, plot_prob = FALSE)",
      "plot_kobe_status(mods, is.nsim, '.', output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = FALSE)",
      "plot_kobe_status(mods, is.nsim, '.', output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = TRUE)",
      "plot_model_performance_radar(mods, is.nsim, '.', output_dir, width, height, dpi, col.opt, use.n.years.first, use.n.years.last, start.years, new_model_names)",
      "plot_model_performance_triangle(mods, is.nsim, '.', output_dir, width, height, dpi, new_model_names, col.opt, use.n.years.first, use.n.years.last, start.years)",
      "plot_model_performance_bar(mods, is.nsim, '.', output_dir, new_model_names, width, height, dpi, col.opt, use.n.years.first, use.n.years.last, start.years)",
      "plot_mean_rec_par(mods, is.nsim, '.', output_dir, width, height, dpi, col.opt, new_model_names)",
      "plot_NAA_sigma_par(mods, is.nsim, '.', output_dir, width, height, dpi, col.opt, new_model_names)",
      "```"
    )
    
    writeLines(rmd_lines, con = rmd_path)
    
    # Render report
    rmarkdown::render(
      rmd_path,
      output_file = report_file,
      output_dir = full_output_dir,
      params = list(mods = mods),
      envir = new.env(),
      quiet = TRUE
    )
    cat(paste0("\nSaved ", toupper(output_format), " report to: ", report_path, "\n"))
    if (output_format == "html") browseURL(report_path)
  }
}