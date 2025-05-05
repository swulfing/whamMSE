#' Generate MSE Output Plots in PNG or HTML Format
#'
#' This function creates a comprehensive set of plots to visualize MSE (Management Strategy Evaluation)
#' results from a list of model simulations. Outputs can be saved as individual PNG files or as a single
#' interactive HTML report.
#'
#' @param mods A list of model outputs from multiple simulations and estimation models.
#' @param output_dir Directory where plots or HTML report will be saved. Default is "Report".
#' @param output_format Output type: either "png" for PNG files or "html" for an interactive HTML report.
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
#' @return Saves output plots to disk; returns nothing explicitly.
#' @export
plot_mse_output <- function(mods,
                            output_dir = "Report",
                            output_format = c("png", "html", "pdf"),
                            width = 10, height = 7, dpi = 300,
                            col.opt = "D",
                            new_model_names = NULL,
                            base.model = "Model1",
                            start.years = 1,
                            use.n.years.first = 5,
                            use.n.years.last = 5) {
  
  output_format <- match.arg(output_format)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  is.nsim <- if (!is.list(mods[[1]][[1]][[1]])) FALSE else TRUE
  
  if (output_format == "png") {
    cat("\nGenerating PNG plots...\n")
    
    plot_ssb_time_series(mods, is.nsim, getwd(), output_dir, "SSB", width, height, dpi, col.opt, new_model_names)
    plot_fbar_time_series(mods, is.nsim, getwd(), output_dir, "Fbar", width, height, dpi, col.opt, new_model_names)
    plot_catch_time_series(mods, is.nsim, getwd(), output_dir, "Catch", width, height, dpi, col.opt, new_model_names)
    
    plot_relative_trajectories(mods, is.nsim, getwd(), output_dir, base.model, new_model_names, width, height, dpi,col.opt) 
    
    plot_ssb_performance(mods, is.nsim, getwd(), output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.last)
    plot_ssb_performance(mods, is.nsim, getwd(), output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)
    
    plot_fbar_performance(mods, is.nsim, getwd(), output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.last)
    plot_fbar_performance(mods, is.nsim, getwd(), output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)
    
    plot_catch_performance(mods, is.nsim, getwd(), output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.last)
    plot_catch_performance(mods, is.nsim, getwd(), output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)
    
    plot_ssb_performance2(mods, is.nsim, getwd(), output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)
    plot_ssb_performance2(mods, is.nsim, getwd(), output_dir, "SSB", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)
    
    plot_fbar_performance2(mods, is.nsim, getwd(), output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)
    plot_fbar_performance2(mods, is.nsim, getwd(), output_dir, "Fbar", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)
    
    plot_catch_performance2(mods, is.nsim, getwd(), output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)
    plot_catch_performance2(mods, is.nsim, getwd(), output_dir, "Catch", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)
    
    plot_catch_variation(mods, is.nsim, getwd(), sub.dir, var = "Catch", width, height, dpi, col.opt, new_model_names)
    plot_catch_variation(mods, is.nsim, getwd(), sub.dir, var = "Catch", width, height, dpi, col.opt, new_model_names, base.model)
    
    plot_ssb_status(mods, is.nsim, getwd(), output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.last, plot_prob = FALSE)
    plot_ssb_status(mods, is.nsim, getwd(), output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)
    
    plot_fbar_status(mods, is.nsim, getwd(), output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.last)
    plot_fbar_status(mods, is.nsim, getwd(), output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.last, base.model)
    
    plot_ssb_status2(mods, is.nsim, getwd(), output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)
    plot_ssb_status2(mods, is.nsim, getwd(), output_dir, "SSB_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)
    
    plot_fbar_status2(mods, is.nsim, getwd(), output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years)
    plot_fbar_status2(mods, is.nsim, getwd(), output_dir, "Fbar_status", width, height, dpi, col.opt, new_model_names, use.n.years.first, start.years, base.model)
    
    plot_kobe_status(mods, is.nsim, getwd(), output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = FALSE)
    plot_kobe_status(mods, is.nsim, getwd(), output_dir, width, height, dpi, col.opt, new_model_names, use.n.years.last, show_density = TRUE)
    
    plot_model_performance_radar(mods, is.nsim, getwd(), output_dir, width, height, dpi, col.opt, use.n.years.first, use.n.years.last, start.years, new_model_names)
    plot_model_performance_triangle(mods, is.nsim, getwd(), output_dir, width, height, dpi, new_model_names, col.opt, use.n.years.first, use.n.years.last, start.years)
    
    plot_mean_rec_par(mods, is.nsim, getwd(), output_dir, width, height, dpi, col.opt, new_model_names)
    plot_NAA_sigma_par(mods, is.nsim, getwd(), output_dir, width, height, dpi, col.opt, new_model_names)
    
  } else if (output_format == "html") {
    cat("\nGenerating HTML report...\n")
    
    rmd_path <- file.path(output_dir, "mse_report.Rmd")
    html_path <- file.path(output_dir, "mse_report.html")
    saveRDS(mods, file = file.path(output_dir, "mods_tmp.RDS"))
    
    writeLines(c(
      "---",
      "title: \"MSE Output Report\"",
      "output: html_document",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
      "library(ggplot2); library(dplyr); library(tidyr); library(fmsb)",
      "library(ggpubr); library(gridExtra); library(ggtern)",
      "mods <- readRDS('mods_tmp.RDS')",
      "is.nsim <- if (!is.list(mods[[1]][[1]][[1]])) FALSE else TRUE",
      "output_dir <- '.'",
      paste0("width <- ", width, "; height <- ", height, "; dpi <- ", dpi, "; col.opt <- '", col.opt, "'"),
      paste0("new_model_names <- ",
             if (is.null(new_model_names)) "NULL" else deparse(new_model_names)),
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
    ), con = rmd_path)
    
    rmarkdown::render(rmd_path, output_file = html_path, output_dir = output_dir, quiet = TRUE)
    browseURL(html_path)
  } else if (output_format == "pdf") {
    cat("\nGenerating PDF report...\n")
    
    dev.off()
    cat(paste0("\nSaved PDF report to: ", pdf_path, "\n"))
  }
}


