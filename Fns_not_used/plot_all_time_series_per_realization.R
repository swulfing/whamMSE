plot_all_time_series_per_realization <- function(mods, main.dir, sub.dir,
                                                 width = 10, height = 7, dpi = 300,
                                                 col.opt = "D", new_model_names = NULL,
                                                 pdf_name = "All_TS_by_realization.pdf",
                                                 realizations_to_plot = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(grDevices)
  
  # --- Detect single vs multiple realizations ---
  is.nsim <- is.list(mods[[1]]) && !is.null(mods[[1]][[1]]$om)
  
  if (is.nsim) {
    n_realizations <- length(mods)
    Years <- mods[[1]][[1]]$om$years
    n_models <- length(mods[[1]])
  } else {
    n_realizations <- 1
    Years <- mods[[1]]$om$years
    n_models <- length(mods)
    mods <- list(mods)  # Wrap single realization as list for consistency
  }
  
  # Default to all realizations
  if (is.null(realizations_to_plot)) {
    realizations_to_plot <- seq_len(n_realizations)
  }
  
  # Safety check
  if (any(realizations_to_plot > n_realizations | realizations_to_plot < 1)) {
    stop("One or more values in realizations_to_plot are out of bounds.")
  }
  
  # Create output directory if needed
  dir.create(file.path(main.dir, sub.dir), recursive = TRUE, showWarnings = FALSE)
  
  # Open multi-page PDF
  pdf(file = file.path(main.dir, sub.dir, pdf_name), width = width, height = height)
  
  for (r in realizations_to_plot) {
    # === SSB ===
    df_ssb <- lapply(seq_len(n_models), function(m) {
      data.frame(
        SSB = mods[[r]][[m]]$om$rep$SSB,
        Model = paste0("Model", m),
        Year = Years
      )
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_ssb$Model <- factor(df_ssb$Model,
                             levels = paste0("Model", seq_len(n_models)),
                             labels = new_model_names)
    }
    
    df_ssb <- pivot_longer(df_ssb, cols = starts_with("SSB"), names_to = "Label", values_to = "SSB")
    
    p_ssb <- ggplot(df_ssb, aes(x = Year, y = SSB, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("SSB from realization ", r)) +
      ylab("SSB")
    
    print(p_ssb)
    
    # === Fbar (Global) ===
    n_fleets <- mods[[r]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[r]][[1]]$om$input$data$n_regions[1]
    index_range <- n_fleets + n_regions + 1  # Global Fbar
    
    df_fbar <- lapply(seq_len(n_models), function(m) {
      tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
      tmp <- as.data.frame(tmp)
      names(tmp) <- "Global"
      tmp$Model <- paste0("Model", m)
      tmp$Year <- Years
      tmp
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_fbar$Model <- factor(df_fbar$Model,
                              levels = paste0("Model", seq_len(n_models)),
                              labels = new_model_names)
    }
    
    df_fbar <- pivot_longer(df_fbar, cols = starts_with("Global"), names_to = "Label", values_to = "Fbar")
    
    p_fbar <- ggplot(df_fbar, aes(x = Year, y = Fbar, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("Fbar from realization ", r)) +
      ylab("Fbar")
    
    print(p_fbar)
    
    # === Catch ===
    df_catch <- lapply(seq_len(n_models), function(m) {
      data.frame(
        Catch = mods[[r]][[m]]$om$rep$pred_catch,
        Model = paste0("Model", m),
        Year = Years
      )
    }) %>% bind_rows()
    
    if (!is.null(new_model_names)) {
      df_catch$Model <- factor(df_catch$Model,
                               levels = paste0("Model", seq_len(n_models)),
                               labels = new_model_names)
    }
    
    df_catch <- pivot_longer(df_catch, cols = starts_with("Catch"), names_to = "Label", values_to = "Catch")
    
    p_catch <- ggplot(df_catch, aes(x = Year, y = Catch, color = Model, group = Model)) +
      geom_line(size = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      facet_grid(Label ~ ., scales = "free") +
      theme_bw() +
      ggtitle(paste0("Catch from realization ", r)) +
      ylab("Catch")
    
    print(p_catch)
  }
  
  dev.off()
}