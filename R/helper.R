plot_ssb_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
                                 width = 10, height = 7, dpi = 300, col.opt = "D",
                                 new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  if (!is.nsim) {

    Years = mods[[1]]$om$years

    # Only one realization
    res <- lapply(seq_along(mods), function(i) {
      data.frame(
        SSB = mods[[i]]$om$rep$SSB,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1  # default realization
      )
    }) %>% bind_rows()
  } else {

    Years = mods[[1]][[1]]$om$years

    # Multiple realizations
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        data.frame(
          SSB = mods[[r]][[m]]$om$rep$SSB,
          Model = paste0("Model", m),
          Year = Years,
          Realization = rep(r, length(Years))
        )
      }) %>% bind_rows()
    }) %>% bind_rows()

  }

  # Allow renaming Model names
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
  }

  # Pivot longer if needed (for multiple SSB types)
  res <- pivot_longer(res, cols = starts_with("SSB"), names_to = "Label", values_to = "SSB")

  # Plot
  p1 <- ggplot(res, aes(x = Year, y = SSB, color = Model, group = interaction(Model, Realization))) +
    geom_line(size = 0.5, alpha = 0.5) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(var) +
    ylab("SSB") +
    theme_bw()

  # Save the plot
  ggsave(file.path(main.dir, sub.dir, paste0(var, ".PNG")), p1, width = width, height = height, dpi = dpi)
  
  return(p1)  # Return the plot if you want to print or modify later
}

plot_fbar_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)

  make_plot_data <- function(index_range, label_prefix) {

    if (!is.nsim) {

      Years = mods[[1]]$om$years

      # Only one realization
      lapply(seq_along(mods), function(i) {
        tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year <- Years
        tmp$Realization <- 1
        tmp
      }) %>% bind_rows()
    } else {

      Years = mods[[1]][[1]]$om$years

      # Multiple realizations
      lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp <- as.data.frame(tmp)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year <- Years
          tmp$Realization <- r
          tmp
        }) %>% bind_rows()
      }) %>% bind_rows()
    }
  }

  if (!is.nsim) {
    # Get number of fleets and regions
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    # Get number of fleets and regions
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }

  # Fleet-level Fbar
  res_fleet <- make_plot_data(1:n_fleets, "Fleet_")

  # Region-level Fbar
  res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")

  # Global Fbar (only one column)
  res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")

  # Function to handle renaming and plotting
  plot_and_save <- function(res, title, ylab_text, filename) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(res$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      res$Model <- factor(res$Model,
                          levels = paste0("Model", seq_along(new_model_names)),
                          labels = new_model_names)
    }

    res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")

    p <- ggplot(res_long, aes(x = Year, y = Fbar, color = Model, group = interaction(Model, Realization))) +
      geom_line(size = 0.5, alpha = 0.5) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title) +
      ylab(ylab_text) +
      theme_bw()

    ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
    return(p)
  }

  # Create and save each plot
  p_fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet"))
  p_region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region"))
  p_global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global"))

  return(list(fleet = p_fleet, region = p_region, global = p_global))
}

plot_catch_time_series <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
                                   width = 10, height = 7, dpi = 300, col.opt = "D",
                                   new_model_names = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Prepare data
  if (!is.nsim) {
    
    Years = mods[[1]]$om$years
    
    # Only one realization
    res <- lapply(seq_along(mods), function(i) {
      data.frame(
        Catch = mods[[i]]$om$rep$pred_catch,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1
      )
    }) %>% bind_rows()
  } else {
    
    Years = mods[[1]][[1]]$om$years
    
    # Multiple realizations
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        data.frame(
          Catch = mods[[r]][[m]]$om$rep$pred_catch,
          Model = paste0("Model", m),
          Year = Years,
          Realization = r
        )
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Allow renaming Model names
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
  }
  
  # Pivot longer if needed (for multiple Catch types)
  res <- pivot_longer(res, cols = starts_with("Catch"), names_to = "Label", values_to = "Catch")
  
  # Plot
  p1 <- ggplot(res, aes(x = Year, y = Catch, color = Model, group = interaction(Model, Realization))) +
    geom_line(size = 0.5, alpha = 0.5) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(var) +
    ylab("Catch") +
    theme_bw()
  
  # Save the plot
  ggsave(file.path(main.dir, sub.dir, paste0(var, ".PNG")), p1, width = width, height = height, dpi = dpi)
  
  return(p1)  # Return the plot if you want to print or modify later
}

# plot_ssb_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
#                                  width = 10, height = 7, dpi = 300, col.opt = "D",
#                                  new_model_names = NULL,
#                                  use.n.years = NULL) {
#   library(dplyr)
#   library(tidyr) 
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   res <- NULL
#   
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     # Only one realization
#     res <- lapply(seq_along(mods), function(i) {
#       data.frame(
#         SSB = mods[[i]]$om$rep$SSB,
#         Model = paste0("Model", i),
#         Year = Years,
#         Realization = 1  # default realization
#       ) %>% tail(use.n.years)  # use user-specified last n years
#     }) %>% bind_rows()
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     # Multiple realizations
#     res <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         data.frame(
#           SSB = mods[[r]][[m]]$om$rep$SSB,
#           Model = paste0("Model", m),
#           Year = Years,
#           Realization = r
#         ) %>% tail(use.n.years)  # use user-specified last n years
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#   }
#   
#   # Allow renaming Model names
#   if (!is.null(new_model_names)) {
#     if (length(new_model_names) != length(unique(res$Model))) {
#       stop("Length of new_model_names must match the number of models.")
#     }
#     res$Model <- factor(res$Model,
#                         levels = paste0("Model", seq_along(new_model_names)),
#                         labels = new_model_names)
#   }
#   
#   # Pivot longer if needed (for multiple SSB types)
#   res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
#   
#   # Plot
#   p1 <- ggplot(res, aes(x = Model, y = !!sym(var), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var, "Last", use.n.years, "Years")) +
#     ylab(var) +
#     theme_bw()
#   
#   # Save the plot
#   ggsave(file.path(main.dir, sub.dir, paste0(var, "_last_",use.n.years,"_years.PNG")), p1, width = width, height = height, dpi = dpi)
#   
#   return(p1)
# }

plot_ssb_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
                                 width = 10, height = 7, dpi = 300, col.opt = "D",
                                 new_model_names = NULL,
                                 use.n.years = NULL,
                                 base.model = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
  
  res <- if (!is.nsim) {
    lapply(seq_along(mods), function(i) {
      data.frame(
        SSB = mods[[i]]$om$rep$SSB,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1
      ) %>% tail(use.n.years)
    }) %>% bind_rows()
  } else {
    lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        data.frame(
          SSB = mods[[r]][[m]]$om$rep$SSB,
          Model = paste0("Model", m),
          Year = Years,
          Realization = r
        ) %>% tail(use.n.years)
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Handle model renaming
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names. Make sure it's the renamed version (e.g., 'M1').")
      }
    }
  }
  
  # Pivot longer if multiple SSB variables
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
  
  # Compute relative difference from base.model if specified
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = !!sym(var)) %>%
      select(Realization, Year, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(!!var := (!!sym(var)) / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = SSB, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0(ifelse(is.null(base.model), var, paste0("Relative ", var, " vs ", base.model)),
                   ": Last ", use.n.years, " Years")) +
    ylab(ifelse(is.null(base.model), var, paste0("Relative ", var, " Difference"))) +
    theme_bw()
  
  # Save
  plot_name <- paste0(var, ifelse(is.null(base.model), "", "_rel"),
                      "_last_", use.n.years, "_years.PNG")
  ggsave(file.path(main.dir, sub.dir, plot_name),
         p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}

# plot_ssb_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
#                                   width = 10, height = 7, dpi = 300, col.opt = "D",
#                                   new_model_names = NULL,
#                                   use.n.years = NULL,
#                                   start.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   if (is.null(start.years)) cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
#   
#   res <- NULL
#   
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     # Only one realization
#     res <- lapply(seq_along(mods), function(i) {
#       tmp <- data.frame(
#         SSB = mods[[i]]$om$rep$SSB,
#         Model = paste0("Model", i),
#         Year = Years,
#         Realization = 1  # default realization
#       ) 
#       start.row <- if (is.null(start.years)) 1 else start.years
#       n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#       
#       if (!is.null(start.row)) {
#         # Custom start row slicing
#         start_idx <- start.row
#         end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#         tmp <- tmp[start_idx:end_idx, ]
#       }
#     }) %>% bind_rows()
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     # Multiple realizations
#     res <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         tmp <- data.frame(
#           SSB = mods[[r]][[m]]$om$rep$SSB,
#           Model = paste0("Model", m),
#           Year = Years,
#           Realization = r
#         )
#         start.row <- if (is.null(start.years)) 1 else start.years
#         n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#         
#         if (!is.null(start.row)) {
#           # Custom start row slicing
#           start_idx <- start.row
#           end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#           tmp <- tmp[start_idx:end_idx, ]
#         }
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#   }
#   
#   # Allow renaming Model names
#   if (!is.null(new_model_names)) {
#     if (length(new_model_names) != length(unique(res$Model))) {
#       stop("Length of new_model_names must match the number of models.")
#     }
#     res$Model <- factor(res$Model,
#                         levels = paste0("Model", seq_along(new_model_names)),
#                         labels = new_model_names)
#   }
#   
#   # Pivot longer if needed (for multiple SSB types)
#   res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
#   
#   # Plot
#   p1 <- ggplot(res, aes(x = Model, y = !!sym(var), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var, "First", use.n.years, "Years")) +
#     ylab(var) +
#     theme_bw()
#   
#   # Save the plot
#   ggsave(file.path(main.dir, sub.dir, paste0(var, "_first_",use.n.years,"_years.PNG")), p1, width = width, height = height, dpi = dpi)
#   
#   return(p1)
# }

plot_ssb_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  new_model_names = NULL,
                                  use.n.years = NULL,
                                  start.years = NULL,
                                  base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  res <- NULL
  
  Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
  
  if (!is.nsim) {
    res <- lapply(seq_along(mods), function(i) {
      tmp <- data.frame(
        SSB = mods[[i]]$om$rep$SSB,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1
      )
      start_idx <- start.years
      end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
      tmp[start_idx:end_idx, ]
    }) %>% bind_rows()
  } else {
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        tmp <- data.frame(
          SSB = mods[[r]][[m]]$om$rep$SSB,
          Model = paste0("Model", m),
          Year = Years,
          Realization = r
        )
        start_idx <- start.years
        end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
        tmp[start_idx:end_idx, ]
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename Model names if needed
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  # Pivot longer if multiple SSB types
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
  
  # Apply relative comparison if base.model is specified
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = !!sym(var)) %>%
      select(Realization, Year, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(!!var := (!!sym(var)) / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = SSB, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0(ifelse(is.null(base.model), var, paste0("Relative ", var, " vs ", base.model)),
                   ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
    ylab(ifelse(is.null(base.model), var, paste0("Relative ", var, " Difference"))) +
    theme_bw()
  
  # Save the plot
  plot_name <- paste0(var, ifelse(is.null(base.model), "", "_rel"),
                      "_first_", use.n.years, "_years.PNG")
  ggsave(file.path(main.dir, sub.dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}

# plot_fbar_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
#                                   width = 10, height = 7, dpi = 300, col.opt = "D",
#                                   new_model_names = NULL,
#                                   use.n.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   make_plot_data <- function(index_range, label_prefix) {
#     if (!is.nsim) {
#       
#       Years = mods[[1]]$om$years
#       
#       # Get number of fleets and regions
#       n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
#       n_regions <- mods[[1]]$om$input$data$n_regions[1]
#       
#       # Only one realization
#       lapply(seq_along(mods), function(i) {
#         tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
#         tmp <- as.data.frame(tmp)
#         names(tmp) <- paste0(label_prefix, seq_along(index_range))
#         tmp$Model <- paste0("Model", i)
#         tmp$Year <- Years
#         tmp$Realization <- 1
#         tmp <- tail(tmp, use.n.years)  # use user-specified last n years
#       }) %>% bind_rows()
#     } else {
#       
#       Years = mods[[1]][[1]]$om$years
#       
#       # Get number of fleets and regions
#       n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
#       n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
#       
#       # Multiple realizations
#       lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
#           tmp <- as.data.frame(tmp)
#           names(tmp) <- paste0(label_prefix, seq_along(index_range))
#           tmp$Model <- paste0("Model", m)
#           tmp$Year <- Years
#           tmp$Realization <- r
#           tmp <- tail(tmp, use.n.years)  # use user-specified last n years
#         }) %>% bind_rows()
#       }) %>% bind_rows()
#     }
#   }
#   
#   # Fleet-level Fbar
#   res_fleet <- make_plot_data(1:n_fleets, "Fleet_")
#   
#   # Region-level Fbar
#   res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
#   
#   # Global Fbar (only one column)
#   res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")
#   
#   # Function to handle renaming and plotting
#   plot_and_save <- function(res, title, ylab_text, filename) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(res$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       res$Model <- factor(res$Model,
#                           levels = paste0("Model", seq_along(new_model_names)),
#                           labels = new_model_names)
#     }
#     
#     res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                              names_to = "Label", values_to = "Fbar")
#     
#     p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   # Create and save each plot
#   p_fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet_last_",use.n.years,"_years"))
#   p_region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region_last_",use.n.years,"_years"))
#   p_global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global_last_",use.n.years,"_years"))
#   
#   return(list(fleet = p_fleet, region = p_region, global = p_global))
# }

plot_fbar_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
                                  width = 10, height = 7, dpi = 300, col.opt = "D",
                                  f.ymin = NULL, f.ymax = NULL,
                                  new_model_names = NULL,
                                  use.n.years = NULL,
                                  base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  make_plot_data <- function(index_range, label_prefix) {
    if (!is.nsim) {
      Years <- mods[[1]]$om$years
      n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
      n_regions <- mods[[1]]$om$input$data$n_regions[1]
      lapply(seq_along(mods), function(i) {
        tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year <- Years
        tmp$Realization <- 1
        tmp <- tail(tmp, use.n.years)
      }) %>% bind_rows()
    } else {
      Years <- mods[[1]][[1]]$om$years
      lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp <- as.data.frame(tmp)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year <- Years
          tmp$Realization <- r
          tmp <- tail(tmp, use.n.years)
        }) %>% bind_rows()
      }) %>% bind_rows()
    }
  }
  
  if (!is.nsim) {
    # Get number of fleets and regions
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }
  
  # Fleet-level Fbar
  res_fleet <- make_plot_data(1:n_fleets, "Fleet_")
  
  # Region-level Fbar
  res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
  
  # Global Fbar (only one column)
  res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")
  
  plot_and_save <- function(res, title, ylab_text, filename) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(res$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      res$Model <- factor(res$Model,
                          levels = paste0("Model", seq_along(new_model_names)),
                          labels = new_model_names)
      if (!is.null(base.model)) {
        if (!(base.model %in% new_model_names)) {
          warning("base.model does not match any of the new_model_names.")
        }
      }
    }
    
    res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")
    
    # Apply relative comparison if requested
    if (!is.null(base.model)) {
      base_df <- res_long %>% filter(Model == base.model) %>%
        rename(base_val = Fbar) %>%
        select(Realization, Year, Label, base_val)
      
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(Fbar = Fbar / base_val - 1)
    }
    
    if (!is.null(base.model)) {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = -1
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    } else {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = 0
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    }
    
    p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      coord_cartesian(ylim = c(y1, y2)) + 
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(paste0(ifelse(is.null(base.model), title, paste0("Relative ", title, " vs ", base.model)),
                     ": Last ", use.n.years, " Years")) +
      ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference in Fbar")) +
      theme_bw()
    
    plot_name <- paste0(filename, ifelse(is.null(base.model), "", "_rel"), ".PNG")
    ggsave(file.path(main.dir, sub.dir, plot_name), p, width = width, height = height, dpi = dpi)
    
    return(p)
  }
  
  p_fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet_last_", use.n.years, "_years"))
  p_region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region_last_", use.n.years, "_years"))
  p_global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global_last_", use.n.years, "_years"))
  
  return(list(fleet = p_fleet, region = p_region, global = p_global))
}

# plot_fbar_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
#                                    width = 10, height = 7, dpi = 300, col.opt = "D",
#                                    new_model_names = NULL,
#                                    use.n.years = NULL,
#                                    start.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   if (is.null(start.years)) cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
#   
#   make_plot_data <- function(index_range, label_prefix) {
#     
#     if (!is.nsim) {
#       
#       Years = mods[[1]]$om$years
#       
#       # Only one realization
#       lapply(seq_along(mods), function(i) {
#         tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
#         tmp <- as.data.frame(tmp)
#         names(tmp) <- paste0(label_prefix, seq_along(index_range))
#         tmp$Model <- paste0("Model", i)
#         tmp$Year <- Years
#         tmp$Realization <- 1
#         
#         start.row <- if (is.null(start.years)) 1 else start.years
#         n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#         
#         if (!is.null(start.row)) {
#           # Custom start row slicing
#           start_idx <- start.row
#           end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#           tmp <- tmp[start_idx:end_idx, ]
#         }
#         
#       }) %>% bind_rows()
#     } else {
#       
#       Years = mods[[1]][[1]]$om$years
#       
#       # Multiple realizations
#       lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
#           tmp <- as.data.frame(tmp)
#           names(tmp) <- paste0(label_prefix, seq_along(index_range))
#           tmp$Model <- paste0("Model", m)
#           tmp$Year <- Years
#           tmp$Realization <- r
#           
#           start.row <- if (is.null(start.years)) 1 else start.years
#           n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#           
#           if (!is.null(start.row)) {
#             # Custom start row slicing
#             start_idx <- start.row
#             end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#             tmp <- tmp[start_idx:end_idx, ]
#           }
#           
#         }) %>% bind_rows()
#       }) %>% bind_rows()
#     }
#   }
#   
#   if (!is.nsim) {
#     # Get number of fleets and regions
#     n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
#     n_regions <- mods[[1]]$om$input$data$n_regions[1]
#   } else {
#     n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
#     n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
#   }
#   
#   # Fleet-level Fbar
#   res_fleet <- make_plot_data(1:n_fleets, "Fleet_")
#   
#   # Region-level Fbar
#   res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
#   
#   # Global Fbar (only one column)
#   res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")
#   
#   # Function to handle renaming and plotting
#   plot_and_save <- function(res, title, ylab_text, filename) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(res$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       res$Model <- factor(res$Model,
#                           levels = paste0("Model", seq_along(new_model_names)),
#                           labels = new_model_names)
#     }
#     
#     res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                              names_to = "Label", values_to = "Fbar")
#     
#     p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   # Create and save each plot
#   p_fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet_first_",use.n.years,"_years"))
#   p_region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region_first_",use.n.years,"_years"))
#   p_global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global_first_",use.n.years,"_years"))
#   
#   return(list(fleet = p_fleet, region = p_region, global = p_global))
# }

plot_fbar_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
                                   width = 10, height = 7, dpi = 300, col.opt = "D",
                                   f.ymin = NULL, f.ymax = NULL, 
                                   new_model_names = NULL,
                                   use.n.years = NULL,
                                   start.years = NULL,
                                   base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  make_plot_data <- function(index_range, label_prefix) {
    
    if (!is.nsim) {
      Years <- mods[[1]]$om$years
      lapply(seq_along(mods), function(i) {
        tmp <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp <- as.data.frame(tmp)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year <- Years
        tmp$Realization <- 1
        
        start_idx <- start.years
        end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
        tmp[start_idx:end_idx, ]
      }) %>% bind_rows()
    } else {
      Years <- mods[[1]][[1]]$om$years
      lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp <- as.data.frame(tmp)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year <- Years
          tmp$Realization <- r
          
          start_idx <- start.years
          end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
          tmp[start_idx:end_idx, ]
        }) %>% bind_rows()
      }) %>% bind_rows()
    }
  }
  
  # Get fleet and region info
  if (!is.nsim) {
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }
  
  res_fleet <- make_plot_data(1:n_fleets, "Fleet_")
  res_region <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
  res_global <- make_plot_data(n_fleets + n_regions + 1, "Global")
  
  plot_and_save <- function(res, title, ylab_text, filename) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(res$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      res$Model <- factor(res$Model,
                          levels = paste0("Model", seq_along(new_model_names)),
                          labels = new_model_names)
      # if (!is.null(base.model)) base.model <- new_model_names[base.model]
      if (!is.null(base.model)) {
        if (!(base.model %in% new_model_names)) {
          warning("base.model does not match any of the new_model_names.")
        }
      }
    }
    
    res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")
    
    # Apply relative comparison
    if (!is.null(base.model)) {
      base_df <- res_long %>%
        filter(Model == base.model) %>%
        rename(base_val = Fbar) %>%
        select(Realization, Year, Label, base_val)
      
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(Fbar = Fbar / base_val - 1)
    }
    
    if (!is.null(base.model)) {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = -1
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    } else {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = 0
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    }
      
    p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      coord_cartesian(ylim = c(y1, y2)) + 
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(paste0(ifelse(is.null(base.model), title, paste0("Relative ", title, " vs ", base.model)),
                     ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
      ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference in Fbar")) +
      theme_bw()
    
    plot_name <- paste0(filename, ifelse(is.null(base.model), "", "_rel"), ".PNG")
    ggsave(file.path(main.dir, sub.dir, plot_name), p, width = width, height = height, dpi = dpi)
    
    return(p)
  }
  
  p_fleet <- plot_and_save(res_fleet, "Fbar by Fleet", "Fbar", paste0(var, "_fleet_first_", use.n.years, "_years"))
  p_region <- plot_and_save(res_region, "Fbar by Region", "Fbar", paste0(var, "_region_first_", use.n.years, "_years"))
  p_global <- plot_and_save(res_global, "Global Fbar", "Fbar", paste0(var, "_global_first_", use.n.years, "_years"))
  
  return(list(fleet = p_fleet, region = p_region, global = p_global))
}

# plot_catch_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
#                                    width = 10, height = 7, dpi = 300, col.opt = "D",
#                                    new_model_names = NULL,
#                                    use.n.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   res <- NULL
#   
#   # Prepare data
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     # Only one realization
#     res <- lapply(seq_along(mods), function(i) {
#       data.frame(
#         Catch = mods[[i]]$om$rep$pred_catch,
#         Model = paste0("Model", i),
#         Year = Years,
#         Realization = 1
#       ) %>% tail(use.n.years)
#     }) %>% bind_rows()
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     # Multiple realizations
#     res <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         data.frame(
#           Catch = mods[[r]][[m]]$om$rep$pred_catch,
#           Model = paste0("Model", m),
#           Year = Years,
#           Realization = r
#         ) %>% tail(use.n.years)
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#   }
#   
#   # Allow renaming Model names
#   if (!is.null(new_model_names)) {
#     if (length(new_model_names) != length(unique(res$Model))) {
#       stop("Length of new_model_names must match the number of models.")
#     }
#     res$Model <- factor(res$Model,
#                         levels = paste0("Model", seq_along(new_model_names)),
#                         labels = new_model_names)
#   }
#   
#   # Pivot longer if needed (for multiple Catch types)
#   res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
#   
#   # Plot
#   p1 <- ggplot(res, aes(x = Model, y = !!sym(var), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var, "Last", use.n.years, "Years")) +
#     ylab(var) +
#     theme_bw()
#   
#   # Save the plot
#   ggsave(file.path(main.dir, sub.dir, paste0(var, "_last_",use.n.years,"_years.PNG")), p1, width = width, height = height, dpi = dpi)
#   
#   return(p1)
# }

plot_catch_performance <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
                                   width = 10, height = 7, dpi = 300, col.opt = "D",
                                   new_model_names = NULL,
                                   use.n.years = NULL,
                                   base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  res <- NULL
  
  # Prepare data
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    res <- lapply(seq_along(mods), function(i) {
      data.frame(
        Catch = mods[[i]]$om$rep$pred_catch,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1
      ) %>% tail(use.n.years)
    }) %>% bind_rows()
  } else {
    Years <- mods[[1]][[1]]$om$years
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        data.frame(
          Catch = mods[[r]][[m]]$om$rep$pred_catch,
          Model = paste0("Model", m),
          Year = Years,
          Realization = r
        ) %>% tail(use.n.years)
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models if specified
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  # Pivot longer if needed (for multiple Catch columns)
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
  
  # Relative difference from base.model
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = !!sym(var)) %>%
      select(Realization, Year, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(!!var := (!!sym(var)) / base_val - 1) 
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = Catch, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0(ifelse(is.null(base.model), var, paste0("Relative ", var, " vs ", base.model)),
                   ": Last ", use.n.years, " Years")) +
    ylab(ifelse(is.null(base.model), var, paste0("Relative ", var, " Difference"))) +
    theme_bw()
  
  plot_name <- paste0(var, ifelse(is.null(base.model), "", "_rel"), "_last_", use.n.years, "_years.PNG")
  ggsave(file.path(main.dir, sub.dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}

# plot_catch_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
#                                     width = 10, height = 7, dpi = 300, col.opt = "D",
#                                     new_model_names = NULL,
#                                     use.n.years = NULL,
#                                     start.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#   if (is.null(use.n.years)) use.n.years = 5
#   
#   if (is.null(start.years)) cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
#   
#   res <- NULL
#   
#   # Prepare data
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     # Only one realization
#     res <- lapply(seq_along(mods), function(i) {
#       tmp <- data.frame(
#         Catch = mods[[i]]$om$rep$pred_catch,
#         Model = paste0("Model", i),
#         Year = Years,
#         Realization = 1
#       ) 
#       start.row <- if (is.null(start.years)) 1 else start.years
#       n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#       
#       if (!is.null(start.row)) {
#         # Custom start row slicing
#         start_idx <- start.row
#         end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#         tmp <- tmp[start_idx:end_idx, ]
#       }
#     }) %>% bind_rows()
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     # Multiple realizations
#     res <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         tmp <- data.frame(
#           Catch = mods[[r]][[m]]$om$rep$pred_catch,
#           Model = paste0("Model", m),
#           Year = Years,
#           Realization = r
#         )
#         start.row <- if (is.null(start.years)) 1 else start.years
#         n_rows <- if (is.null(use.n.years)) 5 else use.n.years
#         
#         if (!is.null(start.row)) {
#           # Custom start row slicing
#           start_idx <- start.row
#           end_idx <- min(start.row + n_rows - 1, nrow(tmp))  # make sure don't go beyond available rows
#           tmp <- tmp[start_idx:end_idx, ]
#         }
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#   }
#   
#   # Pivot longer if needed (for multiple Catch types)
#   res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
#   
#   # Plot
#   p1 <- ggplot(res, aes(x = Model, y = !!sym(var), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var, "First", use.n.years, "Years")) +
#     ylab(var) +
#     theme_bw()
#   
#   # Save the plot
#   ggsave(file.path(main.dir, sub.dir, paste0(var, "_first_",use.n.years,"_years.PNG")), p1, width = width, height = height, dpi = dpi)
#   
#   return(p1)
# }

plot_catch_performance2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
                                    width = 10, height = 7, dpi = 300, col.opt = "D",
                                    new_model_names = NULL,
                                    use.n.years = NULL,
                                    start.years = NULL,
                                    base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  res <- NULL
  
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    res <- lapply(seq_along(mods), function(i) {
      tmp <- data.frame(
        Catch = mods[[i]]$om$rep$pred_catch,
        Model = paste0("Model", i),
        Year = Years,
        Realization = 1
      )
      start_idx <- start.years
      end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
      tmp[start_idx:end_idx, ]
    }) %>% bind_rows()
  } else {
    Years <- mods[[1]][[1]]$om$years
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        tmp <- data.frame(
          Catch = mods[[r]][[m]]$om$rep$pred_catch,
          Model = paste0("Model", m),
          Year = Years,
          Realization = r
        )
        start_idx <- start.years
        end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
        tmp[start_idx:end_idx, ]
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models if requested
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  # Pivot longer if multiple types
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = var)
  
  # Calculate relative difference if base.model is specified
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = !!sym(var)) %>%
      select(Realization, Year, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(!!var := (!!sym(var)) / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = Catch, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0(ifelse(is.null(base.model), var, paste0("Relative ", var, " vs ", base.model)),
                   ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
    ylab(ifelse(is.null(base.model), var, paste0("Relative ", var, " Difference"))) +
    theme_bw()
  
  # Save the plot
  plot_name <- paste0(var, ifelse(is.null(base.model), "", "_rel"),
                      "_first_", use.n.years, "_years.PNG")
  ggsave(file.path(main.dir, sub.dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}

# plot_ssb_status <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB_status",
#                             width = 10, height = 7, dpi = 300, col.opt = "D",
#                             new_model_names = NULL,
#                             use.n.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) {
#     cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#     use.n.years <- 5
#   }
#   
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     title_main = paste0("Probability SSB/SSB", mods[[1]]$om$input$data$percentSPR, "% < 0.5 (Last ", use.n.years, " Years)")
#     
#     # Only one realization
#     res_list <- lapply(seq_along(mods), function(i) {
#       tmp <- mods[[i]]$om$rep$SSB
#       tmp <- cbind(tmp, rowSums(tmp)) # Add total SSB across stocks/regions
#       tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR) # Normalize by SSB at Fspr
#       tmp <- as.data.frame(tmp)
#       
#       name_tmp <- paste0("SSB/SSB", mods[[i]]$om$input$data$percentSPR, "%")
#       names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
#       names(tmp)[ncol(tmp)] <- name_tmp # Last column is global one
#       
#       tmp$Model <- paste0("Model", i)
#       tmp$Year <- Years
#       tmp$Realization <- 1
#       
#       tmp <- tail(tmp, use.n.years)
#     }) 
#     
#     res <- bind_rows(res_list)
#     
#     # --- Calculate probabilities SSB/SSBSPR < 0.5 ---
#     prob <- lapply(seq_along(mods), function(i) {
#       x <- res_list[[i]]
#       y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
#       out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
#       out$Model <- paste0("Model", i)
#       return(out)
#     }) %>% bind_rows()
#     
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     title_main = paste0("Probability SSB/SSB", mods[[1]][[1]]$om$input$data$percentSPR, "% < 0.5 (Last ", use.n.years, " Years)")
#     
#     # Multiple realizations
#     res_list <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         
#         tmp <- mods[[r]][[m]]$om$rep$SSB
#         tmp <- cbind(tmp, rowSums(tmp)) # Add total SSB across stocks/regions
#         tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR) # Normalize by SSB at Fspr
#         tmp <- as.data.frame(tmp)
#         
#         name_tmp <- paste0("SSB/SSB", mods[[r]][[m]]$om$input$data$percentSPR, "%")
#         names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
#         names(tmp)[ncol(tmp)] <- name_tmp # Last column is global one
#         
#         tmp$Model <- paste0("Model", m)
#         tmp$Year <- Years
#         tmp$Realization <- r # default realization
#         
#         tmp <- tail(tmp, use.n.years)
#       })
#     })
#     
#     res <- bind_rows(res_list)
#     
#     # --- Calculate probabilities SSB/SSBSPR < 0.5 ---
#     prob <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         x <- res_list[[r]][[m]]
#         y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
#         out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
#         out$Model <- paste0("Model", m)
#         return(out)
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#     
#   }
#   
#   # --- Rename models if needed ---
#   rename_models <- function(df) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(df$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       df$Model <- factor(df$Model,
#                          levels = paste0("Model", seq_along(new_model_names)),
#                          labels = new_model_names)
#     }
#     df
#   }
#   
#   res <- rename_models(res)
#   prob <- rename_models(prob)
#   
#   # --- Boxplot of SSB status ---
#   var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
#   
#   res_long <- pivot_longer(res, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = var_name)
#   
#   p1 <- ggplot(res_long, aes(x = Model, y = !!rlang::sym(var_name), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var_name, "(Last", use.n.years, "Years)")) +
#     ylab(var_name) +
#     theme_bw()
#   
#   ggsave(file.path(main.dir, sub.dir, paste0("SSB_status_last_", use.n.years, "_years.PNG")),
#          p1, width = width, height = height, dpi = dpi)
#   
#   # --- Point plot of probability SSB/SSBSPR < 0.5 ---
#   prob_long <- pivot_longer(prob, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = "Prob")
#   
#   p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(title_main) +
#     ylab("Probability") +
#     theme_bw()
#   
#   ggsave(file.path(main.dir, sub.dir, paste0("SSB_status_overfishing_prob_last_", use.n.years, "_years.PNG")),
#          p2, width = width, height = height, dpi = dpi)
#   
#   return(list(
#     boxplot = p1,
#     probplot = p2
#   ))
# }

plot_ssb_status <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB_status",
                            width = 10, height = 7, dpi = 300, col.opt = "D",
                            new_model_names = NULL,
                            use.n.years = NULL,
                            base.model = NULL,
                            plot_prob = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    title_main <- paste0("Probability SSB/SSB", mods[[1]]$om$input$data$percentSPR, "% < 0.5: Last ", use.n.years, " Years")
    res_list <- lapply(seq_along(mods), function(i) {
      tmp <- mods[[i]]$om$rep$SSB
      tmp <- cbind(tmp, rowSums(tmp))
      tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR)
      tmp <- as.data.frame(tmp)
      name_tmp <- paste0("SSB/SSB", mods[[i]]$om$input$data$percentSPR, "%")
      names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
      names(tmp)[ncol(tmp)] <- name_tmp
      tmp$Model <- paste0("Model", i)
      tmp$Year <- Years
      tmp$Realization <- 1
      tail(tmp, use.n.years)
    })
    res <- bind_rows(res_list)
    prob <- lapply(seq_along(mods), function(i) {
      x <- res_list[[i]]
      y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
      out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
      out$Model <- paste0("Model", i)
      out
    }) %>% bind_rows()
  } else {
    Years <- mods[[1]][[1]]$om$years
    title_main <- paste0("Probability SSB/SSB", mods[[1]][[1]]$om$input$data$percentSPR, "% < 0.5: Last ", use.n.years, " Years")
    res_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        tmp <- mods[[r]][[m]]$om$rep$SSB
        tmp <- cbind(tmp, rowSums(tmp))
        tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR)
        tmp <- as.data.frame(tmp)
        name_tmp <- paste0("SSB/SSB", mods[[r]][[m]]$om$input$data$percentSPR, "%")
        names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
        names(tmp)[ncol(tmp)] <- name_tmp
        tmp$Model <- paste0("Model", m)
        tmp$Year <- Years
        tmp$Realization <- r
        tail(tmp, use.n.years)
      })
    })
    res <- bind_rows(res_list)
    prob <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        x <- res_list[[r]][[m]]
        y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
        out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
        out$Model <- paste0("Model", m)
        out
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models
  rename_models <- function(df) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(df$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      df$Model <- factor(df$Model,
                         levels = paste0("Model", seq_along(new_model_names)),
                         labels = new_model_names)
      # if (!is.null(base.model)) base.model <<- new_model_names[base.model]
      if (!is.null(base.model)) {
        if (!(base.model %in% new_model_names)) {
          warning("base.model does not match any of the new_model_names.")
        }
      }
    }
    df
  }
  
  res <- rename_models(res)
  prob <- rename_models(prob)
  
  # Boxplot
  
  # Get the variable name pattern
  var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
  
  # Pivot to 'value' column to avoid dynamic naming issues
  res_long <- pivot_longer(
    res,
    cols = matches("^SSB[./]SSB"),
    names_to = "Label",
    values_to = "value"
  )
  
  # Relative difference if base.model provided
  if (!is.null(base.model)) {
    base_df <- res_long %>%
      filter(Model == base.model) %>%
      rename(base_val = value) %>%
      select(Realization, Year, Label, base_val)
    
    res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(value = value / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res_long, aes(x = Model, y = value, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(ifelse(is.null(base.model),
                   paste0(var_name, ": Last ", use.n.years, " Years"),
                   paste0("Relative ", var_name, " vs ", base.model, ": Last ", use.n.years, " Years"))) +
    ylab(ifelse(is.null(base.model),
                var_name,
                paste0("Relative ", var_name, " Difference"))) +
    theme_bw()
  
  # Save plot
  ggsave(file.path(main.dir, sub.dir, paste0(
    "SSB_status",
    ifelse(is.null(base.model), "", "_rel"),
    "_last_", use.n.years, "_years.PNG"
  )),
  p1, width = width, height = height, dpi = dpi)
  
  # Probability plot (not affected by base.model)
  prob_long <- pivot_longer(prob, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = "Prob")
  
  if(plot_prob) {
    p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title_main) +
      ylab("Probability") +
      theme_bw()
    
    ggsave(file.path(main.dir, sub.dir,
                     paste0("SSB_status_overfished_prob_last_", use.n.years, "_years.PNG")),
           p2, width = width, height = height, dpi = dpi)
  } else {
    p2 <- NULL
  }
  
  return(list(
    boxplot = p1,
    probplot = p2
  ))
}

# plot_ssb_status2 <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB_status",
#                              width = 10, height = 7, dpi = 300, col.opt = "D",
#                              new_model_names = NULL,
#                              use.n.years = NULL,
#                              start.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) {
#     cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#     use.n.years <- 5
#   }
#   
#   if (is.null(start.years)) {
#     cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
#     start.years <- 1
#   }
#   
#   if (!is.nsim) {
#     
#     Years = mods[[1]]$om$years
#     
#     title_main = paste0("Probability SSB/SSB", mods[[1]]$om$input$data$percentSPR, "% < 0.5 (Last ", use.n.years, " Years)")
#     
#     # Only one realization
#     res_list <- lapply(seq_along(mods), function(i) {
#       tmp <- mods[[i]]$om$rep$SSB
#       tmp <- cbind(tmp, rowSums(tmp)) # Add total SSB across stocks/regions
#       tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR) # Normalize by SSB at Fspr
#       tmp <- as.data.frame(tmp)
#       
#       name_tmp <- paste0("SSB/SSB", mods[[i]]$om$input$data$percentSPR, "%")
#       names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
#       names(tmp)[ncol(tmp)] <- name_tmp # Last column is global one
#       
#       tmp$Model <- paste0("Model", i)
#       tmp$Year <- Years
#       tmp$Realization <- 1
#       
#       start_idx <- start.years
#       end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
#       tmp <- tmp[start_idx:end_idx, ]
#       
#     }) 
#     
#     res <- bind_rows(res_list)
#     
#     # --- Calculate probabilities SSB/SSBSPR < 0.5 ---
#     prob <- lapply(seq_along(mods), function(i) {
#       x <- res_list[[i]]
#       y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
#       out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
#       out$Model <- paste0("Model", i)
#       return(out)
#     }) %>% bind_rows()
#     
#   } else {
#     
#     Years = mods[[1]][[1]]$om$years
#     
#     title_main = paste0("Probability SSB/SSB", mods[[1]][[1]]$om$input$data$percentSPR, "% < 0.5 (Last ", use.n.years, " Years)")
#     
#     # Multiple realizations
#     res_list <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         
#         tmp <- mods[[r]][[m]]$om$rep$SSB
#         tmp <- cbind(tmp, rowSums(tmp)) # Add total SSB across stocks/regions
#         tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR) # Normalize by SSB at Fspr
#         tmp <- as.data.frame(tmp)
#         
#         name_tmp <- paste0("SSB/SSB", mods[[r]][[m]]$om$input$data$percentSPR, "%")
#         names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
#         names(tmp)[ncol(tmp)] <- name_tmp # Last column is global one
#         
#         tmp$Model <- paste0("Model", m)
#         tmp$Year <- Years
#         tmp$Realization <- r # default realization
#         
#         start_idx <- start.years
#         end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
#         tmp <- tmp[start_idx:end_idx, ]
#       })
#     })
#     
#     res <- bind_rows(res_list)
#     
#     # --- Calculate probabilities SSB/SSBSPR < 0.5 ---
#     prob <- lapply(seq_along(mods), function(r) {
#       lapply(seq_along(mods[[r]]), function(m) {
#         x <- res_list[[r]][[m]]
#         y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
#         out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
#         out$Model <- paste0("Model", m)
#         return(out)
#       }) %>% bind_rows()
#     }) %>% bind_rows()
#     
#   }
#   
#   # --- Rename models if needed ---
#   rename_models <- function(df) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(df$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       df$Model <- factor(df$Model,
#                          levels = paste0("Model", seq_along(new_model_names)),
#                          labels = new_model_names)
#     }
#     df
#   }
#   
#   res <- rename_models(res)
#   prob <- rename_models(prob)
#   
#   # --- Boxplot of SSB status ---
#   var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
#   
#   res_long <- pivot_longer(res, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = var_name)
#   
#   p1 <- ggplot(res_long, aes(x = Model, y = !!rlang::sym(var_name), color = Model)) +
#     geom_boxplot(outlier.shape = NA) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(paste(var_name, "(First", use.n.years, "Years)")) +
#     ylab(var_name) +
#     theme_bw()
#   
#   ggsave(file.path(main.dir, sub.dir, paste0("SSB_status_first_", use.n.years, "_years.PNG")),
#          p1, width = width, height = height, dpi = dpi)
#   
#   # --- Point plot of probability SSB/SSBSPR < 0.5 ---
#   prob_long <- pivot_longer(prob, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = "Prob")
#   
#   p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
#     geom_point(size = 3) +
#     facet_grid(Label ~ ., scales = "free") +
#     scale_color_viridis_d(option = col.opt) +
#     ggtitle(title_main) +
#     ylab("Probability") +
#     theme_bw()
#   
#   ggsave(file.path(main.dir, sub.dir, paste0("SSB_status_overfishing_prob_first_", use.n.years, "_years.PNG")),
#          p2, width = width, height = height, dpi = dpi)
#   
#   return(list(
#     boxplot = p1,
#     probplot = p2
#   ))
# }

plot_ssb_status2 <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB_status",
                             width = 10, height = 7, dpi = 300, col.opt = "D",
                             new_model_names = NULL,
                             use.n.years = NULL,
                             start.years = NULL,
                             base.model = NULL,
                             plot_prob = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    title_main <- paste0("Probability SSB/SSB", mods[[1]]$om$input$data$percentSPR, "% < 0.5: Years ", start.years, " to ", start.years + use.n.years - 1) 
    res_list <- lapply(seq_along(mods), function(i) {
      tmp <- mods[[i]]$om$rep$SSB
      tmp <- cbind(tmp, rowSums(tmp))
      tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR)
      tmp <- as.data.frame(tmp)
      name_tmp <- paste0("SSB/SSB", mods[[i]]$om$input$data$percentSPR, "%")
      names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
      names(tmp)[ncol(tmp)] <- name_tmp
      tmp$Model <- paste0("Model", i)
      tmp$Year <- Years
      tmp$Realization <- 1
      tmp[start.years:min(start.years + use.n.years - 1, nrow(tmp)), ]
    })
    res <- bind_rows(res_list)
    prob <- lapply(seq_along(mods), function(i) {
      x <- res_list[[i]]
      y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
      out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
      out$Model <- paste0("Model", i)
      out
    }) %>% bind_rows()
  } else {
    Years <- mods[[1]][[1]]$om$years
    title_main <- paste0("Probability SSB/SSB", mods[[1]][[1]]$om$input$data$percentSPR, "% < 0.5: Years ", start.years, " to ", start.years + use.n.years - 1)
    res_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        tmp <- mods[[r]][[m]]$om$rep$SSB
        tmp <- cbind(tmp, rowSums(tmp))
        tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR)
        tmp <- as.data.frame(tmp)
        name_tmp <- paste0("SSB/SSB", mods[[r]][[m]]$om$input$data$percentSPR, "%")
        names(tmp) <- paste0(name_tmp, ".s", 1:ncol(tmp))
        names(tmp)[ncol(tmp)] <- name_tmp
        tmp$Model <- paste0("Model", m)
        tmp$Year <- Years
        tmp$Realization <- r
        tmp[start.years:min(start.years + use.n.years - 1, nrow(tmp)), ]
      })
    })
    res <- bind_rows(res_list)
    prob <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        x <- res_list[[r]][[m]]
        y <- x[, grepl("^SSB/SSB", names(x)), drop = FALSE]
        out <- as.data.frame(t(colMeans(y < 0.5, na.rm = TRUE)))
        out$Model <- paste0("Model", m)
        out
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # --- Rename models if needed ---
  rename_models <- function(df) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(df$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      df$Model <- factor(df$Model,
                         levels = paste0("Model", seq_along(new_model_names)),
                         labels = new_model_names)
      # if (!is.null(base.model)) base.model <<- new_model_names[base.model]
      if (!is.null(base.model)) {
        if (!(base.model %in% new_model_names)) {
          warning("base.model does not match any of the new_model_names.")
        }
      }
    }
    df
  }
  
  res <- rename_models(res)
  prob <- rename_models(prob)
  
  # Boxplot of SSB status
  # var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
  # res_long <- pivot_longer(res, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = var_name)
  # 
  # if (!is.null(base.model)) {
  #   base_df <- res_long %>%
  #     filter(Model == base.model) %>%
  #     rename(base_val = !!sym(var_name)) %>%
  #     select(Realization, Year, Label, base_val)
  #   
  #   res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
  #     mutate(!!var_name := (!!sym(var_name)) / base_val - 1)
  # }
  # 
  # p1 <- ggplot(res_long, aes(x = Model, y = .data[[var_name]], color = Model)) +
  #   geom_boxplot(outlier.shape = NA) +
  #   facet_grid(Label ~ ., scales = "free") +
  #   scale_color_viridis_d(option = col.opt) +
  #   ggtitle(paste0(ifelse(is.null(base.model), var_name, paste0("Relative ", var_name, " vs ", base.model)),
  #                  ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
  #   ylab(ifelse(is.null(base.model),
  #               var_name,
  #               paste0("Relative ", var_name, " Difference"))) +
  #   theme_bw()
  
  # Get the variable name pattern
  var_name <- unique(gsub("\\.s\\d+", "", grep("^SSB[./]SSB", names(res), value = TRUE)))
  
  # Pivot to 'value' column to avoid dynamic naming issues
  res_long <- pivot_longer(
    res,
    cols = matches("^SSB[./]SSB"),
    names_to = "Label",
    values_to = "value"
  )
  
  # Relative difference if base.model provided
  if (!is.null(base.model)) {
    base_df <- res_long %>%
      filter(Model == base.model) %>%
      rename(base_val = value) %>%
      select(Realization, Year, Label, base_val)
    
    res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
      mutate(value = value / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res_long, aes(x = Model, y = value, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0(ifelse(is.null(base.model), var_name, paste0("Relative ", var_name, " vs ", base.model)),
                   ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
    ylab(ifelse(is.null(base.model),
                var_name,
                paste0("Relative ", var_name, " Difference"))) +
    theme_bw()
  
  # Save plot
  
  ggsave(file.path(main.dir, sub.dir,
                   paste0("SSB_status", ifelse(is.null(base.model), "", "_rel"),
                          "_first_", use.n.years, "_years.PNG")),
         p1, width = width, height = height, dpi = dpi)
  
  # Point plot of probability SSB/SSBSPR < 0.5 (unchanged)
  prob_long <- pivot_longer(prob, cols = matches("^SSB[./]SSB"), names_to = "Label", values_to = "Prob")
  
  if(plot_prob) {
    p2 <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title_main) +
      ylab("Probability") +
      theme_bw()
    
    ggsave(file.path(main.dir, sub.dir,
                     paste0("SSB_status_overfished_prob_first_", use.n.years, "_years.PNG")),
           p2, width = width, height = height, dpi = dpi)
  } else {
    p2 = NULL
  }
  
  return(list(
    boxplot = p1,
    probplot = p2
  ))
}

# plot_fbar_status <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar_status",
#                              width = 10, height = 7, dpi = 300, col.opt = "D",
#                              new_model_names = NULL,
#                              use.n.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) {
#     cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#     use.n.years <- 5
#   }
#   
#   make_plot_data <- function(index_range, label_prefix) {
#     
#     if (!is.nsim) {
#       
#       Years = mods[[1]]$om$years
#       
#       # Only one realization
#       
#       res_list <- lapply(seq_along(mods), function(i) {
#         tmp1 <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
#         tmp2 <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
#         tmp <- as.data.frame(tmp1 / tmp2)
#         names(tmp) <- paste0(label_prefix, seq_along(index_range))
#         tmp$Model <- paste0("Model", i)
#         tmp$Year <- Years
#         tmp$Realization <- 1
#         
#         tmp <- tail(tmp, use.n.years)
#       }) 
#       
#       res <- bind_rows(res_list)
#       
#       # Calculate overfishing probability (>1)
#       prob <- lapply(res_list, function(x) {
#         y <- x[, 1:length(index_range), drop = FALSE]
#         data.frame(t(colMeans(y > 1, na.rm = TRUE)))
#       }) %>%
#         bind_rows() %>%
#         mutate(Model = paste0("Model", seq_along(mods)))
#       
#       return(list(data = res, prob = prob))
#       
#     } else {
#       
#       Years = mods[[1]][[1]]$om$years
#       
#       # Multiple realizations
#       
#       res_list <- lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           
#           tmp1 <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
#           tmp2 <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
#           tmp <- as.data.frame(tmp1 / tmp2)
#           names(tmp) <- paste0(label_prefix, seq_along(index_range))
#           tmp$Model <- paste0("Model", m)
#           tmp$Year <- Years
#           tmp$Realization <- r
#           
#           tmp <- tail(tmp, use.n.years)
#         })
#       })
#       
#       res <- bind_rows(res_list)
#       
#       prob <- lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           x <- res_list[[r]][[m]]
#           y <- x[, 1:length(index_range), drop = FALSE]
#           df <- data.frame(t(colMeans(y > 1, na.rm = TRUE)))
#           df$Model <- paste0("Model",m)
#           df
#         }) %>% bind_rows()
#       }) %>% bind_rows()
#       
#       return(list(data = res, prob = prob)) 
#     }
#   }
#   
#   if (!is.nsim) {
#     title_main <- paste0("F/F", mods[[1]]$om$input$data$percentSPR, "%")
#   } else {
#     title_main <- paste0("F/F", mods[[1]][[1]]$om$input$data$percentSPR, "%")
#   }
#   
#   # Prepare data
#   res_fleet_all <- make_plot_data(1:n_fleets, "Fleet_")
#   res_fleet <- res_fleet_all$data
#   prob_fleet <- res_fleet_all$prob
#   
#   res_region_all <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
#   res_region <- res_region_all$data
#   prob_region <- res_region_all$prob
#   
#   res_global_all <- make_plot_data(n_fleets + n_regions + 1, "Global")
#   res_global <- res_global_all$data
#   prob_global <- res_global_all$prob
#   
#   # Helper to rename models
#   rename_models <- function(res_or_prob) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(res_or_prob$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       res_or_prob$Model <- factor(res_or_prob$Model,
#                                   levels = paste0("Model", seq_along(new_model_names)),
#                                   labels = new_model_names)
#     }
#     return(res_or_prob)
#   }
#   
#   res_fleet <- rename_models(res_fleet)
#   res_region <- rename_models(res_region)
#   res_global <- rename_models(res_global)
#   
#   prob_fleet <- rename_models(prob_fleet)
#   prob_region <- rename_models(prob_region)
#   prob_global <- rename_models(prob_global)
#   
#   # Plot function for boxplot
#   plot_boxplot <- function(res, title, ylab_text, filename) {
#     res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                              names_to = "Label", values_to = "Fbar")
#     
#     p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   # Plot function for point plot (probabilities)
#   plot_pointplot <- function(prob, title, ylab_text, filename) {
#     prob_long <- pivot_longer(prob, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                               names_to = "Label", values_to = "Prob")
#     
#     p <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   # Create and save all plots
#   p_fleet_box <- plot_boxplot(res_fleet, paste(title_main, "by Fleet","(Last",use.n.years, "Years)"), title_main, paste0(var, "_fleet_last_", use.n.years, "_years"))
#   p_region_box <- plot_boxplot(res_region, paste(title_main, "by Region","(Last",use.n.years, "Years)"), title_main, paste0(var, "_region_last_", use.n.years, "_years"))
#   p_global_box <- plot_boxplot(res_global, paste(title_main, "Global","(Last",use.n.years, "Years)"), title_main, paste0(var, "_global_last_", use.n.years, "_years"))
#   
#   p_fleet_point <- plot_pointplot(prob_fleet, paste0("Probability (",title_main,"> 1) - Fleet","(Last",use.n.years, "Years)"), "Probability", paste0(var, "_fleet_overfishing_prob","_fleet_last_", use.n.years, "_years"))
#   p_region_point <- plot_pointplot(prob_region, paste0("Probability (",title_main,"> 1) - Region","(Last",use.n.years, "Years)"), "Probability", paste0(var, "_region_overfishing_prob","_fleet_last_", use.n.years, "_years"))
#   p_global_point <- plot_pointplot(prob_global, paste0("Probability (",title_main,"> 1) - Global","(Last",use.n.years, "Years)"), "Probability", paste0(var, "_global_overfishing_prob","_fleet_last_", use.n.years, "_years"))
#   
#   return(list(
#     fleet_box = p_fleet_box,
#     region_box = p_region_box,
#     global_box = p_global_box,
#     fleet_prob = p_fleet_point,
#     region_prob = p_region_point,
#     global_prob = p_global_point
#   ))
# }

plot_fbar_status <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar_status",
                             width = 10, height = 7, dpi = 300, col.opt = "D",
                             f.ymin = NULL, f.ymax = NULL, 
                             new_model_names = NULL,
                             use.n.years = NULL,
                             base.model = NULL,
                             plot_prob = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  make_plot_data <- function(index_range, label_prefix) {
    if (!is.nsim) {
      Years = mods[[1]]$om$years
      res_list <- lapply(seq_along(mods), function(i) {
        tmp1 <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp2 <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
        tmp <- as.data.frame(tmp1 / tmp2)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year <- Years
        tmp$Realization <- 1
        tmp <- tail(tmp, use.n.years)
      })
      res <- bind_rows(res_list)
      prob <- lapply(res_list, function(x) {
        y <- x[, 1:length(index_range), drop = FALSE]
        data.frame(t(colMeans(y > 1, na.rm = TRUE)))
      }) %>% bind_rows() %>% mutate(Model = paste0("Model", seq_along(mods)))
      return(list(data = res, prob = prob))
    } else {
      Years = mods[[1]][[1]]$om$years
      res_list <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp1 <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp2 <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
          tmp <- as.data.frame(tmp1 / tmp2)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year <- Years
          tmp$Realization <- r
          tmp <- tail(tmp, use.n.years)
        })
      })
      res <- bind_rows(res_list)
      prob <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          x <- res_list[[r]][[m]]
          y <- x[, 1:length(index_range), drop = FALSE]
          df <- data.frame(t(colMeans(y > 1, na.rm = TRUE)))
          df$Model <- paste0("Model", m)
          df
        }) %>% bind_rows()
      }) %>% bind_rows()
      return(list(data = res, prob = prob))
    }
  }
  
  if (!is.nsim) {
    title_main <- paste0("F/F", mods[[1]]$om$input$data$percentSPR, "%")
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    title_main <- paste0("F/F", mods[[1]][[1]]$om$input$data$percentSPR, "%")
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }
  
  res_fleet_all <- make_plot_data(1:n_fleets, "Fleet_")
  res_region_all <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
  res_global_all <- make_plot_data(n_fleets + n_regions + 1, "Global")
  
  res_fleet <- res_fleet_all$data
  res_region <- res_region_all$data
  res_global <- res_global_all$data
  prob_fleet <- res_fleet_all$prob
  prob_region <- res_region_all$prob
  prob_global <- res_global_all$prob
  
  rename_models <- function(res_or_prob) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(res_or_prob$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      res_or_prob$Model <- factor(res_or_prob$Model,
                                  levels = paste0("Model", seq_along(new_model_names)),
                                  labels = new_model_names)
    }
    return(res_or_prob)
  }
  
  res_fleet <- rename_models(res_fleet)
  res_region <- rename_models(res_region)
  res_global <- rename_models(res_global)
  prob_fleet <- rename_models(prob_fleet)
  prob_region <- rename_models(prob_region)
  prob_global <- rename_models(prob_global)
  
  # if (!is.null(base.model) && !is.null(new_model_names)) {
  #   base.model <- new_model_names[base.model]
  # }
  
  if (!is.null(base.model) && !is.null(new_model_names)) {
    if (!(base.model %in% new_model_names)) {
      warning("base.model does not match any of the new_model_names.")
    }
  }
  
  plot_boxplot <- function(res, title, ylab_text, filename) {
    res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")
    if (!is.null(base.model)) {
      base_df <- res_long %>%
        filter(Model == base.model) %>%
        rename(base_val = Fbar) %>%
        select(Realization, Year, Label, base_val)
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(Fbar = Fbar / base_val - 1)
    }
    
    if (!is.null(base.model)) {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = -1
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    } else {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = 0
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    }
    
    p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      coord_cartesian(ylim = c(y1, y2)) + 
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(ifelse(is.null(base.model),
                     paste0(title, ": Last ", use.n.years, " Years"),
                     paste0("Relative ", title, " vs ", base.model, ": Last ", use.n.years, " Years"))) +
      ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference")) +
      theme_bw()
    ggsave(file.path(main.dir, sub.dir, paste0(filename, ifelse(is.null(base.model), "", "_rel"), ".PNG")),
           p, width = width, height = height, dpi = dpi)
    return(p)
  }
  
  plot_pointplot <- function(prob, title, ylab_text, filename) {
    prob_long <- pivot_longer(prob, cols = starts_with(c("Fleet_", "Region_", "Global")),
                              names_to = "Label", values_to = "Prob")
    p <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title) +
      ylab(ylab_text) +
      theme_bw()
    ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
    return(p)
  }
  
  p_fleet_box <- plot_boxplot(res_fleet, paste(title_main, "by Fleet"), title_main, paste0(var, "_fleet_last_", use.n.years, "_years"))
  p_region_box <- plot_boxplot(res_region, paste(title_main, "by Region"), title_main, paste0(var, "_region_last_", use.n.years, "_years"))
  p_global_box <- plot_boxplot(res_global, paste(title_main, "Global"), title_main, paste0(var, "_global_last_", use.n.years, "_years"))
  
  if (plot_prob) {
    p_fleet_point <- plot_pointplot(prob_fleet, paste0("Probability (", title_main, " > 1) - Fleet: Last ", use.n.years, " Years"), "Probability", paste0(var, "_fleet_overfishing_prob_fleet_last_", use.n.years, "_years"))
    p_region_point <- plot_pointplot(prob_region, paste0("Probability (", title_main, " > 1) - Region: Last ", use.n.years, " Years"), "Probability", paste0(var, "_region_overfishing_prob_fleet_last_", use.n.years, "_years"))
    p_global_point <- plot_pointplot(prob_global, paste0("Probability (", title_main, " > 1) - Global: Last ", use.n.years, " Years"), "Probability", paste0(var, "_global_overfishing_prob_fleet_last_", use.n.years, "_years"))
  } else {
    p_fleet_point <- NULL
    p_region_point <- NULL
    p_global_point <- NULL
  }

  return(list(
    fleet_box = p_fleet_box,
    region_box = p_region_box,
    global_box = p_global_box,
    fleet_prob = p_fleet_point,
    region_prob = p_region_point,
    global_prob = p_global_point
  ))
}

# plot_fbar_status2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar_status",
#                               width = 10, height = 7, dpi = 300, col.opt = "D",
#                               new_model_names = NULL,
#                               use.n.years = NULL,
#                               start.years = NULL) {
#   
#   library(dplyr)
#   library(tidyr)
#   library(ggplot2)
#   
#   if (is.null(use.n.years)) {
#     cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
#     use.n.years <- 5
#   }
#   
#   if (is.null(start.years)) {
#     cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
#     start.years <- 1
#   }
#   
#   make_plot_data <- function(index_range, label_prefix) {
#     
#     if (!is.nsim) {
#       
#       Years = mods[[1]]$om$years
#       
#       # Get number of fleets and regions
#       n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
#       n_regions <- mods[[1]]$om$input$data$n_regions[1]
#       
#       # Only one realization
#       res_list <- lapply(seq_along(mods), function(i) {
#         tmp1 <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
#         tmp2 <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
#         tmp <- as.data.frame(tmp1 / tmp2)
#         names(tmp) <- paste0(label_prefix, seq_along(index_range))
#         tmp$Model <- paste0("Model", i)
#         tmp$Year <- Years
#         tmp$Realization <- 1
#         
#         start_idx <- start.years
#         end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
#         tmp <- tmp[start_idx:end_idx, ]
#       }) 
#       
#       res <- bind_rows(res_list)
#       
#       # Calculate overfishing probability (>1)
#       prob <- lapply(res_list, function(x) {
#         y <- x[, 1:length(index_range), drop = FALSE]
#         data.frame(t(colMeans(y > 1, na.rm = TRUE)))
#       }) %>%
#         bind_rows() %>%
#         mutate(Model = paste0("Model", seq_along(mods)))
#       
#       return(list(data = res, prob = prob))
#       
#     } else {
#       
#       Years = mods[[1]][[1]]$om$years
#       
#       title_main <- paste0("F/F", mods[[1]][[1]]$om$input$data$percentSPR, "%")
#       
#       # Get number of fleets and regions
#       n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
#       n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
#       
#       # Multiple realizations
#       res_list <- lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           
#           tmp1 <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
#           tmp2 <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
#           tmp <- as.data.frame(tmp1 / tmp2)
#           names(tmp) <- paste0(label_prefix, seq_along(index_range))
#           tmp$Model <- paste0("Model", m)
#           tmp$Year <- Years
#           tmp$Realization <- r
#           
#           start_idx <- start.years
#           end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
#           tmp <- tmp[start_idx:end_idx, ]
#         })
#       })
#       
#       res <- bind_rows(res_list)
#       
#       prob <- lapply(seq_along(mods), function(r) {
#         lapply(seq_along(mods[[r]]), function(m) {
#           x <- res_list[[r]][[m]]
#           y <- x[, 1:length(index_range), drop = FALSE]
#           df <- data.frame(t(colMeans(y > 1, na.rm = TRUE)))
#           df$Model <- paste0("Model",m)
#           df
#         }) %>% bind_rows()
#       }) %>% bind_rows()
#       
#       return(list(data = res, prob = prob)) 
#     }
#   }
# 
#   # Prepare data
#   res_fleet_all <- make_plot_data(1:n_fleets, "Fleet_")
#   res_fleet <- res_fleet_all$data
#   prob_fleet <- res_fleet_all$prob
#   
#   res_region_all <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
#   res_region <- res_region_all$data
#   prob_region <- res_region_all$prob
#   
#   res_global_all <- make_plot_data(n_fleets + n_regions + 1, "Global")
#   res_global <- res_global_all$data
#   prob_global <- res_global_all$prob
#   
#   # Helper to rename models
#   rename_models <- function(res_or_prob) {
#     if (!is.null(new_model_names)) {
#       if (length(new_model_names) != length(unique(res_or_prob$Model))) {
#         stop("Length of new_model_names must match the number of models.")
#       }
#       res_or_prob$Model <- factor(res_or_prob$Model,
#                                   levels = paste0("Model", seq_along(new_model_names)),
#                                   labels = new_model_names)
#     }
#     return(res_or_prob)
#   }
#   
#   res_fleet <- rename_models(res_fleet)
#   res_region <- rename_models(res_region)
#   res_global <- rename_models(res_global)
#   
#   prob_fleet <- rename_models(prob_fleet)
#   prob_region <- rename_models(prob_region)
#   prob_global <- rename_models(prob_global)
#   
#   # Plot function for boxplot
#   plot_boxplot <- function(res, title, ylab_text, filename) {
#     res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                              names_to = "Label", values_to = "Fbar")
#     
#     p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   # Plot function for point plot (probabilities)
#   plot_pointplot <- function(prob, title, ylab_text, filename) {
#     prob_long <- pivot_longer(prob, cols = starts_with(c("Fleet_", "Region_", "Global")),
#                               names_to = "Label", values_to = "Prob")
#     
#     p <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
#       geom_boxplot(outlier.shape = NA) +
#       facet_grid(Label ~ ., scales = "free") +
#       scale_color_viridis_d(option = col.opt) +
#       ggtitle(title) +
#       ylab(ylab_text) +
#       theme_bw()
#     
#     ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
#     return(p)
#   }
#   
#   if (!is.nsim) {
#     title_main <- paste0("F/F", mods[[1]]$om$input$data$percentSPR, "%")
#   } else {
#     title_main <- paste0("F/F", mods[[1]][[1]]$om$input$data$percentSPR, "%")
#   }
#   
#   # Create and save all plots
#   p_fleet_box <- plot_boxplot(res_fleet, paste(title_main, "by Fleet","(First",use.n.years, "Years)"), title_main, paste0(var, "_fleet_first_", use.n.years, "_years"))
#   p_region_box <- plot_boxplot(res_region, paste(title_main, "by Region","(First",use.n.years, "Years)"), title_main, paste0(var, "_region_first_", use.n.years, "_years"))
#   p_global_box <- plot_boxplot(res_global, paste(title_main, "Global","(First",use.n.years, "Years)"), title_main, paste0(var, "_global_first_", use.n.years, "_years"))
#   
#   p_fleet_point <- plot_pointplot(prob_fleet, paste0("Probability (",title_main,"> 1) - Fleet","(First",use.n.years, "Years)"), "Probability", paste0(var, "_fleet_overfishing_prob","_fleet_first_", use.n.years, "_years"))
#   p_region_point <- plot_pointplot(prob_region, paste0("Probability (",title_main,"> 1) - Region","(First",use.n.years, "Years)"), "Probability", paste0(var, "_region_overfishing_prob","_fleet_first_", use.n.years, "_years"))
#   p_global_point <- plot_pointplot(prob_global, paste0("Probability (",title_main,"> 1) - Global","(First",use.n.years, "Years)"), "Probability", paste0(var, "_global_overfishing_prob","_fleet_first_", use.n.years, "_years"))
#   
#   return(list(
#     fleet_box = p_fleet_box,
#     region_box = p_region_box,
#     global_box = p_global_box,
#     fleet_prob = p_fleet_point,
#     region_prob = p_region_point,
#     global_prob = p_global_point
#   ))
# }

plot_fbar_status2 <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar_status",
                              width = 10, height = 7, dpi = 300, col.opt = "D",
                              f.ymin = NULL, f.ymax = NULL, 
                              new_model_names = NULL,
                              use.n.years = NULL,
                              start.years = NULL,
                              base.model = NULL,
                              plot_prob = TRUE) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (5 years) is used here!\n")
    use.n.years <- 5
  }
  
  if (is.null(start.years)) {
    cat("\nstart.years is not specified, so default (1st year in historical period) is used here!\n")
    start.years <- 1
  }
  
  make_plot_data <- function(index_range, label_prefix) {
    if (!is.nsim) {
      Years = mods[[1]]$om$years
      res_list <- lapply(seq_along(mods), function(i) {
        tmp1 <- mods[[i]]$om$rep$Fbar[, index_range, drop = FALSE]
        tmp2 <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
        tmp <- as.data.frame(tmp1 / tmp2)
        names(tmp) <- paste0(label_prefix, seq_along(index_range))
        tmp$Model <- paste0("Model", i)
        tmp$Year <- Years
        tmp$Realization <- 1
        start_idx <- start.years
        end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
        tmp[start_idx:end_idx, ]
      })
      res <- bind_rows(res_list)
      prob <- lapply(res_list, function(x) {
        y <- x[, 1:length(index_range), drop = FALSE]
        data.frame(t(colMeans(y > 1, na.rm = TRUE)))
      }) %>% bind_rows() %>% mutate(Model = paste0("Model", seq_along(mods)))
      return(list(data = res, prob = prob))
    } else {
      Years = mods[[1]][[1]]$om$years
      res_list <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          tmp1 <- mods[[r]][[m]]$om$rep$Fbar[, index_range, drop = FALSE]
          tmp2 <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, index_range, drop = FALSE])
          tmp <- as.data.frame(tmp1 / tmp2)
          names(tmp) <- paste0(label_prefix, seq_along(index_range))
          tmp$Model <- paste0("Model", m)
          tmp$Year <- Years
          tmp$Realization <- r
          start_idx <- start.years
          end_idx <- min(start.years + use.n.years - 1, nrow(tmp))
          tmp[start_idx:end_idx, ]
        })
      })
      res <- bind_rows(res_list)
      prob <- lapply(seq_along(mods), function(r) {
        lapply(seq_along(mods[[r]]), function(m) {
          x <- res_list[[r]][[m]]
          y <- x[, 1:length(index_range), drop = FALSE]
          df <- data.frame(t(colMeans(y > 1, na.rm = TRUE)))
          df$Model <- paste0("Model", m)
          df
        }) %>% bind_rows()
      }) %>% bind_rows()
      return(list(data = res, prob = prob))
    }
  }
  
  if (!is.nsim) {
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
  } else {
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
  }
  
  res_fleet_all <- make_plot_data(1:n_fleets, "Fleet_")
  res_region_all <- make_plot_data((n_fleets + 1):(n_fleets + n_regions), "Region_")
  res_global_all <- make_plot_data(n_fleets + n_regions + 1, "Global")
  res_fleet <- res_fleet_all$data
  res_region <- res_region_all$data
  res_global <- res_global_all$data
  prob_fleet <- res_fleet_all$prob
  prob_region <- res_region_all$prob
  prob_global <- res_global_all$prob
  
  rename_models <- function(res_or_prob) {
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(res_or_prob$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      res_or_prob$Model <- factor(res_or_prob$Model,
                                  levels = paste0("Model", seq_along(new_model_names)),
                                  labels = new_model_names)
    }
    return(res_or_prob)
  }
  
  res_fleet <- rename_models(res_fleet)
  res_region <- rename_models(res_region)
  res_global <- rename_models(res_global)
  prob_fleet <- rename_models(prob_fleet)
  prob_region <- rename_models(prob_region)
  prob_global <- rename_models(prob_global)
  
  # if (!is.null(base.model) && !is.null(new_model_names)) {
  #   base.model <- new_model_names[base.model]
  # }
  
  if (!is.null(base.model) && !is.null(new_model_names)) {
    if (!(base.model %in% new_model_names)) {
      warning("base.model does not match any of the new_model_names.")
    }
  }
  
  plot_boxplot <- function(res, title, ylab_text, filename) {
    res_long <- pivot_longer(res, cols = starts_with(c("Fleet_", "Region_", "Global")),
                             names_to = "Label", values_to = "Fbar")
    if (!is.null(base.model)) {
      base_df <- res_long %>%
        filter(Model == base.model) %>%
        rename(base_val = Fbar) %>%
        select(Realization, Year, Label, base_val)
      res_long <- left_join(res_long, base_df, by = c("Realization", "Year", "Label")) %>%
        mutate(Fbar = Fbar / base_val - 1)
    }
    
    if (!is.null(base.model)) {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = -1
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    } else {
      if(!is.null(f.ymin)) y1 = f.ymin else y1 = 0
      if(!is.null(f.ymax)) y2 = f.ymax else y2 = 2
    }
    
    p <- ggplot(res_long, aes(x = Model, y = Fbar, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      coord_cartesian(ylim = c(y1, y2)) + 
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(paste0(ifelse(is.null(base.model), title, paste0("Relative ", title, " vs ", base.model)),
                     ": Years ", start.years, " to ", start.years + use.n.years - 1)) +
      ylab(ifelse(is.null(base.model), ylab_text, "Relative Difference")) +
      theme_bw()
    ggsave(file.path(main.dir, sub.dir, paste0(filename, ifelse(is.null(base.model), "", "_rel"), ".PNG")),
           p, width = width, height = height, dpi = dpi)
    return(p)
  }
  
  plot_pointplot <- function(prob, title, ylab_text, filename) {
    prob_long <- pivot_longer(prob, cols = starts_with(c("Fleet_", "Region_", "Global")),
                              names_to = "Label", values_to = "Prob")
    p <- ggplot(prob_long, aes(x = Model, y = Prob, color = Model)) +
      geom_boxplot(outlier.shape = NA) +
      facet_grid(Label ~ ., scales = "free") +
      scale_color_viridis_d(option = col.opt) +
      ggtitle(title) +
      ylab(ylab_text) +
      theme_bw()
    ggsave(file.path(main.dir, sub.dir, paste0(filename, ".PNG")), p, width = width, height = height, dpi = dpi)
    return(p)
  }
  
  if (!is.nsim) {
    title_main <- paste0("F/F", mods[[1]]$om$input$data$percentSPR, "%")
  } else {
    title_main <- paste0("F/F", mods[[1]][[1]]$om$input$data$percentSPR, "%")
  }
    
  p_fleet_box <- plot_boxplot(res_fleet, paste(title_main, "by Fleet"), title_main, paste0(var, "_fleet_first_", use.n.years, "_years"))
  p_region_box <- plot_boxplot(res_region, paste(title_main, "by Region"), title_main, paste0(var, "_region_first_", use.n.years, "_years"))
  p_global_box <- plot_boxplot(res_global, paste(title_main, "Global"), title_main, paste0(var, "_global_first_", use.n.years, "_years"))
  
  if (plot_prob) {
    p_fleet_point <- plot_pointplot(prob_fleet, paste0("Probability (", title_main, " > 1) - Fleet: Years ", start.years, " to ", start.years + use.n.years - 1), "Probability", paste0(var, "_fleet_overfishing_prob_fleet_first_", use.n.years, "_years"))
    p_region_point <- plot_pointplot(prob_region, paste0("Probability (", title_main, " > 1) - Region: Years ", start.years, " to ", start.years + use.n.years - 1), "Probability", paste0(var, "_region_overfishing_prob_fleet_first_", use.n.years, "_years"))
    p_global_point <- plot_pointplot(prob_global, paste0("Probability (", title_main, " > 1) - Global: Years ", start.years, " to ", start.years + use.n.years - 1), "Probability", paste0(var, "_global_overfishing_prob_fleet_first_", use.n.years, "_years"))
  } else {
    p_fleet_point <- NULL
    p_region_point <- NULL
    p_global_point <- NULL
  }

  return(list(
    fleet_box = p_fleet_box,
    region_box = p_region_box,
    global_box = p_global_box,
    fleet_prob = p_fleet_point,
    region_prob = p_region_point,
    global_prob = p_global_point
  ))
}

plot_kobe_status <- function(mods, is.nsim, main.dir, sub.dir, 
                             width = 10, height = 7, dpi = 300, col.opt = "D",
                             new_model_names = NULL,
                             use.n.years = NULL,
                             show_density = FALSE) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridis)
  
  if (is.null(use.n.years)) {
    cat("\nuse.n.years is not specified, so default (terminal year) is used here!\n")
    use.n.years <- 1
  }
  
  # === Extract SSB/SSBxx% ===
  if (!is.nsim) {
    Years <- mods[[1]]$om$years
    ssb_list <- lapply(seq_along(mods), function(i) {
      tmp <- mods[[i]]$om$rep$SSB
      tmp <- cbind(tmp, rowSums(tmp))
      tmp <- tmp / exp(mods[[i]]$om$rep$log_SSB_FXSPR)
      ssb <- tail(tmp[, ncol(tmp)], use.n.years)
      data.frame(Model = paste0("Model", i),
                 Realization = 1,
                 Year = tail(Years, use.n.years),
                 Overfished = ssb)
    })
    ssb_df <- bind_rows(ssb_list)
  } else {
    Years <- mods[[1]][[1]]$om$years
    ssb_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        tmp <- mods[[r]][[m]]$om$rep$SSB
        tmp <- cbind(tmp, rowSums(tmp))
        tmp <- tmp / exp(mods[[r]][[m]]$om$rep$log_SSB_FXSPR)
        ssb <- tail(tmp[, ncol(tmp)], use.n.years)
        data.frame(Model = paste0("Model", m),
                   Realization = r,
                   Year = tail(Years, use.n.years),
                   Overfished = ssb)
      })
    })
    ssb_df <- bind_rows(ssb_list)
  }
  
  # === Extract F/Fxx% ===
  if (!is.nsim) {
    fbar_list <- lapply(seq_along(mods), function(i) {
      fbar <- mods[[i]]$om$rep$Fbar[, ncol(mods[[i]]$om$rep$Fbar)]
      fbar_ref <- exp(mods[[i]]$om$rep$log_Fbar_XSPR[, ncol(mods[[i]]$om$rep$log_Fbar_XSPR)])
      ff <- tail(fbar / fbar_ref, use.n.years)
      data.frame(Model = paste0("Model", i),
                 Realization = 1,
                 Year = tail(Years, use.n.years),
                 Overfishing = ff)
    })
    fbar_df <- bind_rows(fbar_list)
  } else {
    fbar_list <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        fbar <- mods[[r]][[m]]$om$rep$Fbar[, ncol(mods[[r]][[m]]$om$rep$Fbar)]
        fbar_ref <- exp(mods[[r]][[m]]$om$rep$log_Fbar_XSPR[, ncol(mods[[r]][[m]]$om$rep$log_Fbar_XSPR)])
        ff <- tail(fbar / fbar_ref, use.n.years)
        data.frame(Model = paste0("Model", m),
                   Realization = r,
                   Year = tail(Years, use.n.years),
                   Overfishing = ff)
      })
    })
    fbar_df <- bind_rows(fbar_list)
  }
  
  # === Merge ===
  temp <- left_join(ssb_df, fbar_df, by = c("Model", "Realization", "Year"))
  temp$Index <- paste0("Year ", temp$Year)
  
  # === Rename Models if needed ===
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(temp$Model))) {
      stop("Length of new_model_names must match number of models.")
    }
    temp$Model <- factor(temp$Model,
                         levels = paste0("Model", seq_along(new_model_names)),
                         labels = new_model_names)
  }
  
  percentSPR <- if (!is.nsim) mods[[1]]$om$input$data$percentSPR else mods[[1]][[1]]$om$input$data$percentSPR
  
  # === Plot
  p <- ggplot(temp, aes(x = Overfished, y = Overfishing)) +
    facet_wrap(~ Model) +
    annotate('rect', xmin = 0.5, xmax = Inf, ymin = -Inf, ymax = 1, alpha = 0.2, fill = "yellow")
  
  # === Add smoothed 2D density layer
  if (show_density) {
    p <- p + geom_density_2d_filled(aes(fill = after_stat(level)), alpha = 0.4, contour_var = "density", bins = 100) +
      scale_fill_viridis_d(option = col.opt) + guides(fill = "none") 
  }
  
  # === Add points and reference lines
  if (show_density) {
    p <- p +
      geom_point(aes(color = Model), size = 1.5, alpha = 0.3) +
      scale_color_viridis_d(option = col.opt) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      xlab(bquote(paste("SSB/", SSB[.(percentSPR)*"%"]))) +
      ylab(bquote(paste("F/", F[.(percentSPR)*"%"]))) +
      ggtitle(paste0("Stock Status in the Last ", use.n.years, " Year(s)")) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 12),
            strip.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            aspect.ratio = 1,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      coord_cartesian(xlim = c(0, quantile(temp$Overfished, 0.95, na.rm = TRUE)),
                      ylim = c(0, quantile(temp$Overfishing, 0.95, na.rm = TRUE)))
  } else {
    p <- p +
      geom_point(aes(color = Model), size = 1.5, alpha = 0.8) +
      scale_color_viridis_d(option = col.opt) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
      xlab(bquote(paste("SSB/", SSB[.(percentSPR)*"%"]))) +
      ylab(bquote(paste("F/", F[.(percentSPR)*"%"]))) +
      ggtitle(paste0("Stock Status in the Last ", use.n.years, " Year(s)")) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 12),
            strip.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            aspect.ratio = 1,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black")) +
      coord_cartesian(xlim = c(0, quantile(temp$Overfished, 0.95, na.rm = TRUE)),
                      ylim = c(0, quantile(temp$Overfishing, 0.95, na.rm = TRUE)))
  }
  
  # === Save
  ggsave(file.path(main.dir, sub.dir, paste0("Kobe_Plot_KDE_", use.n.years, "_Year.png")),
         p, width = width, height = height, dpi = dpi)
  
  return(p)
}

plot_model_performance_radar <- function(mods, is.nsim, main.dir, sub.dir, 
                                         width = 10, height = 10, dpi = 300, col.opt = "D",
                                         use.n.years.first = 5,
                                         use.n.years.last = 5,
                                         start.years = 1, 
                                         new_model_names = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(fmsb)
  library(viridis)
  
  if (is.nsim) {
    
    n_models <- length(mods[[1]])
    n_reps <- length(mods)
    results <- list()
    
    for (r in seq_len(n_reps)) {
      tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
      
      for (m in seq_len(n_models)) {
        rep <- mods[[r]][[m]]$om$rep
        
        # Catch and SSB as rowSums
        catch_ts <- rowSums(rep$pred_catch)
        ssb_ts <- rowSums(rep$SSB)
        
        # Global Fbar
        n_fleets <- mods[[r]][[m]]$om$input$data$n_fleets[1]
        n_regions <- mods[[r]][[m]]$om$input$data$n_regions[1]
        fbar_ts <- rep$Fbar[, n_fleets + n_regions + 1]
        
        # First and last period means
        tmp$Catch_first[m] <- median(catch_ts[start.years:(start.years + use.n.years.first - 1)])
        tmp$SSB_first[m] <- median(ssb_ts[start.years:(start.years + use.n.years.first - 1)])
        tmp$Fbar_first[m] <- median(fbar_ts[start.years:(start.years + use.n.years.first - 1)])
        
        tmp$Catch_last[m] <- median(tail(catch_ts, use.n.years.last))
        tmp$SSB_last[m] <- median(tail(ssb_ts, use.n.years.last))
        tmp$Fbar_last[m] <- median(tail(fbar_ts, use.n.years.last))
      }
      
      # Normalize: higher is better except for Fbar
      for (v in c("Catch_first", "SSB_first", "Catch_last", "SSB_last")) {
        range_val <- max(tmp[[v]]) - min(tmp[[v]])
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          tmp[[v]] <- 100 * (tmp[[v]] - min(tmp[[v]])) / range_val
        }
      }
      for (v in c("Fbar_first", "Fbar_last")) {
        range_val <- max(tmp[[v]]) - min(tmp[[v]])
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          norm_f <- (tmp[[v]] - min(tmp[[v]])) / range_val
          tmp[[v]] <- 100 * (1 - norm_f)
        }
      }
      
      results[[r]] <- tmp
    }
    
    # Combine and compute median across realizations
    combined <- bind_rows(results, .id = "Realization")
    scores_median <- combined %>%
      group_by(Model) %>%
      summarise(across(-Realization, median), .groups = "drop")
    
    # Optional renaming
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(scores_median$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      scores_median$Model <- factor(scores_median$Model,
                                    levels = paste0("Model", seq_along(new_model_names)),
                                    labels = new_model_names)
    }
    
    # Radar chart prep
    plot_df <- as.data.frame(scores_median)         # Ensure it's a base data.frame
    rownames(plot_df) <- plot_df$Model              # Set row names safely
    plot_df$Model <- NULL                           # Drop Model column
    plot_df <- as.data.frame(t(plot_df))            # Transpose for radar chart
    
    # Add min/max rows for radar scale
    plot_df <- rbind(rep(100, ncol(plot_df)), rep(0, ncol(plot_df)), plot_df)
    
  } else {
    
    n_models <- length(mods)
    results <- list()
    
    tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      
      rep <- mods[[m]]$om$rep
      
      # Catch and SSB as rowSums
      catch_ts <- rowSums(rep$pred_catch)
      ssb_ts <- rowSums(rep$SSB)
      
      # Global Fbar
      n_fleets <- mods[[m]]$om$input$data$n_fleets[1]
      n_regions <- mods[[m]]$om$input$data$n_regions[1]
      fbar_ts <- rep$Fbar[, n_fleets + n_regions + 1]
      
      # First and last period means
      tmp$Catch_first[m] <- median(catch_ts[start.years:(start.years + use.n.years.first - 1)])
      tmp$SSB_first[m] <- median(ssb_ts[start.years:(start.years + use.n.years.first - 1)])
      tmp$Fbar_first[m] <- median(fbar_ts[start.years:(start.years + use.n.years.first - 1)])
      
      tmp$Catch_last[m] <- median(tail(catch_ts, use.n.years.last))
      tmp$SSB_last[m] <- median(tail(ssb_ts, use.n.years.last))
      tmp$Fbar_last[m] <- median(tail(fbar_ts, use.n.years.last))
    }
    
    # Normalize: higher is better except for Fbar
    for (v in c("Catch_first", "SSB_first", "Catch_last", "SSB_last")) {
      range_val <- max(tmp[[v]]) - min(tmp[[v]])
      if (range_val == 0) {
        tmp[[v]] <- 100  # All values are identical; assign full score
      } else {
        tmp[[v]] <- 100 * (tmp[[v]] - min(tmp[[v]])) / range_val
      }
    }
    
    for (v in c("Fbar_first", "Fbar_last")) {
      range_val <- max(tmp[[v]]) - min(tmp[[v]])
      if (range_val == 0) {
        tmp[[v]] <- 100  # All values identical; assign full score
      } else {
        norm_f <- (tmp[[v]] - min(tmp[[v]])) / range_val
        tmp[[v]] <- 100 * (1 - norm_f)
      }
    }
    
    results[[1]] <- tmp
    
    # Combine and compute median across realizations
    combined <- bind_rows(results, .id = "Realization")
    scores_median <- combined %>%
      group_by(Model) %>%
      summarise(across(-Realization, median), .groups = "drop")
    
    # Optional renaming
    if (!is.null(new_model_names)) {
      if (length(new_model_names) != length(unique(scores_median$Model))) {
        stop("Length of new_model_names must match the number of models.")
      }
      scores_median$Model <- factor(scores_median$Model,
                                    levels = paste0("Model", seq_along(new_model_names)),
                                    labels = new_model_names)
    }
    
    # Radar chart prep
    plot_df <- as.data.frame(scores_median)         # Ensure it's a base data.frame
    rownames(plot_df) <- plot_df$Model              # Set row names safely
    plot_df$Model <- NULL                           # Drop Model column
    plot_df <- as.data.frame(t(plot_df))            # Transpose for radar chart
    
    # Add min/max rows for radar scale
    plot_df <- rbind(rep(100, ncol(plot_df)), rep(0, ncol(plot_df)), plot_df)
    
  }
  
  if (ncol(plot_df) < 3) {
    warning("Radar chart needs at least 3 models. Showing barplot instead.")
    return(invisible(NULL))
  }
  
  # Plot without any further changes
  colors <- viridisLite::viridis(n = nrow(plot_df) - 2, option = col.opt) # -2 for min/max rows
  
  # Save to PNG
  output_file <- file.path(file.path(main.dir, sub.dir, "model_performance_radar.png"))
  png(filename = output_file, width = width, height = height, units = "in", res = dpi)
  radarchart(plot_df,
             axistype = 4,
             pcol = colors,
             plwd = 2,
             plty = 1,
             cglcol = "grey80",
             cglty = 1,
             axislabcol = "grey30",
             vlcex = 0.9,
             title = paste("Model Performance"))
  legend(x = 1.05, y = 0.5, legend = rownames(plot_df)[-c(1,2)], col = colors,
         lty = 1, lwd = 2, cex = 0.8, bty = "n")
  dev.off()
  
  # Also plot to screen (inline)
  op <- par(mar = c(1, 1, 3, 1))  # NEW: minimize margins
  radarchart(plot_df,
             axistype = 4,
             pcol = colors,
             plwd = 2,
             plty = 1,
             cglcol = "grey80",
             cglty = 1,
             axislabcol = "grey30",
             vlcex = 0.9,
             title = paste("Model Performance"))
  legend(x = 0.85, y = 0.4, legend = rownames(plot_df)[-c(1,2)], col = colors,
         lty = 1, lwd = 2, cex = 0.8, bty = "n")
  on.exit(par(op))  # restore after plotting
}

# plot_model_performance_triangle <- function(mods, is.nsim,
#                                             main.dir, sub.dir,
#                                             width = 8, height = 7, dpi = 300,
#                                             new_model_names = NULL,
#                                             col.opt = "D",
#                                             use.n.years.first = 5,
#                                             use.n.years.last = 5,
#                                             start.years = 1) {
#   library(dplyr)
#   library(ggtern)
#   library(viridisLite)
#   
#   if(start.years == 1) warnings("Starting year must be specified otherwise the first year in the historical period will be used!")
#   
#   process_scores <- function(rep, n_fleets, n_regions, use.n.years, start.years = NULL, type = c("short", "long")) {
#     catch_ts <- rowSums(rep$pred_catch)
#     ssb_ts <- rowSums(rep$SSB)
#     fbar_ts <- rep$Fbar[, n_fleets + n_regions + 1]
#     
#     if (type == "short") {
#       idx <- start.years:(start.years + use.n.years - 1)
#     } else {
#       idx <- (length(catch_ts) - use.n.years + 1):length(catch_ts)
#     }
#     
#     c(mean(catch_ts[idx]), mean(ssb_ts[idx]), mean(fbar_ts[idx]))
#   }
#   
#   results_short <- list()
#   results_long <- list()
#   
#   if (is.nsim) {
#     n_models <- length(mods[[1]])
#     n_reps <- length(mods)
#     
#     for (r in seq_len(n_reps)) {
#       df_short <- df_long <- data.frame(Model = paste0("Model", seq_len(n_models)))
#       for (m in seq_len(n_models)) {
#         rep <- mods[[r]][[m]]$om$rep
#         input <- mods[[r]][[m]]$om$input$data
#         n_fleets <- input$n_fleets[1]
#         n_regions <- input$n_regions[1]
#         
#         short_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.first, start.years, "short")
#         long_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.last, NULL, "long")
#         
#         df_short[m, c("Catch", "SSB", "Fbar")] <- short_vals
#         df_long[m, c("Catch", "SSB", "Fbar")] <- long_vals
#       }
#       
#       # Normalize within realization
#       for (v in c("Catch", "SSB")) {
#         df_short[[v]] <- 100 * (df_short[[v]] - min(df_short[[v]])) / (max(df_short[[v]]) - min(df_short[[v]]))
#         df_long[[v]] <- 100 * (df_long[[v]] - min(df_long[[v]])) / (max(df_long[[v]]) - min(df_long[[v]]))
#       }
#       for (v in c("Fbar")) {
#         df_short[[v]] <- 100 * (1 - (df_short[[v]] - min(df_short[[v]])) / (max(df_short[[v]]) - min(df_short[[v]])))
#         df_long[[v]] <- 100 * (1 - (df_long[[v]] - min(df_long[[v]]) ) / (max(df_long[[v]]) - min(df_long[[v]]) ))
#       }
#       
#       df_short <- df_short %>%
#         mutate(total = Catch + SSB + Fbar,
#                Catch = Catch / total,
#                SSB = SSB / total,
#                Fbar = Fbar / total,
#                Realization = r)
#       
#       df_long <- df_long %>%
#         mutate(total = Catch + SSB + Fbar,
#                Catch = Catch / total,
#                SSB = SSB / total,
#                Fbar = Fbar / total,
#                Realization = r)
#       
#       results_short[[r]] <- df_short
#       results_long[[r]] <- df_long
#     }
#   } else {
#     n_models <- length(mods)
#     df_short <- df_long <- data.frame(Model = paste0("Model", seq_len(n_models)))
#     
#     for (m in seq_len(n_models)) {
#       rep <- mods[[m]]$om$rep
#       input <- mods[[m]]$om$input$data
#       n_fleets <- input$n_fleets[1]
#       n_regions <- input$n_regions[1]
#       
#       short_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.first, start.years, "short")
#       long_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.last, NULL, "long")
#       
#       df_short[m, c("Catch", "SSB", "Fbar")] <- short_vals
#       df_long[m, c("Catch", "SSB", "Fbar")] <- long_vals
#     }
#     
#     # Normalize across single realization
#     for (v in c("Catch", "SSB")) {
#       range_short <- max(df_short[[v]]) - min(df_short[[v]])
#       if (range_short == 0) {
#         df_short[[v]] <- 100  # All identical, assign full score
#       } else {
#         df_short[[v]] <- 100 * (df_short[[v]] - min(df_short[[v]])) / range_short
#       }
#       
#       range_long <- max(df_long[[v]]) - min(df_long[[v]])
#       if (range_long == 0) {
#         df_long[[v]] <- 100
#       } else {
#         df_long[[v]] <- 100 * (df_long[[v]] - min(df_long[[v]])) / range_long
#       }
#     }
#     
#     for (v in c("Fbar")) {
#       range_short <- max(df_short[[v]]) - min(df_short[[v]])
#       if (range_short == 0) {
#         df_short[[v]] <- 100
#       } else {
#         norm_f_short <- (df_short[[v]] - min(df_short[[v]])) / range_short
#         df_short[[v]] <- 100 * (1 - norm_f_short)
#       }
#       
#       range_long <- max(df_long[[v]]) - min(df_long[[v]])
#       if (range_long == 0) {
#         df_long[[v]] <- 100
#       } else {
#         norm_f_long <- (df_long[[v]] - min(df_long[[v]])) / range_long
#         df_long[[v]] <- 100 * (1 - norm_f_long)
#       }
#     }
#     
#     df_short <- df_short %>%
#       mutate(total = Catch + SSB + Fbar,
#              Catch = Catch / total,
#              SSB = SSB / total,
#              Fbar = Fbar / total,
#              Realization = 1)
#     
#     df_long <- df_long %>%
#       mutate(total = Catch + SSB + Fbar,
#              Catch = Catch / total,
#              SSB = SSB / total,
#              Fbar = Fbar / total,
#              Realization = 1)
#     
#     results_short[[1]] <- df_short
#     results_long[[1]] <- df_long
#   }
#   
#   df_short_all <- bind_rows(results_short)
#   df_long_all <- bind_rows(results_long)
#   
#   if (!is.null(new_model_names)) {
#     if (length(new_model_names) != length(unique(df_short_all$Model))) {
#       stop("Length of new_model_names must match number of models.")
#     }
#     df_short_all$Model <- factor(df_short_all$Model,
#                                  levels = paste0("Model", seq_along(new_model_names)),
#                                  labels = new_model_names)
#     df_long_all$Model <- factor(df_long_all$Model,
#                                 levels = paste0("Model", seq_along(new_model_names)),
#                                 labels = new_model_names)
#   }
#   
#   # === Plot function ===
#   plot_and_save <- function(df, title, file) {
#     colors <- viridisLite::viridis(n = length(unique(df$Model)), option = col.opt)
#     p <- ggtern(df, aes(x = Catch, y = SSB, z = Fbar, color = Model)) +
#       geom_point(alpha = 0.3, size = 2) +
#       scale_color_manual(values = colors) +
#       labs(title = title, T = "Catch", L = "SSB", R = "Fbar") +
#       theme_rgbw() +
#       theme(plot.title = element_text(hjust = 0.5))
#     
#     ggsave(filename = file.path(main.dir, sub.dir, file), plot = p,
#            width = width, height = height, dpi = dpi)
#     
#     print(p)
#   }
#   
#   plot_and_save(df_short_all, paste0("Short-term Performance (Normalized): Years ", start.years, " to ", start.years+use.n.years.first-1), "model_performance_triangle_short.png")
#   plot_and_save(df_long_all, paste0("Long-term Performance (Normalized): Last ", use.n.years.last, " Years"), "model_performance_triangle_long.png")
# }

plot_mean_rec_par <- function(mods, is.nsim, main.dir, sub.dir, 
                              width = 10, height = 7, dpi = 300, col.opt = "D",
                              new_model_names = NULL) {
  library(dplyr)
  library(ggplot2)
  library(viridisLite)
  
  res <- NULL
  
  if (is.nsim) {
    for (i in seq_along(mods)) {
      for (j in seq_along(mods[[1]])) {
        tmp <- mods[[i]][[j]]
        k <- length(tmp$par.est)
        
        if (any(names(tmp$par.est[[k]]) == "mean_rec_pars")) {
          temp <- exp(tmp$par.est[[k]]$mean_rec_pars[, 1])
        } else {
          m <- length(tmp$par.est[[k]])
          temp <- NULL
          for (n in seq_len(m)) {
            temp1 <- exp(tmp$par.est[[k]][[n]]$mean_rec_pars[, 1])
            temp <- c(temp, temp1)
          }
        }
        
        res1 <- data.frame(
          Model = j,
          nsim = i,
          Value = temp,
          Var = if (length(temp) == 1) "Mean_Rec" else paste0("Mean_Rec_", seq_along(temp))
        )
        
        res <- rbind(res, res1)
      }
    }
  } else {
    for (j in seq_along(mods)) {
      tmp <- mods[[j]]
      k <- length(tmp$par.est)
      
      if (any(names(tmp$par.est[[k]]) == "mean_rec_pars")) {
        temp <- exp(tmp$par.est[[k]]$mean_rec_pars[, 1])
      } else {
        m <- length(tmp$par.est[[k]])
        temp <- NULL
        for (n in seq_len(m)) {
          temp1 <- exp(tmp$par.est[[k]][[n]]$mean_rec_pars[, 1])
          temp <- c(temp, temp1)
        }
      }
      
      res1 <- data.frame(
        Model = j,
        Value = temp,
        Var = if (length(temp) == 1) "Mean_Rec" else paste0("Mean_Rec_", seq_along(temp))
      )
      
      res <- rbind(res, res1)
    }
  }
  
  res$Model <- as.factor(paste0("Model",res$Model))
  
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
  }
  
  p1 <- ggplot(res, aes(x = Model, y = Value, col = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Var ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle("Mean Recruitment from the Last EM") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 15),
      plot.title = element_text(size = 12),
      strip.text = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
  
  ggsave(file.path(main.dir, sub.dir, "Mean_rec_par.png"), p1, width = 10, height = 10, dpi = dpi)
  
  return(p1)
}

plot_NAA_sigma_par <- function(mods, is.nsim, main.dir, sub.dir,
                               width = 10, height = 15, dpi = 300, col.opt = "D",
                               new_model_names = NULL) {
  library(dplyr)
  library(ggplot2)
  library(viridisLite)
  
  res <- NULL
  
  if (is.nsim) {
    for (i in seq_along(mods)) {
      for (j in seq_along(mods[[1]])) {
        tmp <- mods[[i]][[j]]
        k <- length(tmp$par.est)
        
        if (any(names(tmp$par.est[[k]]) == "log_NAA_sigma")) {
          rec_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 1])
          naa_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 2])
          temp <- c(rec_sig, naa_sig)
        } else {
          m <- length(tmp$par.est[[k]])
          temp <- NULL
          for (n in seq_len(m)) {
            rec_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 1])
            naa_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 2])
            temp <- c(temp, rec_sig, naa_sig)
          }
        }
        
        res1 <- data.frame(
          Model = j,
          nsim = i,
          Value = temp,
          Var = if (length(temp) == 2) {
            c("Rec_sigma", "NAA_sigma")
          } else {
            c(paste0("Rec_sigma", seq_along(rec_sig)), paste0("NAA_sigma", seq_along(naa_sig)))
          }
        )
        res <- rbind(res, res1)
      }
    }
  } else {
    for (j in seq_along(mods)) {
      tmp <- mods[[j]]
      k <- length(tmp$par.est)
      
      if (any(names(tmp$par.est[[k]]) == "log_NAA_sigma")) {
        rec_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 1])
        naa_sig <- exp(tmp$par.est[[k]]$log_NAA_sigma[, 1, 2])
        temp <- c(rec_sig, naa_sig)
      } else {
        m <- length(tmp$par.est[[k]])
        temp <- NULL
        for (n in seq_len(m)) {
          rec_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 1])
          naa_sig <- exp(tmp$par.est[[k]][[n]]$log_NAA_sigma[, 1, 2])
          temp <- c(temp, rec_sig, naa_sig)
        }
      }
      
      res1 <- data.frame(
        Model = j,
        Value = temp,
        Var = if (length(temp) == 2) {
          c("Rec_sigma", "NAA_sigma")
        } else {
          c(paste0("Rec_sigma", seq_along(rec_sig)), paste0("NAA_sigma", seq_along(naa_sig)))
        }
      )
      
      res <- rbind(res, res1)
    }
  }
  
  res$Model <- as.factor(paste0("Model", res$Model))
  
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
  }
  
  p2 <- ggplot(res, aes(x = Model, y = Value, col = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Var ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle("Variance of NAA from the Last EM") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 15),
      plot.title = element_text(size = 12),
      strip.text = element_text(size = 10, color = "black"),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
  
  ggsave(file.path(main.dir, sub.dir, "Variance_Para_NAA.png"), p2, width = width, height = height, dpi = dpi)
  
  return(p2)
}

plot_model_performance_bar <- function(mods, is.nsim,
                                       main.dir = ".",
                                       sub.dir = ".",
                                       new_model_names = NULL,
                                       width = 12, height = 8, dpi = 300,
                                       col.opt = "D",
                                       use.n.years.first = 5,
                                       use.n.years.last = 5,
                                       start.years = 1) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(viridisLite)
  
  results_list <- list()
  
  if (is.nsim) {
    n_models <- length(mods[[1]])
    n_reps <- length(mods)
    
    for (r in seq_len(n_reps)) {
      tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
      
      for (m in seq_len(n_models)) {
        rep <- mods[[r]][[m]]$om$rep
        input <- mods[[r]][[m]]$om$input$data
        n_fleets <- input$n_fleets[1]
        n_regions <- input$n_regions[1]
        
        # Collect timeseries
        catch_ts <- rowSums(rep$pred_catch)
        ssb_ts <- rowSums(rep$SSB)
        fbar_ts <- rep$Fbar[, n_fleets + n_regions + 1]
        
        # First and last period
        tmp$Catch_first[m] <- median(catch_ts[start.years:(start.years + use.n.years.first - 1)])
        tmp$SSB_first[m] <- median(ssb_ts[start.years:(start.years + use.n.years.first - 1)])
        tmp$Fbar_first[m] <- median(fbar_ts[start.years:(start.years + use.n.years.first - 1)])
        
        tmp$Catch_last[m] <- median(tail(catch_ts, use.n.years.last))
        tmp$SSB_last[m] <- median(tail(ssb_ts, use.n.years.last))
        tmp$Fbar_last[m] <- median(tail(fbar_ts, use.n.years.last))
      }
      
      # Normalize within realization (safe check)
      for (v in c("Catch_first", "SSB_first", "Catch_last", "SSB_last")) {
        range_val <- max(tmp[[v]]) - min(tmp[[v]])
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          tmp[[v]] <- 100 * (tmp[[v]] - min(tmp[[v]])) / range_val
        }
      }
      for (v in c("Fbar_first", "Fbar_last")) {
        range_val <- max(tmp[[v]]) - min(tmp[[v]])
        if (range_val == 0) {
          tmp[[v]] <- 100
        } else {
          norm_f <- (tmp[[v]] - min(tmp[[v]])) / range_val
          tmp[[v]] <- 100 * (1 - norm_f)
        }
      }
      
      tmp$Realization <- r
      results_list[[r]] <- tmp
    }
    
    combined <- bind_rows(results_list)
    
    # Summarize across realizations
    summary_data <- combined %>%
      pivot_longer(-c(Model, Realization), names_to = "Metric", values_to = "Score") %>%
      group_by(Model, Metric) %>%
      summarize(
        Median = median(Score),
        Q1 = quantile(Score, 0.25),
        Q3 = quantile(Score, 0.75),
        .groups = "drop"
      )
    
  } else {
    n_models <- length(mods)
    tmp <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      rep <- mods[[m]]$om$rep
      input <- mods[[m]]$om$input$data
      n_fleets <- input$n_fleets[1]
      n_regions <- input$n_regions[1]
      
      # Collect timeseries
      catch_ts <- rowSums(rep$pred_catch)
      ssb_ts <- rowSums(rep$SSB)
      fbar_ts <- rep$Fbar[, n_fleets + n_regions + 1]
      
      # First and last period
      tmp$Catch_first[m] <- median(catch_ts[start.years:(start.years + use.n.years.first - 1)])
      tmp$SSB_first[m] <- median(ssb_ts[start.years:(start.years + use.n.years.first - 1)])
      tmp$Fbar_first[m] <- median(fbar_ts[start.years:(start.years + use.n.years.first - 1)])
      
      tmp$Catch_last[m] <- median(tail(catch_ts, use.n.years.last))
      tmp$SSB_last[m] <- median(tail(ssb_ts, use.n.years.last))
      tmp$Fbar_last[m] <- median(tail(fbar_ts, use.n.years.last))
    }
    
    # Normalize across models (safe check)
    for (v in c("Catch_first", "SSB_first", "Catch_last", "SSB_last")) {
      range_val <- max(tmp[[v]]) - min(tmp[[v]])
      if (range_val == 0) {
        tmp[[v]] <- 100
      } else {
        tmp[[v]] <- 100 * (tmp[[v]] - min(tmp[[v]])) / range_val
      }
    }
    for (v in c("Fbar_first", "Fbar_last")) {
      range_val <- max(tmp[[v]]) - min(tmp[[v]])
      if (range_val == 0) {
        tmp[[v]] <- 100
      } else {
        norm_f <- (tmp[[v]] - min(tmp[[v]])) / range_val
        tmp[[v]] <- 100 * (1 - norm_f)
      }
    }
    
    tmp$Realization <- 1
    combined <- tmp
    
    summary_data <- tmp %>%
      pivot_longer(-c(Model, Realization), names_to = "Metric", values_to = "Median") %>%
      mutate(Q1 = Median, Q3 = Median)
  }
  
  # Rename if needed
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(summary_data$Model))) {
      stop("Length of new_model_names must match number of models.")
    }
    summary_data$Model <- factor(summary_data$Model,
                                 levels = paste0("Model", seq_along(new_model_names)),
                                 labels = new_model_names)
  }
  
  # Colors
  n_metrics <- length(unique(summary_data$Metric))
  my_colors <- viridisLite::viridis(n = n_metrics, option = col.opt)
  
  # Plot
  plot <- ggplot(summary_data, aes(x = Median, y = Model, fill = Metric, color = Metric)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(xmin = Q1, xmax = Q3), col = "black",
                  position = position_dodge(width = 0.9), width = 0.3, alpha = 0.3) +
    scale_fill_manual(values = my_colors) +
    scale_color_manual(values = my_colors) +
    theme_bw() +
    labs(title = paste("Model Performance: Last",use.n.years.last,"Years"),
         x = "Score (0-100)",
         y = "Estimation Model",
         fill = "Metric",
         color = "Metric") +
    theme(
      axis.text = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      plot.title = element_text(size = 18, face = "bold")
    )
  
  # Print and save
  print(plot)
  ggsave(filename = file.path(main.dir, sub.dir, "model_performance_barplot.png"),
         plot = plot, width = width, height = height, dpi = dpi)
}

plot_model_performance_triangle <- function(mods, is.nsim,
                                            main.dir, sub.dir,
                                            width = 8, height = 7, dpi = 300,
                                            new_model_names = NULL,
                                            col.opt = "D",
                                            use.n.years.first = 5,
                                            use.n.years.last = 5,
                                            start.years = 1) {
  library(dplyr)
  library(ggtern)
  library(viridisLite)
  
  # Safer warning
  if (missing(start.years)) {
    warning("start.years not provided; defaulting to 1 (first historical year).")
  }
  
  # Function to extract mean Catch, SSB, and Fbar
  process_scores <- function(rep, n_fleets, n_regions, use.n.years, start.years = NULL, type = c("short", "long")) {
    catch_ts <- rowSums(rep$pred_catch)
    ssb_ts <- rowSums(rep$SSB)
    fbar_ts <- rep$Fbar[, ncol(rep$Fbar)] # SAFER: use last column (global Fbar)
    
    if (type == "short") {
      idx <- start.years:(start.years + use.n.years - 1)
    } else {
      idx <- (length(catch_ts) - use.n.years + 1):length(catch_ts)
    }
    
    c(median(catch_ts[idx]), median(ssb_ts[idx]), median(fbar_ts[idx]))
  }
  
  results_short <- list()
  results_long <- list()
  
  if (is.nsim) {
    n_models <- length(mods[[1]])
    n_reps <- length(mods)
    
    for (r in seq_len(n_reps)) {
      df_short <- df_long <- data.frame(Model = paste0("Model", seq_len(n_models)))
      for (m in seq_len(n_models)) {
        rep <- mods[[r]][[m]]$om$rep
        input <- mods[[r]][[m]]$om$input$data
        n_fleets <- input$n_fleets[1]
        n_regions <- input$n_regions[1]
        
        short_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.first, start.years, "short")
        long_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.last, NULL, "long")
        
        df_short[m, c("Catch", "SSB", "Fbar")] <- short_vals
        df_long[m, c("Catch", "SSB", "Fbar")] <- long_vals
      }
      
      # Normalize within realization (Catch & SSB up, Fbar inverted)
      for (v in c("Catch", "SSB")) {
        range_v <- max(df_short[[v]]) - min(df_short[[v]])
        df_short[[v]] <- if (range_v == 0) 1 else (df_short[[v]] - min(df_short[[v]])) / range_v
        
        range_v_long <- max(df_long[[v]]) - min(df_long[[v]])
        df_long[[v]] <- if (range_v_long == 0) 1 else (df_long[[v]] - min(df_long[[v]])) / range_v_long
      }
      for (v in c("Fbar")) {
        range_v <- max(df_short[[v]]) - min(df_short[[v]])
        norm_f <- if (range_v == 0) 0 else (df_short[[v]] - min(df_short[[v]])) / range_v
        df_short[[v]] <- 1 - norm_f # inverted
        
        range_v_long <- max(df_long[[v]]) - min(df_long[[v]])
        norm_f_long <- if (range_v_long == 0) 0 else (df_long[[v]] - min(df_long[[v]])) / range_v_long
        df_long[[v]] <- 1 - norm_f_long
      }
      
      # Now we already have values 0–1: just renormalize to sum = 1
      df_short <- df_short %>%
        mutate(total = Catch + SSB + Fbar,
               Catch = Catch / total,
               SSB = SSB / total,
               Fbar = Fbar / total,
               Realization = r)
      
      df_long <- df_long %>%
        mutate(total = Catch + SSB + Fbar,
               Catch = Catch / total,
               SSB = SSB / total,
               Fbar = Fbar / total,
               Realization = r)
      
      results_short[[r]] <- df_short
      results_long[[r]] <- df_long
    }
  } else {
    n_models <- length(mods)
    df_short <- df_long <- data.frame(Model = paste0("Model", seq_len(n_models)))
    
    for (m in seq_len(n_models)) {
      rep <- mods[[m]]$om$rep
      input <- mods[[m]]$om$input$data
      n_fleets <- input$n_fleets[1]
      n_regions <- input$n_regions[1]
      
      short_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.first, start.years, "short")
      long_vals <- process_scores(rep, n_fleets, n_regions, use.n.years.last, NULL, "long")
      
      df_short[m, c("Catch", "SSB", "Fbar")] <- short_vals
      df_long[m, c("Catch", "SSB", "Fbar")] <- long_vals
    }
    
    for (v in c("Catch", "SSB")) {
      range_v <- max(df_short[[v]]) - min(df_short[[v]])
      df_short[[v]] <- if (range_v == 0) 1 else (df_short[[v]] - min(df_short[[v]])) / range_v
      
      range_v_long <- max(df_long[[v]]) - min(df_long[[v]])
      df_long[[v]] <- if (range_v_long == 0) 1 else (df_long[[v]] - min(df_long[[v]])) / range_v_long
    }
    for (v in c("Fbar")) {
      range_v <- max(df_short[[v]]) - min(df_short[[v]])
      norm_f <- if (range_v == 0) 0 else (df_short[[v]] - min(df_short[[v]])) / range_v
      df_short[[v]] <- 1 - norm_f
      
      range_v_long <- max(df_long[[v]]) - min(df_long[[v]])
      norm_f_long <- if (range_v_long == 0) 0 else (df_long[[v]] - min(df_long[[v]])) / range_v_long
      df_long[[v]] <- 1 - norm_f_long
    }
    
    df_short <- df_short %>%
      mutate(total = Catch + SSB + Fbar,
             Catch = Catch / total,
             SSB = SSB / total,
             Fbar = Fbar / total,
             Realization = 1)
    
    df_long <- df_long %>%
      mutate(total = Catch + SSB + Fbar,
             Catch = Catch / total,
             SSB = SSB / total,
             Fbar = Fbar / total,
             Realization = 1)
    
    results_short[[1]] <- df_short
    results_long[[1]] <- df_long
  }
  
  df_short_all <- bind_rows(results_short)
  df_long_all <- bind_rows(results_long)
  
  df_short_all <- bind_rows(results_short) %>%
    group_by(Model) %>%
    summarise(
      Catch = median(Catch),
      SSB = median(SSB),
      Fbar = median(Fbar),
      .groups = "drop"
    )
  
  df_long_all <- bind_rows(results_long) %>%
    group_by(Model) %>%
    summarise(
      Catch = median(Catch),
      SSB = median(SSB),
      Fbar = median(Fbar),
      .groups = "drop"
    )
  
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(df_short_all$Model))) {
      stop("Length of new_model_names must match number of models.")
    }
    df_short_all$Model <- factor(df_short_all$Model,
                                 levels = paste0("Model", seq_along(new_model_names)),
                                 labels = new_model_names)
    df_long_all$Model <- factor(df_long_all$Model,
                                levels = paste0("Model", seq_along(new_model_names)),
                                labels = new_model_names)
  }
  
  # === Plot function ===
  plot_and_save <- function(df, title, file) {
    colors <- viridisLite::viridis(n = length(unique(df$Model)), option = col.opt)
    p <- ggtern(df, aes(x = Catch, y = SSB, z = Fbar, color = Model)) +
      geom_point(alpha = 0.8, size = 8) +
      scale_color_manual(values = colors) +
      labs(title = title, T = "Catch", L = "SSB", R = "Fbar") +
      theme_rgbw() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggsave(filename = file.path(main.dir, sub.dir, file), plot = p,
           width = width, height = height, dpi = dpi)
    
    print(p)
  }
  
  plot_and_save2 <- function(df, title, file) {
    colors <- viridisLite::viridis(n = length(unique(df$Model)), option = col.opt)
    p <- ggtern(df, aes(x = Catch, y = SSB, z = Fbar, color = Model)) +
      geom_point(size = 1) +
      scale_color_manual(values = colors) +
      labs(title = title, T = "Catch", L = "SSB", R = "Fbar") +
      theme_rgbw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_confidence_tern(breaks = 0.95)
    
    ggsave(filename = file.path(main.dir, sub.dir, file), plot = p,
           width = width, height = height, dpi = dpi)
    
    print(p)
  }
  
  plot_and_save(df_short_all, paste0("Short-term Performance (Normalized): Years ", start.years, " to ", start.years + use.n.years.first - 1), "model_performance_triangle_short.png")
  plot_and_save(df_long_all, paste0("Long-term Performance (Normalized): Last ", use.n.years.last, " Years"), "model_performance_triangle_long.png")
  plot_and_save2(bind_rows(results_short), paste0("Short-term Performance: Years ", start.years, " to ", start.years + use.n.years.first - 1), "model_performance_triangle_short_raw.png")
  plot_and_save2(bind_rows(results_long), paste0("Long-term Performance: Last ", use.n.years.last, " Years"), "model_performance_triangle_long_raw.png")
}

calculate_aacv <- function(catch_values) {
  # Ensure catch_values is a numeric vector
  if (!is.numeric(catch_values)) {
    stop("Input catch_values must be a numeric vector.")
  }
  
  if (length(catch_values) < 2) {
    warning("Need at least 2 years of catch to calculate AACV.")
    return(NA)
  }
  
  catch_diff <- abs(diff(catch_values))
  denom <- sum(catch_values[-length(catch_values)])
  
  if (denom == 0) {
    warning("Total catch is zero; returning NA for AACV.")
    return(NA)
  }
  
  aacv <- sum(catch_diff) / denom
  return(aacv)
}

plot_catch_variation <- function(mods, is.nsim, main.dir, sub.dir, var = "Catch",
                                 width = 10, height = 7, dpi = 300, col.opt = "D",
                                 new_model_names = NULL,
                                 base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  # Helper to calculate AACV (Average Annual Catch Variation)
  calculate_aacv <- function(catch_values) {
    if (!is.numeric(catch_values)) {
      stop("Input catch_values must be a numeric vector.")
    }
    catch_diff <- abs(diff(catch_values))
    aacv <- sum(catch_diff) / sum(catch_values[-length(catch_values)])
    return(aacv)
  }
  
  res <- NULL
  
  if (!is.nsim) {
    n_fleets <- ncol(mods[[1]]$om$rep$pred_catch)
    Years <- mods[[1]]$om$years
    
    res <- lapply(seq_along(mods), function(i) {
      catch_mat <- mods[[i]]$om$rep$pred_catch # n_years × n_fleets
      aacv_list <- lapply(seq_len(n_fleets), function(f) {
        calculate_aacv(catch_mat[, f])
      })
      # Global catch AACV
      global_aacv <- calculate_aacv(rowSums(catch_mat))
      
      tmp <- data.frame(
        Local = t(unlist(aacv_list)),
        Global = global_aacv,
        Model = paste0("Model", i),
        Realization = 1
      )
      colnames(tmp)[1:n_fleets] <- paste0(var, "_Fleet", seq_len(n_fleets))
      colnames(tmp)[n_fleets + 1] <- paste0(var, "_Global")
      tmp
    }) %>% bind_rows()
    
  } else {
    n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)
    Years <- mods[[1]][[1]]$om$years
    
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        catch_mat <- mods[[r]][[m]]$om$rep$pred_catch
        aacv_list <- lapply(seq_len(n_fleets), function(f) {
          calculate_aacv(catch_mat[, f])
        })
        global_aacv <- calculate_aacv(rowSums(catch_mat))
        
        tmp <- data.frame(
          Local = t(unlist(aacv_list)),
          Global = global_aacv,
          Model = paste0("Model", m),
          Realization = r
        )
        colnames(tmp)[1:n_fleets] <- paste0(var, "_Fleet", seq_len(n_fleets))
        colnames(tmp)[n_fleets + 1] <- paste0(var, "_Global")
        tmp
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models if specified
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  # Pivot longer
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = "AACV")
  
  # Relative difference if base.model specified
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = AACV) %>%
      select(Realization, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Label")) %>%
      mutate(AACV = AACV / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = AACV, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0("Average Annual Catch Variation",
                   if (!is.null(base.model)) paste0(" (Relative to ", base.model, ")"))) +
    ylab(ifelse(is.null(base.model), "AACV", "Relative AACV Difference")) +
    theme_bw() 
  
  # Save plot
  plot_name <- paste0(var, "_variation", ifelse(is.null(base.model), "", "_rel"), ".png")
  ggsave(file.path(main.dir, sub.dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}

plot_relative_trajectories <- function(mods, is.nsim,
                                       main.dir, sub.dir,
                                       base.model = "Model1",
                                       new_model_names = NULL,
                                       width = 10, height = 7, dpi = 300,
                                       col.opt = "D") {

  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(rlang)

  # ---- Helper: extract relative trajectories ----
  extract_relative_trajectories <- function(mods, base_em_idx) {

    data <- NULL
    Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years

    if (!is.nsim) {
      n_stocks <- ncol(mods[[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]]$om$rep$pred_catch)

      for (m in seq_along(mods)) {
        ssb_mat <- mods[[m]]$om$rep$SSB
        ssb_base <- mods[[base_em_idx]]$om$rep$SSB
        catch_mat <- mods[[m]]$om$rep$pred_catch
        catch_base <- mods[[base_em_idx]]$om$rep$pred_catch

        # SSB per stock
        for (s in seq_len(n_stocks)) {
          tmp <- ssb_mat[, s] / ssb_base[, s] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("SSB_", s), Value = tmp)
          data <- bind_rows(data, res)
        }

        # SSB global
        tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "SSB_Global", Value = tmp)
        data <- bind_rows(data, res)

        # Catch per fleet
        for (f in seq_len(n_fleets)) {
          tmp <- catch_mat[, f] / catch_base[, f] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("Catch_", f), Value = tmp)
          data <- bind_rows(data, res)
        }

        # Catch global
        tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "Catch_Global", Value = tmp)
        data <- bind_rows(data, res)
      }

    } else {
      n_stocks <- ncol(mods[[1]][[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)

      for (r in seq_along(mods)) {
        for (m in seq_along(mods[[r]])) {
          ssb_mat <- mods[[r]][[m]]$om$rep$SSB
          ssb_base <- mods[[r]][[base_em_idx]]$om$rep$SSB
          catch_mat <- mods[[r]][[m]]$om$rep$pred_catch
          catch_base <- mods[[r]][[base_em_idx]]$om$rep$pred_catch

          # SSB per stock
          for (s in seq_len(n_stocks)) {
            tmp <- ssb_mat[, s] / ssb_base[, s] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("SSB_", s), Value = tmp)
            data <- bind_rows(data, res)
          }

          # SSB global
          tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "SSB_Global", Value = tmp)
          data <- bind_rows(data, res)

          # Catch per fleet
          for (f in seq_len(n_fleets)) {
            tmp <- catch_mat[, f] / catch_base[, f] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("Catch_", f), Value = tmp)
            data <- bind_rows(data, res)
          }

          # Catch global
          tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "Catch_Global", Value = tmp)
          data <- bind_rows(data, res)
        }
      }
    }

    return(data)
  }

  # --- Determine base_em_idx ---
  n_models <- if (!is.nsim) length(mods) else length(mods[[1]])

  if (is.null(new_model_names)) {
    new_model_names <- paste0("Model", seq_len(n_models))
  }

  if (is.character(base.model)) {
    base_em_idx <- which(new_model_names == base.model)
    if (length(base_em_idx) == 0) stop("base.model not found in new_model_names.")
  } else {
    stop("base.model must be specified as a model name (e.g., 'Model1').")
  }

  # --- Extract data ---
  data <- extract_relative_trajectories(mods, base_em_idx = base_em_idx)

  # --- Rename EMs ---
  data$EM <- factor(data$EM,
                    levels = seq_along(new_model_names),
                    labels = new_model_names)

  # --- Summarize ---
  sum_data <- data %>%
    group_by(EM, Index, Years) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.4, na.rm = TRUE),
      Q3 = quantile(Value, 0.6, na.rm = TRUE),
      .groups = "drop"
    )

  plot = list()

  t = 0
  # --- Plot loop ---
  for (name in unique(sum_data$Index)) {

    subset_data <- filter(sum_data, Index == name)

    t = t + 1

    plot[[t]] <- ggplot(subset_data, aes(x = Years, y = Median, color = EM)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = EM), alpha = 0.1, color = NA) +
      scale_color_viridis_d(option = col.opt) +
      scale_fill_viridis_d(option = col.opt) +
      labs(x = "Years", y = "Relative Difference", color = "EM", fill = "EM") +
      ggtitle(name) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")

    plot_name <- paste0(name,"_trajectories.png")
    ggsave(file.path(main.dir, sub.dir, plot_name), plot[[t]], width = width, height = height, dpi = dpi)

    # return(list(plot[[t]]))
    # print(plot[[t]])
  }
  print(plot[[t]])
}


plot_relative_trajectories1 <- function(mods, is.nsim,
                                        main.dir, sub.dir,
                                        base.model = "Model1",
                                        new_model_names = NULL,
                                        width = 10, height = 7, dpi = 300,
                                        col.opt = "D") {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(rlang)
  
  # ---- Helper: extract relative trajectories ----
  extract_relative_trajectories <- function(mods, base_em_idx) {
    
    data <- NULL
    Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
    
    if (!is.nsim) {
      n_stocks <- ncol(mods[[1]]$om$rep$SSB)
      
      for (m in seq_along(mods)) {
        ssb_mat <- mods[[m]]$om$rep$SSB
        ssb_base <- mods[[base_em_idx]]$om$rep$SSB
        
        # SSB per stock
        for (s in seq_len(n_stocks)) {
          tmp <- ssb_mat[, s] / ssb_base[, s] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("SSB_", s), Value = tmp)
          data <- bind_rows(data, res)
        }
        
        # SSB global
        tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "SSB_Global", Value = tmp)
        data <- bind_rows(data, res)
        
      }
      
    } else {
      n_stocks <- ncol(mods[[1]][[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)
      
      for (r in seq_along(mods)) {
        for (m in seq_along(mods[[r]])) {
          ssb_mat <- mods[[r]][[m]]$om$rep$SSB
          ssb_base <- mods[[r]][[base_em_idx]]$om$rep$SSB
          
          # SSB per stock
          for (s in seq_len(n_stocks)) {
            tmp <- ssb_mat[, s] / ssb_base[, s] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("SSB_", s), Value = tmp)
            data <- bind_rows(data, res)
          }
          
          # SSB global
          tmp <- rowSums(ssb_mat) / rowSums(ssb_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "SSB_Global", Value = tmp)
          data <- bind_rows(data, res)
          
        }
      }
    }
    
    return(data)
  }
  
  # --- Determine base_em_idx ---
  n_models <- if (!is.nsim) length(mods) else length(mods[[1]])
  
  if (is.null(new_model_names)) {
    new_model_names <- paste0("Model", seq_len(n_models))
  }
  
  if (is.character(base.model)) {
    base_em_idx <- which(new_model_names == base.model)
    if (length(base_em_idx) == 0) stop("base.model not found in new_model_names.")
  } else {
    stop("base.model must be specified as a model name (e.g., 'Model1').")
  }
  
  # --- Extract data ---
  data <- extract_relative_trajectories(mods, base_em_idx = base_em_idx)
  
  # --- Rename EMs ---
  data$EM <- factor(data$EM,
                    levels = seq_along(new_model_names),
                    labels = new_model_names)
  
  # --- Summarize ---
  sum_data <- data %>%
    group_by(EM, Index, Years) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.4, na.rm = TRUE),
      Q3 = quantile(Value, 0.6, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Plot loop ---
  for (name in unique(sum_data$Index)) {
    
    subset_data <- filter(sum_data, Index == name)
    
    plot1 <- ggplot(subset_data, aes(x = Years, y = Median, color = EM)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = EM), alpha = 0.1, color = NA) +
      scale_color_viridis_d(option = col.opt) +
      scale_fill_viridis_d(option = col.opt) +
      labs(x = "Years", y = "Relative Difference", color = "EM", fill = "EM") +
      ggtitle(name) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")
    
    plot_name <- paste0(name,"_trajectories.png")
    ggsave(file.path(main.dir, sub.dir, plot_name), plot1, width = width, height = height, dpi = dpi)
  }
  return(plot1)
}

plot_relative_trajectories2 <- function(mods, is.nsim,
                                        main.dir, sub.dir,
                                        base.model = "Model1",
                                        new_model_names = NULL,
                                        width = 10, height = 7, dpi = 300,
                                        col.opt = "D") {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(rlang)
  
  # ---- Helper: extract relative trajectories ----
  extract_relative_trajectories <- function(mods, base_em_idx) {
    
    data <- NULL
    Years <- if (!is.nsim) mods[[1]]$om$years else mods[[1]][[1]]$om$years
    
    if (!is.nsim) {
      n_stocks <- ncol(mods[[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]]$om$rep$pred_catch)
      
      for (m in seq_along(mods)) {
        catch_mat <- mods[[m]]$om$rep$pred_catch
        catch_base <- mods[[base_em_idx]]$om$rep$pred_catch
        
        # Catch per fleet
        for (f in seq_len(n_fleets)) {
          tmp <- catch_mat[, f] / catch_base[, f] - 1
          res <- data.frame(Realization = 1, EM = m, Years = Years,
                            Index = paste0("Catch_", f), Value = tmp)
          data <- bind_rows(data, res)
        }
        
        # Catch global
        tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
        res <- data.frame(Realization = 1, EM = m, Years = Years,
                          Index = "Catch_Global", Value = tmp)
        data <- bind_rows(data, res)
      }
      
    } else {
      n_stocks <- ncol(mods[[1]][[1]]$om$rep$SSB)
      n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)
      
      for (r in seq_along(mods)) {
        for (m in seq_along(mods[[r]])) {
          catch_mat <- mods[[r]][[m]]$om$rep$pred_catch
          catch_base <- mods[[r]][[base_em_idx]]$om$rep$pred_catch
          
          # Catch per fleet
          for (f in seq_len(n_fleets)) {
            tmp <- catch_mat[, f] / catch_base[, f] - 1
            res <- data.frame(Realization = r, EM = m, Years = Years,
                              Index = paste0("Catch_", f), Value = tmp)
            data <- bind_rows(data, res)
          }
          
          # Catch global
          tmp <- rowSums(catch_mat) / rowSums(catch_base) - 1
          res <- data.frame(Realization = r, EM = m, Years = Years,
                            Index = "Catch_Global", Value = tmp)
          data <- bind_rows(data, res)
        }
      }
    }
    
    return(data)
  }
  
  # --- Determine base_em_idx ---
  n_models <- if (!is.nsim) length(mods) else length(mods[[1]])
  
  if (is.null(new_model_names)) {
    new_model_names <- paste0("Model", seq_len(n_models))
  }
  
  if (is.character(base.model)) {
    base_em_idx <- which(new_model_names == base.model)
    if (length(base_em_idx) == 0) stop("base.model not found in new_model_names.")
  } else {
    stop("base.model must be specified as a model name (e.g., 'Model1').")
  }
  
  # --- Extract data ---
  data <- extract_relative_trajectories(mods, base_em_idx = base_em_idx)
  
  # --- Rename EMs ---
  data$EM <- factor(data$EM,
                    levels = seq_along(new_model_names),
                    labels = new_model_names)
  
  # --- Summarize ---
  sum_data <- data %>%
    group_by(EM, Index, Years) %>%
    summarize(
      Median = median(Value, na.rm = TRUE),
      Q1 = quantile(Value, 0.4, na.rm = TRUE),
      Q3 = quantile(Value, 0.6, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --- Plot loop ---
  for (name in unique(sum_data$Index)) {
    
    subset_data <- filter(sum_data, Index == name)
    
    plot2 <- ggplot(subset_data, aes(x = Years, y = Median, color = EM)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = EM), alpha = 0.1, color = NA) +
      scale_color_viridis_d(option = col.opt) +
      scale_fill_viridis_d(option = col.opt) +
      labs(x = "Years", y = "Relative Difference", color = "EM", fill = "EM") +
      ggtitle(name) +
      theme_bw() +
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")
    
    plot_name <- paste0(name,"_trajectories.png")
    ggsave(file.path(main.dir, sub.dir, plot_name), plot2, width = width, height = height, dpi = dpi)
    
  }
  return(plot2)
}

plot_ssb_variation <- function(mods, is.nsim, main.dir, sub.dir, var = "SSB",
                               width = 10, height = 7, dpi = 300, col.opt = "D",
                               new_model_names = NULL,
                               base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  # Helper to calculate AACV (Average Annual Catch Variation)
  calculate_aacv <- function(catch_values) {
    if (!is.numeric(catch_values)) {
      stop("Input catch_values must be a numeric vector.")
    }
    catch_diff <- abs(diff(catch_values))
    aacv <- sum(catch_diff) / sum(catch_values[-length(catch_values)])
    return(aacv)
  }
  
  res <- NULL
  
  if (!is.nsim) {
    n_regions <- ncol(mods[[1]]$om$rep$SSB)
    Years <- mods[[1]]$om$years
    
    res <- lapply(seq_along(mods), function(i) {
      catch_mat <- mods[[i]]$om$rep$SSB # n_years × n_regions
      aacv_list <- lapply(seq_len(n_regions), function(f) {
        calculate_aacv(catch_mat[, f])
      })
      # Global catch AACV
      global_aacv <- calculate_aacv(rowSums(catch_mat))
      
      tmp <- data.frame(
        Local = t(unlist(aacv_list)),
        Global = global_aacv,
        Model = paste0("Model", i),
        Realization = 1
      )
      colnames(tmp)[1:n_regions] <- paste0(var, "_Region", seq_len(n_regions))
      colnames(tmp)[n_regions + 1] <- paste0(var, "_Global")
      tmp
    }) %>% bind_rows()
    
  } else {
    n_regions <- ncol(mods[[1]][[1]]$om$rep$SSB)
    Years <- mods[[1]][[1]]$om$years
    
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        catch_mat <- mods[[r]][[m]]$om$rep$SSB
        aacv_list <- lapply(seq_len(n_regions), function(f) {
          calculate_aacv(catch_mat[, f])
        })
        global_aacv <- calculate_aacv(rowSums(catch_mat))
        
        tmp <- data.frame(
          Local = t(unlist(aacv_list)),
          Global = global_aacv,
          Model = paste0("Model", m),
          Realization = r
        )
        colnames(tmp)[1:n_regions] <- paste0(var, "_Region", seq_len(n_regions))
        colnames(tmp)[n_regions + 1] <- paste0(var, "_Global")
        tmp
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models if specified
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    # if (!is.null(base.model)) base.model <- new_model_names[base.model]
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  # Pivot longer
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = "AACV")
  
  # Ensure Global is always the last row in facet
  labels_order <- res %>%
    distinct(Label) %>%
    arrange(if_else(grepl("Global", Label), 2, 1), Label) %>%
    pull(Label)
  
  res$Label <- factor(res$Label, levels = labels_order)
  
  # Relative difference if base.model specified
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = AACV) %>%
      select(Realization, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Label")) %>%
      mutate(AACV = AACV / base_val - 1)
  }
  
  # Plot
  p1 <- ggplot(res, aes(x = Model, y = AACV, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0("Average Annual SSB Variation",
                   if (!is.null(base.model)) paste0(" (Relative to ", base.model, ")"))) +
    ylab(ifelse(is.null(base.model), "AACV", "Relative AACV Difference")) +
    theme_bw() 
  
  # Save plot
  plot_name <- paste0(var, "_variation", ifelse(is.null(base.model), "", "_rel"), ".png")
  ggsave(file.path(main.dir, sub.dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}


plot_fbar_variation <- function(mods, is.nsim, main.dir, sub.dir, var = "Fbar",
                                width = 10, height = 7, dpi = 300, col.opt = "D",
                                new_model_names = NULL,
                                base.model = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(rlang)
  
  calculate_aacv <- function(values) {
    if (!is.numeric(values)) stop("Input must be numeric.")
    diff_abs <- abs(diff(values))
    aacv <- sum(diff_abs) / sum(values[-length(values)])
    return(aacv)
  }
  
  res <- NULL
  
  if (!is.nsim) {
    n_fleets <- mods[[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]]$om$input$data$n_regions[1]
    Years <- mods[[1]]$om$years
    
    res <- lapply(seq_along(mods), function(i) {
      fbar_mat <- mods[[i]]$om$rep$Fbar # Fbar matrix
      
      # Fleets
      aacv_fleet <- lapply(seq_len(n_fleets), function(f) {
        calculate_aacv(fbar_mat[, f])
      })
      
      # Regions
      aacv_region <- lapply(seq_len(n_regions), function(r) {
        calculate_aacv(fbar_mat[, n_fleets + r])
      })
      
      # Global
      aacv_global <- calculate_aacv(fbar_mat[, n_fleets + n_regions + 1])
      
      tmp <- data.frame(
        t(unlist(aacv_fleet)),
        t(unlist(aacv_region)),
        Global = aacv_global,
        Model = paste0("Model", i),
        Realization = 1
      )
      colnames(tmp)[1:n_fleets] <- paste0(var, "_Fleet", seq_len(n_fleets))
      colnames(tmp)[(n_fleets + 1):(n_fleets + n_regions)] <- paste0(var, "_Region", seq_len(n_regions))
      colnames(tmp)[n_fleets + n_regions + 1] <- paste0(var, "_Global")
      tmp
    }) %>% bind_rows()
    
  } else {
    n_fleets <- mods[[1]][[1]]$om$input$data$n_fleets[1]
    n_regions <- mods[[1]][[1]]$om$input$data$n_regions[1]
    Years <- mods[[1]][[1]]$om$years
    
    res <- lapply(seq_along(mods), function(r) {
      lapply(seq_along(mods[[r]]), function(m) {
        fbar_mat <- mods[[r]][[m]]$om$rep$Fbar
        
        # Fleets
        aacv_fleet <- lapply(seq_len(n_fleets), function(f) {
          calculate_aacv(fbar_mat[, f])
        })
        
        # Regions
        aacv_region <- lapply(seq_len(n_regions), function(rr) {
          calculate_aacv(fbar_mat[, n_fleets + rr])
        })
        
        # Global
        aacv_global <- calculate_aacv(fbar_mat[, n_fleets + n_regions + 1])
        
        tmp <- data.frame(
          t(unlist(aacv_fleet)),
          t(unlist(aacv_region)),
          Global = aacv_global,
          Model = paste0("Model", m),
          Realization = r
        )
        colnames(tmp)[1:n_fleets] <- paste0(var, "_Fleet", seq_len(n_fleets))
        colnames(tmp)[(n_fleets + 1):(n_fleets + n_regions)] <- paste0(var, "_Region", seq_len(n_regions))
        colnames(tmp)[n_fleets + n_regions + 1] <- paste0(var, "_Global")
        tmp
      }) %>% bind_rows()
    }) %>% bind_rows()
  }
  
  # Rename models if needed
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(res$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    res$Model <- factor(res$Model,
                        levels = paste0("Model", seq_along(new_model_names)),
                        labels = new_model_names)
    if (!is.null(base.model)) {
      if (!(base.model %in% new_model_names)) {
        warning("base.model does not match any of the new_model_names.")
      }
    }
  }
  
  res <- pivot_longer(res, cols = starts_with(var), names_to = "Label", values_to = "AACV")
  
  # Ensure Global is always the last row in facet
  labels_order <- res %>%
    distinct(Label) %>%
    arrange(if_else(grepl("Global", Label), 2, 1), Label) %>%
    pull(Label)
  
  res$Label <- factor(res$Label, levels = labels_order)
  
  
  if (!is.null(base.model)) {
    base_df <- res %>% filter(Model == base.model) %>%
      rename(base_val = AACV) %>%
      select(Realization, Label, base_val)
    
    res <- left_join(res, base_df, by = c("Realization", "Label")) %>%
      mutate(AACV = AACV / base_val - 1)
  }
  
  p1 <- ggplot(res, aes(x = Model, y = AACV, color = Model)) +
    geom_boxplot(outlier.shape = NA) +
    facet_grid(Label ~ ., scales = "free") +
    scale_color_viridis_d(option = col.opt) +
    ggtitle(paste0("Average Annual Fbar Variation",
                   if (!is.null(base.model)) paste0(" (Relative to ", base.model, ")"))) +
    ylab(ifelse(is.null(base.model), "AACV", "Relative AACV Difference")) +
    theme_bw()
  
  # Save
  out_dir <- file.path(main.dir, sub.dir)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  plot_name <- paste0(var, "_variation", ifelse(is.null(base.model), "", "_rel"), ".png")
  ggsave(file.path(out_dir, plot_name), p1, width = width, height = height, dpi = dpi)
  
  return(p1)
}


plot_AAV_performance <- function(mods, is.nsim,
                                 main.dir, sub.dir,
                                 width = 8, height = 7, dpi = 300,
                                 new_model_names = NULL,
                                 col.opt = "D") {
  library(dplyr)
  library(ggtern)
  library(viridisLite)
  
  # Helper: calculate Average Annual Catch Variation (AACV)
  calculate_aacv <- function(values) {
    if (!is.numeric(values)) {
      stop("Input must be a numeric vector.")
    }
    diffs <- abs(diff(values))
    aacv <- sum(diffs) / sum(values[-length(values)])
    return(aacv)
  }
  
  # --- Loop through each mod and collect variation measures ---
  res_list <- list()
  
  if (!is.nsim) {
    n_fleets <- ncol(mods[[1]]$om$rep$pred_catch)
    n_regions <- ncol(mods[[1]]$om$rep$SSB)
    
    for (i in seq_along(mods)) {
      om_rep <- mods[[i]]$om$rep
      
      # Catch variation
      catch_aacv <- calculate_aacv(rowSums(om_rep$pred_catch))
      
      # SSB variation
      ssb_aacv <- calculate_aacv(rowSums(om_rep$SSB))
      
      # Fbar variation (global is last col)
      fbar_aacv <- calculate_aacv(om_rep$Fbar[, ncol(om_rep$Fbar)])
      
      res_list[[i]] <- data.frame(
        Model = paste0("Model", i),
        Catch = catch_aacv,
        SSB = ssb_aacv,
        Fbar = fbar_aacv
      )
    }
    
  } else {
    n_fleets <- ncol(mods[[1]][[1]]$om$rep$pred_catch)
    n_regions <- ncol(mods[[1]][[1]]$om$rep$SSB)
    
    for (r in seq_along(mods)) {
      for (m in seq_along(mods[[r]])) {
        om_rep <- mods[[r]][[m]]$om$rep
        
        # Catch variation
        catch_aacv <- calculate_aacv(rowSums(om_rep$pred_catch))
        
        # SSB variation
        ssb_aacv <- calculate_aacv(rowSums(om_rep$SSB))
        
        # Fbar variation (global is last col)
        fbar_aacv <- calculate_aacv(om_rep$Fbar[, ncol(om_rep$Fbar)])
        
        res_list[[length(res_list) + 1]] <- data.frame(
          Model = paste0("Model", m),
          Realization = r,
          Catch = catch_aacv,
          SSB = ssb_aacv,
          Fbar = fbar_aacv
        )
      }
    }
  }
  
  res_all <- bind_rows(res_list)
  
  # --- Take median across realizations if needed ---
  df_all <- res_all %>%
    group_by(Model) %>%
    summarise(
      Catch = median(Catch),
      SSB = median(SSB),
      Fbar = median(Fbar),
      .groups = "drop"
    )
  
  # Rename models if user provided new names
  if (!is.null(new_model_names)) {
    if (length(new_model_names) != length(unique(df_all$Model))) {
      stop("Length of new_model_names must match the number of models.")
    }
    df_all$Model <- factor(df_all$Model,
                           levels = paste0("Model", seq_along(new_model_names)),
                           labels = new_model_names)
  }
  
  # --- Normalize & invert scores (higher variation is BAD) ---
  for (v in c("Catch", "SSB", "Fbar")) {
    range_v <- max(df_all[[v]]) - min(df_all[[v]])
    if (range_v == 0) {
      df_all[[paste0(v, "_score")]] <- 100 # if no variation, give full score
    } else {
      norm_v <- (df_all[[v]] - min(df_all[[v]])) / range_v
      df_all[[paste0(v, "_score")]] <- 100 * (1 - norm_v)
    }
  }
  
  # --- Plot triangle using inverted scores ---
  colors <- viridisLite::viridis(n = length(unique(df_all$Model)), option = col.opt)
  
  p <- ggtern(df_all, aes(x = Catch_score, y = SSB_score, z = Fbar_score, color = Model)) +
    geom_point(alpha = 0.8, size = 8) +
    scale_color_manual(values = colors) +
    labs(title = "Average Annual Variation in Catch, SSB, and F\nHigher Score = Lower Variation",
         T = "Catch",
         L = "SSB",
         R = "Fbar") +
    theme_rgbw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Save & print
  ggsave(filename = file.path(main.dir, sub.dir, "model_performance_AAV.png"),
         plot = p, width = width, height = height, dpi = dpi)
  
  print(p)
  
  # --- Normalize & invert scores (higher variation is BAD) ---
  for (v in c("Catch", "SSB", "Fbar")) {
    range_v <- max(res_all[[v]]) - min(res_all[[v]])
    if (range_v == 0) {
      res_all[[paste0(v, "_score")]] <- 100 # if no variation, give full score
    } else {
      norm_v <- (res_all[[v]] - min(res_all[[v]])) / range_v
      res_all[[paste0(v, "_score")]] <- 100 * (1 - norm_v)
    }
  }
  
  p2 <- ggtern(res_all, aes(x = Catch_score, y = SSB_score, z = Fbar_score, color = Model)) +
    geom_point(size = 1) +
    scale_color_manual(values = colors) +
    labs(title = "Average Annual Variation in Catch, SSB, and F\nHigher Score = Lower Variation",
         T = "Catch",
         L = "SSB",
         R = "Fbar") +
    theme_rgbw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_confidence_tern(breaks = 0.95)
  
  print(p2)
  # Save & print
  ggsave(filename = file.path(main.dir, sub.dir, "model_performance_AAV_raw.png"),
         plot = p2, width = width, height = height, dpi = dpi)
  
}