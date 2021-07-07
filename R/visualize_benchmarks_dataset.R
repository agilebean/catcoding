# visualize final benchmarking
visualize_benchmarks_dataset <- function(
  dataset_label, metric, models_lists_dataset = NULL, 
  cv_repeats = CV.REPEATS, study = STUDY,
  palette = "Set1", boxfill = "slategray3",
  save = TRUE, width = 6, height = 6, dpi = 300) {
  
  models.lists.dataset <- if (!is.null(models_lists_dataset)) {
    models_lists_dataset
  } else {
    get_models_list_dataset(study, dataset_label, cv_repeats)
  }
  
  # return the sampling folds for the best algorithm
  sampling.folds <- models.lists.dataset %>% 
    # imap(~ mutate(.x, name = .y))
    map(~ get_sampling_models_list(.x, "RMSE")) %>% 
    # tricky tricky: concatenate the sampling folds for all best algorithms
    imap_dfc(~ set_names(.x, .y)) %>% 
    as_tibble() # %T>% print
  
  sampling.folds.ordered <- sampling.folds %>% 
    pivot_longer(
      cols = everything(), names_to = "encoder", values_to = metric
    ) %>% 
    arrange(RMSE) # %T>% print
  
  color.codes <- RColorBrewer::brewer.pal(8, palette)[-c(1:2)]
  color.values <- colorRampPalette(color.codes)(ncol(sampling.folds))
  
  plot.sampling.folds.ordered <- sampling.folds.ordered %>% 
    ggplot(aes(x = reorder(encoder, desc(RMSE)), y = RMSE)) +
    coord_flip() +
    # geom_boxplot(fill = "#778899") + # lightslategrey
    geom_boxplot(fill = boxfill) +
    # geom_point(aes(color = encoder), alpha = 0.25, size = 1.5) +
    # geom_point(aes(color = encoder), alpha = 1, size = 1.5, shape = 1) +
    geom_jitter(aes(color = encoder), alpha = 1, size = 0.5, shape = 1) +
    # scale_color_brewer(guide = "none", palette = palette) +
    scale_color_manual(guide = "none", values = color.values) +
    labs(
      title = paste("dataset:", dataset_label),
      x = "model",
      y = "RMSE"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  
  if (save) {
    plot.label <- benchmark_plot_label(study, cv_repeats, dataset_label) 
    ggsave(
      filename = plot.label, 
      plot = plot.sampling.folds.ordered,
      dpi = dpi, width = width, height = height)
    print("****************************************************")
    print("Saved benchmark plot under")
    print(plot.label)
    print("****************************************************")
  }
  
  return(plot.sampling.folds.ordered)
}