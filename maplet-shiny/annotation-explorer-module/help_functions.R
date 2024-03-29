# Extract the object names from result SE "D"------------
get_obj_name <- function(D){
  obj_list <- data.frame()
  for (i in seq_along(metadata(D)$results)) {
    for (j in seq_along(metadata(D)$results[[i]]$fun)) {
      obj_list[i, j] <- metadata(D)$results[[i]]$fun[j]
    }
  }
  
  # Extract the stat_name
  stat_name <- data.frame(stat_name=NA)
  for (i in seq_along(metadata(D)$results)) {
    stat_name[i, 1] <- if ('stat_name' %in% names(metadata(D)$results[[i]]$args)) {
      metadata(D)$results[[i]]$args$stat_name
    } else if ('stat_list' %in% names(metadata(D)$results[[i]]$args) & class(metadata(D)$results[[i]]$args$stat_list)!="call") {
      metadata(D)$results[[i]]$args$stat_list
    } else {
      NA
    }
  }
  
  # Merge object names and stat_name
  order_id <- 1:nrow(obj_list)
  obj_name <- cbind(order_id, obj_list, stat_name) 
  obj_name$stat_name <- 
    ifelse(is.na(obj_name$stat_name), 
           "(no stat_name)", 
           obj_name$stat_name)
  
  # count number of objects in the list of output of each metadata
  count_list <- c()
  for (i in seq_along(metadata(D)$results)){
    count_list[i] <- length(metadata(D)$results[[i]]$output)
  }
  obj_name$cnt <- count_list
  return(obj_name)
}

# get pathway annotations
get_pathway_annotations <- function(D, pwvar) {
  rd <- rowData(D) %>% as.data.frame %>%
    dplyr::mutate(name = rownames(rowData(D))) %>%
    dplyr::select(name,!!sym(pwvar), BIOCHEMICAL)
  if (class(rd[[pwvar]][1]) == "AsIs") {
    # remove rows with missing pathway annotations and unnest pathway column
    miss_idx <-
      apply(rd, 1, function(x) {
        x[[pwvar]][[1]] %>% is.null() %>% unname()
      })
    rd <- rd[!miss_idx, ] %>% tidyr::unnest(!!sym(pwvar))
    # extract pathway_name column form pathways data frame
    rd %<>% dplyr::left_join(metadata(D)$pathways[[pwvar]], by = setNames("ID", pwvar)) %>%
      dplyr::select(name, BIOCHEMICAL, pathway_name)
    # replace value for pathway column variable
    pwvar <- "pathway_name"
  }
  
  rd
}

# Get the data set -----------------------
get_data_by_name <- function(D, args.name, plot.nmae, stat.name) {
  plots <- mtm_res_get_entries(D, c("plots", plot.nmae))
  for (i in seq_along(plots)) {
    if (plots[[i]]$args[args.name] == stat.name) {
      data <- plots[[i]]$output[[1]]$data
    }
  }
  data
}


# create reverselog_trans for log10-SCALE in volcano plot-----------------
reverselog_trans <- function (base = exp(1)){
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(paste0("reverselog-", format(base)), trans, inv,
                    scales::log_breaks(base = base),
                    domain = c(1e-100, Inf))
}

# get the threshold for significance (extracted from corresponding stat_bar plot)
get_threshold_for_p_adj <- function(D, stat_name) {
  # define threshold for significance (extracted from corresponding stat_bar plot)
  stats_plots <- mtm_res_get_entries(D, c("plots", "stats"))
  for (plot in stats_plots) {
    if (plot$args$stat_list == stat_name) {
      alpha <- plot$args$feat_filter[3][[1]]
    }
  }
  alpha
}


# define PCA output function for mod3 referring 'mt_plots_pca'------------
mod3_plots_pca <- function(D, title = "PCA", 
                           ## scale argument
                           scale_data,
                           ## color argument
                           color,
                           categorizing,
                           pc1 = 1, pc2 = 2, 
                           data_type,
                           hover,
                           ...){
  X = t(assay(D))
  if (any(is.na(X))) 
    stop("Data matrix for PCA cannot contain NAs")
  if (!(data_type %in% c("scores", "loadings"))) 
    stop("Show must be either 'scores' or 'loadings'")
  # scale scores if scale checkbox=T
  if (scale_data) 
    X <- scale(X)
  pca <- stats::prcomp(x = as.matrix(X), center = F, scale = F)
  expvar <- (pca$sdev)^2/sum(pca$sdev^2)
  ## reactivate axis labels
  pc1name <- sprintf("PC%d (%.1f%%)", pc1, expvar[pc1] * 
                       100)
  pc2name <- sprintf("PC%d (%.1f%%)", pc2, expvar[pc2] * 
                       100)
  
  if (data_type == "scores") {
    df <- data.frame(x = pca$x[, pc1], 
                     y = pca$x[, pc2], 
                     colData(D)
    )
    ## reactivate plot title
    plot_title <- paste0(ifelse(scale_data,
                                "Scaled ", 
                                "Non-scaled "),
                         title, " - ", color)
    ## categorize coloring if color checkbox=T
    if(categorizing){
      df[, color] <- factor(df[, color])
    }
    # customize hover text
    # hover_text <- paste0(names(data.frame(colData(D)))[as.numeric(hover)], ": ", 
    #                     data.frame(colData(D))[[as.numeric(hover)]])
    # draw ggplot
    p <- ggplot(data = df, 
                do.call(aes_string, as.list(structure(c("x","y",color,hover), names = c("x","y","colour",hover))))
    ) + 
      geom_point() + 
      xlab(pc1name) + 
      ylab(pc2name) + 
      ggtitle(plot_title) +
      ## reactive legend title
      labs(color = color)
  } else {
    df = data.frame(x = pca$rotation[, pc1], 
                    y = pca$rotation[, pc2], 
                    rowData(D)
    )
    ## reactivate plot title
    plot_title <- paste0(ifelse(scale_data, 
                                "Scaled ", 
                                "Non-scaled "),
                         title)
    ## categorize coloring if color checkbox=T
    if(categorizing){
      df[, color] <- factor(df[, color])
    }
    # customize hover text
    # hover_text <- paste0(names(data.frame(rowData(D)))[as.numeric(hover)], ": ", 
    #                      data.frame(rowData(D))[[as.numeric(hover)]])
    # draw ggplot
    p <- ggplot(data = df, 
                do.call(aes_string, as.list(structure(c("x","y",color,hover), names = c("x","y","colour",hover))))
    ) + 
      geom_point() + 
      xlab(pc1name) + 
      ylab(pc2name) + 
      ggtitle(plot_title)
  }
  
  # draw plotly
  ggplotly(p, tooltip = c(color, hover)) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5, ## set position of legend
                         y = -0.2,
                         tracegroupgap = 5),
           autosize = TRUE
    )
}


# define UMAP output function for mod3 referring 'mt_plots_umap'---------------
mod3_plots_umap <- function (D, title = "UMAP", 
                             ## scale argument
                             scale_data,  
                             ## color argument
                             color,
                             categorizing,
                             n_neighbors, 
                             hover,
                             ...) {
  X <- t(assay(D))
  if (any(is.na(X))) 
    stop("Data matrix for UMAP cannot contain NAs")
  ## dependent on scale checkbox
  if (scale_data) 
    X <- scale(X)
  um <- umap::umap(d = as.matrix(X), n_neighbors = as.numeric(n_neighbors))
  df <- data.frame(x = um$layout[, 1], y = um$layout[, 2], colData(D))
  ## reactivate plot title
  plot_title <- paste0(ifelse(scale_data,
                              "Scaled ", 
                              "Non-scaled "), title, 
                       " with ", n_neighbors, 
                       " neighbors colored by ", color)
  ## categorize coloring if color checkbox=T
  if(categorizing){
    df[, color] <- factor(df[, color])
  }
  # customize hover text
  # hover_text <- paste0(names(data.frame(colData(D)))[as.numeric(hover)], ": ", 
  #                      data.frame(colData(D))[[as.numeric(hover)]])
  # draw ggplot
  p <- ggplot(data = df,
              do.call(aes_string, as.list(structure(c("x","y",color,hover), names = c("x","y","color","hover"))))
  ) + 
    geom_point() + 
    xlab("comp 1") + 
    ylab("comp 2") + 
    ggtitle(plot_title) +
    ## reactive legend title
    labs(color = color)
  
  # draw plotly
  ggplotly(p, tooltip = c(color, hover)) %>% 
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = .5,
                         y = -.2,
                         tracegroupgap = 5),
           autosize = TRUE
    )
  
}


# define boxplot function in module 5---------------
mod5_boxplot <- function(D, x, x_cate, y, y_cate, fill, hover, ...){
  df <- data.frame(colData(D))
  ## categorize variable if user think it's not continuous
  if(x_cate==FALSE){
    df[, x] <- factor(df[, x])
  }
  if(y_cate==FALSE){
    df[, y] <- factor(df[, y])
  }
  # to reproduce jitter plot
  set.seed(4017)
  p <- ggplot(df,
              do.call(aes_string, as.list(structure(c(x, y, fill, hover), names = c("x","y", "fill", "hover"))))
  ) + 
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = .2, alpha = 0.5) +
    theme(legend.title = element_blank()) +
    ggtitle("Boxplot with ignored outliers and Jitter with Set Seed")
  
  # get number of boxplots from plot object
  nbox <- p$data %>% dplyr::pull(p$mapping$x[[2]]) %>% unique %>% length
  
  # draw plotly
  fig <- ggplotly(p) %>%
    layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = -0.3))
  
  # remove outlier dots from boxplot layer (but not from jitter layer)
  lapply(1:nbox, function(i){
    fig$x$data[i] <<- lapply(fig$x$data[i], FUN = function(x){
      x$marker = list(opacity = 0)
      return(x)
    })
  })
  # return fig
  fig
}


# define scatterplot function in mod5------------------
mod5_scatter <- function(D, x, y, hover, ...){
  df <- data.frame(colData(D))
  p <- ggplot(df,
              do.call(aes_string, as.list(structure(c(x, y, hover), names = c("x","y", "hover"))))
  ) + geom_point()
  
  ggplotly(p)
}


# define barplot function in mod5--------------------
mod5_barplot <- function(D, x, fill, hover, ...){
  df <- data.frame(colData(D))
  p <- ggplot(df,
              do.call(aes_string, as.list(structure(c(x, fill, hover), names = c("x","fill", "hover"))))
  ) + geom_bar()
  
  ggplotly(p) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5, ## set position of legend
                         y = -0.2,
                         tracegroupgap = 5),
           autosize = TRUE
    )
}

# SE generating function for Mod6
diff_analysis_func <- function(D,
                               var,
                               binary=F,
                               analysis_type="lm",
                               mult_test_method="BH",
                               alpha=0.05,
                               group_col_barplot,
                               color_col_barplot=NULL){
  D %<>%
    mt_reporting_heading(heading = sprintf("%s Differential Analysis",var), lvl = 2) %>%
    {.}
  
  if(analysis_type=="lm"){
    D %<>%
      mt_stats_univ_lm(formula = as.formula(sprintf("~  %s",var)), stat_name = sprintf("%s analysis",var)) %>%
      {.}
  }else{
    D %<>%
      mt_stats_univ_cor(in_col = var, stat_name = sprintf("%s analysis",var),method = analysis_type) %>%
      {.}
  }
  
  if(binary){
    D %<>%
      mt_post_fold_change(stat_name = sprintf("%s analysis",var))
  }
  D %<>%
    mt_post_multtest(stat_name = sprintf("%s analysis",var), method = mult_test_method) %>%
    mt_reporting_stats(stat_name = sprintf("%s analysis",var), stat_filter = p.adj < alpha) %>%
    mt_plots_volcano(stat_name = sprintf("%s analysis",var),
                     x = !!sym(ifelse(binary,"fc","statistic")),
                     feat_filter = p.adj < alpha,
                     color = p.adj < alpha) %>%
    mt_plots_box_scatter(stat_name = sprintf("%s analysis",var),
                         x = !!sym(var),
                         plot_type = ifelse(binary,"box","scatter"),
                         feat_filter = p.adj < alpha, 
                         feat_sort = p.value,
                         annotation = "{sprintf('P-value: %.2e', p.value)}\nP.adj: {sprintf('%.2e', p.adj)}") %>%
    mt_plots_stats_pathway_bar(stat_list = sprintf("%s analysis",var),
                               y_scale = "count",
                               feat_filter = p.adj < alpha,
                               group_col = group_col_barplot,
                               color_col = color_col_barplot) %>%
    {.}
  
  return(D)
}

# get the volcano plot in mod2--------------------
mod2_plot_vol <- function(D, inputs, legend_name, d, pwvar, alpha) {
  # Get volcano data set if the plot1 is null
  data_vol <- get_data_by_name(D, "stat_name", "volcano", inputs[1])
  
  # Get volcano data set if the plot1 is bar
  if (inputs[2] == "bar") {
    if (!is.null(d)) {
      # get the click information for the bar plot 
      data_bar <- get_data_by_name(D, "stat_list", "stats", inputs[1])
      lvls <- rev(levels(data_bar$label))
      label <- lvls[round(as.numeric(d$y))]
      sub_pathway_name <- data_bar[data_bar$label == label, ]$name
      # get the variable name by sub_pathway_name
      row_data <- rowData(D) %>% data.frame()
      names <- unlist(row_data[row_data[pwvar] == sub_pathway_name,]$name)
      data_vol <- data_vol[data_vol$name %in% names, ]
    }
  }
  # Set the legend color column
  data_vol[, legend_name] <- ifelse(data_vol$p.adj < alpha, TRUE, FALSE)
  
  # draw the plot2
  plot <- data_vol %>%
    ggplot(aes(x = statistic, y = p.value, color = !!sym(legend_name), label = name)) +
    geom_point() +
    scale_y_continuous(trans = reverselog_trans(10),
                       breaks = scales::trans_breaks("log10", function(x) 10^x),
                       labels = scales::trans_format("log10", scales::math_format(10^.x))) +
    labs(y = "p-value (10^(-y))") +
    ggtitle(paste0(inputs[1])) +
    scale_color_brewer(palette="Dark2")
  
  if (inputs[2] == "bar") {
    plot <- plot +
      geom_point(size = 3) +
      ggtitle(paste0(sub_pathway_name, "-", inputs[1]))
  }
  
  plot
}



# get the equalizer plot in mod2--------------------
mod2_plot_eq <- function(D, inputs, rd, alpha, pwvar, path_name, d) {
  # add pathway annotations to results
  res <- maplet::mtm_get_stat_by_name(D, inputs[1]) %>%
    dplyr::left_join(rd, by=c("var"="name")) %>%
    dplyr::mutate(x = sign(statistic)*log10(p.adj)) %>%
    dplyr::filter(!is.na(BIOCHEMICAL))
  # compute multiple testing correction line
  sel <- res %>% 
    dplyr::filter(p.adj < alpha)
  if(nrow(sel)>0){
    xfine <- sel %>% .$p.adj %>% max(., na.rm = T)
  } else {
    xfine <- Inf
  }
  
  # If plot1 is not bar
  if (inputs[2] == "null") {
    df <- res %>%
      dplyr::filter(!!sym(pwvar)==path_name)
  } else {
    if (!is.null(d)) {
      # get the click information for the bar plot
      data_bar <- get_data_by_name(D, "stat_list", "stats", inputs[1])
      lvls <- rev(levels(data_bar$label))
      label <- lvls[round(as.numeric(d$y))]
      sub_pathway_name <- data_bar[data_bar$label == label,]$name
      
      # create equalizer plots
      df <- res %>%
        dplyr::filter(!!dplyr::sym(pwvar) == sub_pathway_name)
    }
  }
  
  # colors
  clrs <- c("#9494FF","red")
  # x axis limits
  a = max(abs(df$x)) + 0.3
  plot <-
    ggplot(df, aes(x = x, y = BIOCHEMICAL)) +
    geom_vline(xintercept = 0, color = "gray") +
    (if (!is.infinite(xfine)) {
      geom_vline(
        xintercept = c(-log10(xfine), log10(xfine)),
        color = "red",
        alpha = 0.4
      )
    }) +
    geom_point(pch = 22,
               fill = clrs[1],
               size = 3) +
    facet_grid(as.formula(sprintf("%s~.", pwvar)),
               scales = "free_y",
               space = "free_y") +
    theme(
      strip.background = element_rect(fill = NA),
      strip.text = element_text(colour = 'black', face = "bold"),
      strip.text.y = element_text(angle = 0, hjust = 0),
      panel.grid.major.y = element_line(color = "gray"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = NA, color =
                                        "black")
    ) +
    ylab("") +
    xlab("sign(statistic)*log10(p.adj)") +
    scale_x_continuous(limits = c(-a, a))
  
  if (inputs[2] == "bar") {
    plot <- plot +
      (if (!is.infinite(xfine)) {
        ggtitle(paste0(sub_pathway_name, "-", inputs[1],"-",
                       sprintf("Differential Metabolites at alpha %.2f", alpha)))
      } else{
        ggtitle(sprintf("No significant results at alpha %.2f", alpha))
      })
  }
  
  plot
}


# get the box/scatter plot in mod2--------------------
mod2_plot_box_scatter <- function(D,
                                  inputs,
                                  d.bar,
                                  d.eq,
                                  d.vol,
                                  rd,
                                  pwvar,
                                  path_name,
                                  alpha,
                                  is_categorical,
                                  data) {
  
  # get the bar name if plot1 is selected "bar"
  if (inputs[2] == "bar") {
    if (!is.null(d.bar)) {
      data_bar <- get_data_by_name(D, "stat_list", "stats", inputs[1])
      lvls <- rev(levels(data_bar$label))
      label <- lvls[round(as.numeric(d.bar$y))]
      sub_pathway_name <- data_bar[data_bar$label == label,]$name
    }
  }
  
  # Get the metabolite name by click information
  if (inputs[3] == "equalizer") {
    if (!is.null(d.eq)) {
      res <- maplet::mtm_get_stat_by_name(D, inputs[1]) %>%
        dplyr::left_join(rd, by=c("var"="name")) %>%
        dplyr::mutate(x = sign(statistic)*log10(p.adj)) %>%
        dplyr::filter(!is.na(BIOCHEMICAL))
      
      # get the data set of the equalizer plot
      if (inputs[2] == "bar") {
        # Filter the data by selected pwvar
        df <- res %>%
          dplyr::filter(!!dplyr::sym(pwvar) == sub_pathway_name)
      } else {
        df <- res %>%
          dplyr::filter(!!dplyr::sym(pwvar) == path_name)
      }
      # get the metabolite name by click info for equalizer plot
      metabolite <- df[as.numeric(d.eq$pointNumber) + 1,]$var[1]
      term <- df$term[1]
    }
  } else { # get the click info in volcano plot
    if (!is.null(d.vol)) {
      data_vol <- get_data_by_name(D, "stat_name", "volcano", inputs[1])
      if (inputs[2] == "bar") {
        # get the variable name by sub_pathway_name
        row_data <- rowData(D) %>% data.frame()
        names <- unlist(row_data[row_data[pwvar] == sub_pathway_name,]$name)
        data_vol <- data_vol[data_vol$name %in% names, ]
      }
      # set the column curveNumber by color legend
      p.adj.significant <- alpha
      data_vol[, "curveNumber"] <- ifelse(data_vol$p.adj < as.numeric(p.adj.significant), 1, 0)
      data_vol_true <- data_vol[data_vol$curveNumber==1, ]
      data_vol_false <- data_vol[data_vol$curveNumber==0, ]
      # By using click info (curveNumber & ponitNumber) to get the metabolite name
      metabolite <- ifelse(d.vol$curveNumber == 1,
                           data_vol_true[d.vol$pointNumber + 1, ]$var[1],
                           data_vol_false[d.vol$pointNumber + 1, ]$var[1])
      term <- data_vol$term[1]
      
    }
  }
  
  # Filter the data by metabolite name
  data <- data[data$var == metabolite, ]
  
  # Treat as categorical or not?
  if (is_categorical) {
    data[, term] <- factor(data[, term])
  } else {
    data[, term] <- as.numeric(data[, term])
  }
  
  # Draw the plot3
  if (inputs[4] == "scatter") {
    plot <- data %>%
      ggplot(aes(x = !!sym(term), y = value)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", se = T, color = "black") + 
      ggtitle(metabolite)
  } else {
    plot <- data %>%
      ggplot(aes(x = !!sym(term), y = value)) +
      geom_boxplot() +
      geom_jitter(size = 3, width = 0.2) +
      ggtitle(metabolite)
  }
  
  plot
}


# get log text from a SE object
get_log_text <- function(D){
  dt <- metadata(D)$results
  text <- lapply(names(dt), function(x){
    sprintf("%s\n",dt[[x]]$logtxt)
  }) %>% {do.call(cat,.)}
  # return text
  text
}