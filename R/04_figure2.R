#This script is to reproduce the Figure II: Global real rates of return

source("R/02_construct_returns.R")

library(ggplot2)
library(patchwork)

# Define the two samples
full_rows = data$sample
post1950_rows = data$sample & data$tp_post1950

# Mean real returns for the two samples
full_real_mean = colMeans(data[full_rows,return_vars]) * 100
post1950_real_mean = colMeans(data[post1950_rows,return_vars]) * 100

# Create the plotting data for Figure II

# Asset levels from bottom to top on the y-axis
asset_names = c("Bills","Bonds","Equity","Housing")

# Create the plotting dataset
figure2_data = data.frame(
  sample = rep(c("Full sample","Post-1950"),each = 4),
  asset = rep(asset_names,2),
  mean_return = as.numeric(c(full_real_mean,post1950_real_mean)),
  bills_return = c(rep(full_real_mean[1],4),rep(post1950_real_mean[1],4))
)

# Calculate the excess returns relative to bills
figure2_data$excess_return = figure2_data$mean_return-figure2_data$bills_return

# Fix the order of samples and assets
figure2_data$sample = factor(
  figure2_data$sample,
  levels = c("Full sample","Post-1950")
)

figure2_data$asset = factor(
  figure2_data$asset,
  levels = asset_names
)

View(figure2_data)

# Draw Figure II
figure2_plot <- ggplot(
  figure2_data,
  aes(y = asset)
) +
  
  geom_col(
    aes(
      x = mean_return,
      fill = "Excess Return vs Bills"
    ),
    width = 0.34
  ) +

  geom_col(
    aes(
      x = bills_return,
      fill = "Bills"
    ),
    width = 0.34
  ) +
  
  geom_hline(
    yintercept = c(2, 3, 4),
    linetype = "dotted",
    color = "black",
    linewidth = 0.4
  ) +
  
  geom_point(
    data = subset(
      figure2_data,
      asset != "Bills"
    ),
    aes(
      x = mean_return,
      shape = "Mean Annual Return"
    ),
    color = "#FF7F00",
    size = 3
  ) +
  
  scale_fill_manual(
    name = NULL,
    values = c(
      "Bills" = "#006400",
      "Excess Return vs Bills" = "#6BBCE3"
    ),
    breaks = c(
      "Bills",
      "Excess Return vs Bills"
    )
  ) +
  
  scale_shape_manual(
    name = NULL,
    values = c(
      "Mean Annual Return" = 18
    )
  ) +
  
  guides(
    fill = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  ) +
  
  facet_wrap(
    ~ sample,
    nrow = 1
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 8, by = 2),
    limits = c(0, 8.6),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  
  labs(
    x = "Mean annual return, per cent",
    y = NULL,
    
    title = expression(
      bold("Figure II:") ~
        italic("Global real rates of return")
    ),
    
    caption = paste0(
      "Notes: Arithmetic average real returns p.a., unweighted, 16 countries. ",
      "Consistent coverage within each country: each\n country-year observation ",
      "used to compute the average has data for all four asset returns."
    )
  ) +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16
    ),
    
    plot.caption = element_text(
      hjust = 0,
      size = 10
    ),
    
    strip.background = element_blank(),
    strip.text = element_text(
      size = 13
    ),
    
    aspect.ratio = 1,
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    
    # Legend
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.box.background = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
  )

figure2_plot

ggsave(
  filename = "output/figure2_global_real_rates_of_return.png",
  plot = figure2_plot,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)

file.exists(
  "output/figure2_global_real_rates_of_return.png"
)

