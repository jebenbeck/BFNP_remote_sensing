#' Converts a trajectory of GeoSLAM Scanner to geopackage
#' Works with the georeferenced trajectory file generated from GeoSLAM Connect

library(ggplot2)
library(dplyr)

#' Function:
traj_process <- function(input_txt, plot_id) {
  
  #' read txt to df: 
  data <- read.table(input_txt, header = TRUE, sep = " ", dec = ".") %>% 
    sample_n(10000) %>% 
    mutate(
      distance = sqrt(x^2 + y^2),
      azimuth_deg = (atan2(x, y) * 180 / pi) %% 360
    )
  
  #' plot the trajectory:
  
  ggplot(data, aes(x = azimuth_deg, y = distance, color = X..world_time)) +
    geom_point(size = 2) +
    scale_color_viridis_c(option = "plasma") +
    scale_x_continuous(breaks = seq(0, 360, by = 45),
                       limits = c(0, 360),
                       expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 22, by = 2),
                       limits = c(0, 22),
                       expand = c(0, 0)
                       ) +
    geom_hline(yintercept = c(5, 13), color = "black", linewidth = 1) +
    coord_polar(theta = "x", start = 0, direction = 1) +
    
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_blank()) +
    labs(title = plot_id)

}

traj_process("C:/ProgramData/LidarOs/projects/KI-Recover/g/fif_01092/tsc/ts/FIF_01092/traj.txt",
             plot_id = "FIF_01092")
