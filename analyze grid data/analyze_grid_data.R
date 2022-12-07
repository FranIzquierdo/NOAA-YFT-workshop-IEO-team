# Clean workspace:
rm(list = ls())

# Set working directory
#mainDir = 'C:/Users/moroncog/Documents/StockAssessmentModels/SpatialStockAssessmentGroup'
#setwd(mainDir)

# Libraries:
require(dplyr)
require(tidyr)
library(rnaturalearth)
library(rnaturalearthdata)
require(reshape2)
require(ggplot2)
require(scatterpie)
require(gridExtra)
# -------------------------------------------------------------------------
# Read data from Github:
this_url = "https://github.com/aaronmberger-nwfsc/Spatial-Assessment-Modeling-Workshop/blob/main/data/YFT_221cell_observations_1-100_ESS_00x.RData"
mydata = Rfssa::load_github_data(github_data_url = this_url)


# CPUE data ---------------------------------------------------------------
save_total = list()
for(i in seq_along(mydata)) {
  
  this_data = get(mydata[i])

  Lat_grid = data.frame("lat" = paste0("r",as.character(seq(1,13))),
                        "Lat" = as.numeric(this_data$layers$`layer[latitude]`$data[,1]))
  Lon_grid = data.frame("lon" = paste0("c",as.character(seq(1,17))),
                        "Lon" = as.numeric(this_data$layers$`layer[longitude]`$data[1,]))
  region_matrix = this_data$layers$`layer[region]`$data
  rownames(region_matrix) = paste0("r",as.character(seq(1,13)))
  colnames(region_matrix) = paste0("c",as.character(seq(1,17)))
  region_info = melt(region_matrix)
  region_info$index = paste0(region_info$Var1, '-', region_info$Var2)
  # Create master grid lon lat
  if(i == 1) {
    region_info$lat = Lat_grid$Lat[match(region_info$Var1, Lat_grid$lat)]
    region_info$lon = Lon_grid$Lon[match(region_info$Var2, Lon_grid$lon)]
    write.csv(region_info, 'master_grid_info.csv', row.names = FALSE)
  }
  
  # CPUE data:
  save_tmp = list()
  pos_cpue = grep(pattern = 'simulated_cpue_ll_jpn_', x = names(this_data$obs))
  for (y in seq_along(pos_cpue)) {
    
    tmp_data = this_data$obs[[pos_cpue[y]]]
    this_cpue_data = data.frame(year = as.numeric(tmp_data$data$year), 
                                LatLon = tmp_data$data$obs[,1],
                                cpue = as.numeric(tmp_data$data$obs[,2]))
    save_tmp[[y]] = this_cpue_data
    
    
  }
  
  all_cpue = dplyr::bind_rows(save_tmp)
  all_cpue$region = region_info$value[match(all_cpue$LatLon, region_info$index)]
  
  all_cpue2 = all_cpue %>%
                separate(LatLon, c("lat", "lon"), "-")
  all_cpue2$lat2 = Lat_grid$Lat[match(all_cpue2$lat, Lat_grid$lat)]
  all_cpue2$lon2 = Lon_grid$Lon[match(all_cpue2$lon, Lon_grid$lon)]
  all_cpue2$sim = mydata[i]
  
  save_total[[i]] = all_cpue2
}

out_cpue = dplyr::bind_rows(save_total)
write.csv(out_cpue, 'CPUE_grid_data.csv', row.names = FALSE)

# Catch data ---------------------------------------------------------------
save_total = list()
for(i in seq_along(mydata)) {
  
  this_data = get(mydata[i])
  
  lat_matrix = melt(this_data$layers$`layer[latitude]`$data)
  lon_matrix = melt(this_data$layers$`layer[longitude]`$data)
  region_matrix = this_data$layers$`layer[region]`$data
  rownames(region_matrix) = paste0("r",as.character(seq(1,13)))
  colnames(region_matrix) = paste0("c",as.character(seq(1,17)))
  region_info = melt(region_matrix)
  region_info$index = paste0(region_info$Var1, '-', region_info$Var2)
  
  # Catch data:
  save_tmp = list()
  pos_cpue = grep(pattern = 'fishing_', x = names(this_data$layers))
  for (k in seq_along(pos_cpue)) {
    
    this_name = names(this_data$layers)[pos_cpue[k]]
    fishery_name = strsplit(x = this_name, split = '_')[[1]][2]
    year = as.numeric(gsub(pattern = ']', replacement = '', x = strsplit(x = this_name, split = '_')[[1]][3]))
    tmp_data = this_data$layers[[pos_cpue[k]]]
    catch_data = melt(tmp_data$data)
    this_cpue_data = data.frame(catch = catch_data$value,
                                lon = lon_matrix$value,
                                lat = lat_matrix$value,
                                year = year,
                                fishery = fishery_name,
                                region = region_info$value)
    save_tmp[[k]] = this_cpue_data
    
  }
  
  all_cpue = dplyr::bind_rows(save_tmp)
  all_cpue$sim = mydata[i]
  
  save_total[[i]] = all_cpue
  print(i)
}

out_catch = dplyr::bind_rows(save_total)

write.csv(out_catch, 'Catch_grid_data.csv', row.names = FALSE)


# -------------------------------------------------------------------------
# Len comp data

# Read polygons:
grid_info = read.csv('CPUE_standardization/data/master_grid_info.csv')
## world map:
map_world_df <- map_data('world', wrap=c(0, 360)) %>%
  dplyr::filter(region != "Antarctica")
country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x=long, y = lat, group = group),
                                fill = "gainsboro",
                                color = "gainsboro",
                                linewidth = 0.15)


# Create object to plot:
this_data = get(mydata[1]) # select replicate
# select fishery: ps, trol, bb, gill, ll, hand, other
fish_pattern = c('lf_ps', 'lf_trol', 'lf_bb', 'lf_gill', 'lf_ll', 'lf_hand', 'lf_other')
fish_title = c('PS fishery', 'TROL fishery', 'BB fishery', 'GILL fishery', 'LL fishery', 
               'HAND fishery', 'OTHER fishery')

# to save plots:
save_plots = list()
for(j in seq_along(fish_pattern)) {

  find_fish = grep(pattern = fish_pattern[j], x = names(this_data$obs))
  mainTitle = fish_title[j]
  
  save_data = list()
  for(k in seq_along(find_fish)) {
    
    lendata = this_data$obs[[find_fish[k]]]$data$obs # select fishery: ps, trol, bb, gill, ll, hand, other
    if(is.vector(lendata)) lendata = matrix(lendata, nrow = 1)
    
    # Make plot:
    lendata2 = lendata[,-1]
    if(is.vector(lendata2)) lendata2 = matrix(lendata2, nrow = 1)
    rownames(lendata2) = lendata[,1]
    colnames(lendata2) = this_data$obs$simulated_lf_ps_121$data$length_bins[-1]
    lendata3 = setNames(melt(lendata2), c('grid','len_bin', 'prop'))
    lendata3$len_bin = as.numeric(lendata3$len_bin)
    lendata3$prop = as.numeric(lendata3$prop)
    lendata3$grid = as.character(lendata3$grid)
    # Find lat lon information:
    lendata3$lat = grid_info$lat[match(lendata3$grid, grid_info$index)]
    lendata3$lon = grid_info$lon[match(lendata3$grid, grid_info$index)]
    # len categories:
    lendata3$len_cat = cut(lendata3$len_bin, breaks = c(0, 60, 120, 200))
    lendata3$year = k
    plot_data = lendata3 %>%
                  group_by(year, grid, lon, lat, len_cat) %>%
                  summarise(prop = mean(prop))
    save_data[[k]] = plot_data
  
  }
  
  plot_data = bind_rows(save_data)
  plot_data = plot_data %>%
    group_by(lon, lat, len_cat) %>%
    summarise(prop = mean(prop))
  plot_data2 = spread(data = plot_data, key = c('len_cat'), value = 'prop')
  
  if(j == 2) {
  save_plots[[j]] = ggplot() + 
                      country_shapes +
                      geom_scatterpie(data = plot_data2, aes(x=lon, y=lat, r = 2),
                                      cols = colnames(plot_data2)[3:5], legend_name = "Size_range") +
                      coord_cartesian(xlim = c(20, 105), ylim = c(-40, 25)) +  
                      scale_x_continuous(breaks = c(20, 60, 100), 
                                         labels = c('20\u00B0E', '60\u00B0E', '100\u00B0E')) +
                      scale_y_continuous(breaks = c(-40, -20, 0, 20), 
                                         labels = c('40\u00B0S', '20\u00B0S', '0\u00B0', '20\u00B0N')) +
                      xlab(NULL) +
                      ylab(NULL) +
                      theme_bw() +
                      theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            axis.line = element_line(linewidth = 0.5, linetype = "solid",
                                                     colour = "black"),
                            legend.background = element_rect(fill = "transparent"),
                            legend.position = c(0.7,0.25)) +
                      annotate(geom = 'text', label = mainTitle, x = -Inf, y = Inf, hjust = 0, vjust = 1)
  } else {
    save_plots[[j]] = ggplot() + 
      country_shapes +
      geom_scatterpie(data = plot_data2, aes(x=lon, y=lat, r = 2),
                      cols = colnames(plot_data2)[3:5], legend_name = "Size_range") +
      coord_cartesian(xlim = c(20, 105), ylim = c(-40, 25)) +  
      scale_x_continuous(breaks = c(20, 60, 100), 
                         labels = c('20\u00B0E', '60\u00B0E', '100\u00B0E')) +
      scale_y_continuous(breaks = c(-40, -20, 0, 20), 
                         labels = c('40\u00B0S', '20\u00B0S', '0\u00B0', '20\u00B0N')) +
      xlab(NULL) +
      ylab(NULL) +
      theme_bw() +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(linewidth = 0.5, linetype = "solid",
                                     colour = "black"),
            legend.background = element_rect(fill = "transparent"),
            legend.position = 'none') +
      annotate(geom = 'text', label = mainTitle, x = -Inf, y = Inf, hjust = 0, vjust = 1)
  }

}

png(filename = 'figures/size_spatial.png', width = 250, height = 200, 
    units = 'mm', res = 500)
do.call("grid.arrange", c(save_plots, ncol = 3))
dev.off()
