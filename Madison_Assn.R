# December 6, 2022

## Madison Heat Map assignment ##


# Download the following libraries:
library(stats)
library(ggplot2)


# Import the required dataset for the assignment 
heat_plot <- read.csv("MAH_assignment_data.csv")


# Give the data and set the x and y values; then specify what to fill the heat map with
ggplot(heat_plot, aes(x = Sample, y = Gene, fill = Expression)) +
  geom_tile(colour="black", linewidth=0.5)+ # specify the tile color and the width of the lines
  scale_fill_gradient(low = "black", high = "light blue")+ # low expression color = black, and high expression = light blue
  theme_grey(base_size=8)+ # change the theme and size of the x and y axis text
  facet_grid(~ Tissue, switch = "x", scales = "free_x", space = "free_x") + # make into a tissue specific heat map (facet by tissue type)
  # free_x (scale) removes columns with no data and free_x (space) makes the columns the same width
  ggtitle(label = "Expression of Pheromone Genes") + # names the title of the figure
  scale_x_discrete(labels=c('A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez', # the species for each tissue group
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez',
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez',
                            'A.tri', 'D.brim', 'E.tyn M', 'E.tyn P', 'P.alb', 'R.kez'))+
  theme(plot.title = element_text(face="bold"), # bold the title
        strip.placement = "outside", # puts the facet label outside the x axis
        strip.text = element_text(face = "bold"), # bolds the text
        legend.title = element_text(face = "bold"), # bolds
        axis.title = element_text(face="bold"), # bolds
        axis.title.x = element_blank(),
        axis.text.y =element_text(color = "black"), # text color is black
        axis.text.x =element_text(angle = 315, hjust = 0, vjust = 0.5, color = "black"),
        # changes the angle of the axis labels, position, and color
        axis.ticks=element_blank(), # removes tick marks and all of the gglot background aesthetics
        plot.background=element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank())
