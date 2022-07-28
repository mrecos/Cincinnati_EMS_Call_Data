
grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = cinBoundary) +
               geom_sf(data = opioid, colour="darkred", size=0.1, show.legend = "point") +
               labs(title= "Opioid Overdose Cases in 2016",
                    subtitle = 'Cincinnnati, OH\n',
                    caption = 'Figure 1.1') ,
             
             ggplot() + 
               geom_sf(data = cinBoundary, fill = "#E5E5E5") +
               stat_density2d(data = data.frame(st_coordinates(opioid)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              bins = 40, geom = 'polygon') +
               scale_fill_viridis_c(option = "plasma") +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of opioid overdose Cases in 2016",
                    subtitle = 'Cincinnati, OH\n',
                    caption = 'Figure 1.2') +
               theme(legend.position = "none"))

ggplot() +
  geom_sf(data = opioid_net, aes(fill = countopioid), color = NA) +
  scale_fill_viridis() +
  labs(title = "Observed Opioid overdose Joined to Fishnet, 2016",
       subtitle = 'Cincinnati, OH\n',
       caption = 'Figure 2')

ggplot(data = buffersAndDose, aes(x = reorder(ENGINE,-counter), y = counter, fill = "#BD0026")) +
  geom_bar(stat = "identity", color = "black", size = 1) + 
  scale_fill_manual(values = c("#BD0026")) +
  labs(title = "Count of Overdose Incidents within a 1/4-Mile of Each Fire Station", 
       x = "Fire Station", y = "Count of Overdoses Within 1/4-Mile") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", size = 16), axis.title = element_text(face = "italic")) 

mapList <- list()
for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis_c(option = "plasma",
                         name = " ") +
    labs(title=i)}
do.call(grid.arrange,c(mapList, ncol =3, top = "Figure. 3.2.1 Risk Factors by Fishnet\n"))
















