#  Cargamos las Librerias ----------------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(ggnewscale)
library(raster)
library(extrafont)      # custom font
library(hrbrthemes)     # to use import_roboto_condensed()
library(ggthemes)
library(elevatr)
library(ggspatial)
library(tmap)
library(ggpubr)
library(ggrepel)
library(ggforce)
library(grid)
library(png)
library(ggimage)

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Ecuador         <- getData('GADM', country='Ecuador', level=0) %>% st_as_sf()
Peru         <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

library(rgbif)
Archilochus<- occ_search(scientificName="Oreotrochilus chimborazo")
Archilochus_alexandri <- subset(Archilochus$data , 
                                scientificName == "Oreotrochilus chimborazo (Delattre & Bourcier, 1846)")

img <- readPNG("PNG/Imagen.png", FALSE)
g <- rasterGrob(img, x = unit(0.8, "npc"),y = unit(0.2, "npc"), width = unit(0.2, "npc"))



SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="white", color="black", size=0.05)+
  geom_sf(data = Ecuador, fill="black", color="black", size=0.01)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))
SurA.grob  <- ggplotGrob(SurA)

library(RColorBrewer)
Final= ggplot() + 
  geom_sf(data = SurAmeric  , fill="white", color="black", size=0.05)+
  geom_sf(data = Peru       , fill="white", color="black", size=0.05)+
  geom_sf(data = Ecuador    , fill="gray", color="black")+
  geom_point(data = Archilochus_alexandri, aes( x=decimalLongitude, 
                                                y = decimalLatitude), size = 0.04)+
  geom_hex(data=Archilochus_alexandri,  aes(x=decimalLongitude, 
                                            y = decimalLatitude),
           
           size=0.01,
           bins=25)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-81.5 ,-75), ylim = c(-7  ,3),expand = FALSE)+
  scale_fill_gradientn(colours = brewer.pal(n=8,name="PiYG"),trans="sqrt",
                       breaks = c(10,20,30,40,50,60,80),
                       name="Número de registros \nOreotrochilus chimborazo", 
                       guide = guide_legend( keyheight = unit(2.5, units = "mm"), 
                                             keywidth=unit(10, units = "mm"), 
                                             label.position = "bottom", title.position = 'top', nrow=1)
  )+
  
  annotate("text", x = -81, y = 2, label="Mapa de distribucion de \nOreotrochilus chimborazo", 
           colour = "black", size=6, alpha=1, hjust=0, family="serif",
           fontface="italic") +
  theme_classic()+
  theme(
    legend.position = c(0.5, 0.09),
    legend.title=element_text(color="black", size=8, family="serif"),
    text = element_text(color = "#22211d"),
    
    axis.text.x  = element_text(face="bold", size=10,
                                family="serif"),
    axis.text.y  = element_text(angle = 90,face="bold",
                                family="serif",size=10),
    axis.title = element_text(face="bold"),
    legend.text=element_text(size=9, family="serif"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
    
    panel.background = element_rect(fill = "#cce3de"),
    plot.title = element_text(size= 13, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  ) +
  labs(title = '',  x = 'Longitud', y = 'Latitud')+
  annotate(geom = "text", x = -78, y = -1, hjust = 0, vjust = 1, 
           label = "Ecuador",
           size = 5, family="serif", color = "#606c38",  fontface="italic")+
  annotate(geom = "text", x = -78, y = 1.5, hjust = 0, vjust = 1, 
           label = "Colombia",size = 5, family="serif", color = "#606c38",  fontface="italic")+
  annotate(geom = "text", x = -76, y = -4, hjust = 0, vjust = 1, 
           label = "Peru",size = 5, family="serif", color = "#606c38",  fontface="italic")+
  
  annotate(geom = "text", x = -81, y = 0, hjust = 0, vjust = 1, angle = 70,
           label = "Oceano Pacifico",size = 4, family="serif", color = "#6b9080",  fontface="italic")+
  
  
  annotate(geom = "text", x = -81, y =-6.8, hjust = 0, vjust = 0, lineheight = .9,
           label = "Author: Gorky Florez (@gflorezc) Aprende R desde cero, Geometries: RStudio Data",
           size = 3, family = "serif", color = "grey50")+
  annotate(geom = "text", x = -76, y = -6.9, hjust = 0, vjust = 0,
           label = "2022", fontface = 2,
           size = 5, family = "serif", color = "#35978f")+
  annotation_custom(SurA.grob, xmin = -77, xmax = -75, ymin =1, ymax=3)+
  annotation_custom(g)
Final

ggsave(plot=Final,"Mapa/Mapa de dispersion.png",units = "cm",width = 21, #alto
       height = 29, #ancho
       dpi=1200)













