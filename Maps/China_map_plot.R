library(sf)
library(tidyverse)
library(ggspatial)
library(RColorBrewer)
library(ggtext)
library(showtext)


china_shp <- "中国省级地图GS（2019）1719号.geojson"
nine <- "九段线GS（2019）1719号.geojson"

china <- sf::read_sf(china_shp)
nine_line <- sf::read_sf(nine)

#初步绘图
library(ggspatial)
ggplot() + 
  geom_sf(data = china,fill="NA",size=1,color="black") + 
  geom_sf(data = nine_line) + 
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  annotation_scale(location = "bl") +
      # spatial-aware automagic north arrow
       annotation_north_arrow(location = "tl", which_north = "false",
                             style = north_arrow_fancy_orienteering)
							 
							 
							 
							 
							 
#读取绘制数据
library(openxlsx) #
file <- "LONLAT.xlsx"
scatter_df <- read.xlsx(file)
head(scatter_df)

#需要对数据进行投影转换
scatter_df_tro <- st_as_sf(scatter_df,coords = c("lon", "lat"),crs = 4326)

#plot
ggplot() + 
  geom_sf(data = china,fill="NA",size=.5,color="black") + 
  geom_sf(data = nine_line) + 
  geom_sf(data = scatter_df_tro,aes(fill=class,size=data),shape=21,colour='black',stroke=.25)+
  coord_sf(crs = "+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c("#E21C21","#3A7CB5","#51AE4F"))+
  scale_size(range = c(1,5))+
  annotation_scale(location = "bl") +
      # spatial-aware automagic north arrow
       annotation_north_arrow(location = "tl", which_north = "false",
                             style = north_arrow_fancy_orienteering)+
  theme_linedraw()+
  theme(text = element_text(family = 'Times_New_Roman',size = 12,face = "bold"),
        panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = "grey50"),
        #axis.line = element_line(size = 1, colour = "black"),
        panel.ontop = FALSE
        )

#添加南海小地图范围

tibble(lon = c(105,125),
       lat = c(3,25),
       ) -> df2

df2 %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) -> df2_sf

df2_sf_pre <- sf::st_transform(df2_sf,crs="+proj=laea +lat_0=40 +lon_0=104")

#plot
nine_map <- ggplot() +
  geom_sf(data = china,fill='NA') + 
  geom_sf(data = nine_line,color='gray70',size=1.)+
  #geom_sf(data = scatter_df_tro,aes(fill=class,size=data),shape=21,colour='black',stroke=.25)+
  coord_sf(ylim = c(-4028017,-1877844),xlim = c(117131.4,2115095),crs="+proj=laea +lat_0=40 +lon_0=104")+
  theme(
    #aspect.ratio = 1.25, #调节长宽比
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=1.),
    plot.margin=unit(c(0,0,0,0),"mm"))
nine_map


#cowplot包子图添加
library(cowplot)

map <- ggplot() + 
  geom_sf(data = china,fill=NA) + 
  geom_sf(data = nine_line,color='gray50',size=.8)+
  geom_sf(data = scatter_df_tro,aes(fill=class,size=data),shape=21)+
  coord_sf(ylim = c(-2387082,1654989),crs="+proj=laea +lat_0=40 +lon_0=104")+
  scale_fill_manual(values = c("#E21C21","#3A7CB5","#51AE4F"))+
  scale_size(range = c(1,5))+
  annotation_scale(location = "bl",text_face = "bold",
                   text_family = "Times_New_Roman") +
      # spatial-aware automagic north arrow
       annotation_north_arrow(location = "tl", which_north = "false",
                             style = north_arrow_fancy_orienteering,
                            )+
  #定制化图例：这一步可以设计出亮眼的图例哦
  guides(fill = guide_legend(override.aes = list(size = 3),
                             title = "",
                             label.position = "right",
                             ncol=3,
                             ),
         size = guide_legend(
                             title = "",
                             label.position = "right",
                             ncol=5)) +
 labs(
          caption = 'Visualization by DataCharm')+
 #theme_bw()+
 theme(
     text = element_text(family = 'Times_New_Roman',size = 18,face = "bold"),
     
     panel.background = element_rect(fill = NA),
     panel.grid.major = element_line(colour = "grey80",size=.2),
     legend.key = element_rect(fill = "white"),
     legend.position = "bottom",
 )

nine_map <- ggplot() +
  geom_sf(data = china,fill='NA') + 
  geom_sf(data = nine_line,color='gray70',size=1.)+
  #geom_sf(data = scatter_df_tro,aes(fill=class,size=data),shape=21,colour='black',stroke=.25)+
  coord_sf(ylim = c(-4028017,-1877844),xlim = c(117131.4,2115095),crs="+proj=laea +lat_0=40 +lon_0=104")+
  theme(
    #aspect.ratio = 1.25, #调节长宽比
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill=NA,color="grey10",linetype=1,size=1.),
    plot.margin=unit(c(0,0,0,0),"mm"))


gg_inset_map = ggdraw() +
  draw_plot(map) +
  draw_plot(nine_map, x = 0.8, y = 0.15, width = 0.1, height = 0.3)

gg_inset_map

