
library(sp)
library(sf)
library(tidyverse)
library(tmap)
library(rgdal)
library(plotrix)  
library(georob)
library(ggforce)  
library(spatstat)
library(maptools)
library(raster)
library(foreign)
library(gstat)
library(ggpubr)
library(stars)
library(raster)
library(viridis)
require(RColorBrewer)
require(lattice)

setwd("C:/Users/fabia/OneDrive/Documentos/Fabian/Estadística/2019/II Ciclo/Estadística Espacial/Proyectos/3")

#Datos======================================================================

# arboles.df <- read.csv("arbolado-urbano.csv", sep = ";", dec = ".") %>% 
#   filter(Clasificación == "Árbol", DAP != 9.999)

load("arboles.RData")

arboles.sf <- st_as_sf(arboles.df, 
                       coords = c("X.crtm", "Y.crtm"),
                       crs = "+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +units=m +no_defs")

mapa.sf <- st_read("CR_Distritos.shp") %>% filter(nom_cant == "Belén")

#En el paquete sp
arboles.sp <- arboles.df
coordinates(arboles.sp) <- c("X.crtm", "Y.crtm")
proj4string(arboles.sp) <- CRS("+proj=utm +zone=10+datum=WGS84")
arboles.sp <- spTransform(arboles.sp, CRS("+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

#Estadísticas Descriptivas==============================================================

#Cantidad de Árboles por distrito
arboles.df %>% 
  group_by(Distrito) %>% 
  summarize(Cantidad = n()) %>% 
  mutate(Total = sum(Cantidad)) %>% 
  mutate(Proporción = Cantidad / Total) %>% 
  dplyr::select(Distrito, Cantidad, Proporción)

#Cantidad de Árboles por estado fitosanitario
arboles.df %>% 
  group_by(Estado.Fitosanitario) %>% 
  summarize(Cantidad = n()) %>% 
  mutate(Total = sum(Cantidad)) %>% 
  mutate(Proporción = Cantidad / Total) %>% 
  dplyr::select(Estado.Fitosanitario, Cantidad, Proporción)

#Cantidad de Árboles por procedencia
arboles.df %>% 
  group_by(Procedencia) %>% 
  summarize(Cantidad = n()) %>% 
  mutate(Total = sum(Cantidad)) %>% 
  mutate(Proporción = Cantidad / Total) %>% 
  dplyr::select(Procedencia, Cantidad, Proporción)

#Gráficos Descriptivos===============================================================================

#Gráficos de caja 
##Tamaño del árbol por distrito
arboles.df %>% 
  ggplot(aes(y = H, fill = Distrito)) + geom_boxplot(alpha = 1 / 2) + 
  theme(legend.position = "bottom")

##DAP del árbol por distrito
arboles.df %>% 
  ggplot(aes(y = DAP, fill = Distrito)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

##Tamaño del árbol por estado fitosanitaria
arboles.df %>% 
  ggplot(aes(y = H, fill = Estado.Fitosanitario)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

arboles.df %>% 
  ggplot(aes(y = DAP, fill = Estado.Fitosanitario)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

##Procedencia
arboles.df %>% 
  ggplot(aes(y = H, fill = Procedencia)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

arboles.df %>% 
  ggplot(aes(y = DAP, fill = Procedencia)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

##Por sitio
arboles.df %>% 
  ggplot(aes(y = H, fill = Sitio)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

arboles.df %>% 
  ggplot(aes(y = DAP, fill = Sitio)) + geom_boxplot(alpha = 1 /2) + 
  theme(legend.position = "bottom")

#Mapas============================================================= 
arboles.p <- mapa.sf %>%
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(data = arboles.sf, aes(colour = H))  + 
  theme(legend.position = "none")

arboles.grid <- mapa.sf %>%
  st_make_grid(cellsize = 150) %>%
  st_intersection(mapa.sf) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number()) %>% 
  st_join(arboles.sf, by = "geometry")

arboles.grid.p <- grid %>% ggplot() + geom_sf() + geom_sf(aes(fill = H))
ggarrange(arboles.p, arboles.grid.p, ncol = 2, common.legend = T, legend = "bottom") %>% 
  annotate_figure(top = text_grob("Gráfico 1: Mapa de los árboles del cantón de Belén", 
                                  face = "bold", size = 14))

#Correlación entre altura y DAP

graf2.scatterplot <- arboles.df %>% 
  ggplot(aes(DAP, H)) + geom_point(alpha = 1 / 3) + 
  geom_smooth(method = "lm", se = F, colour = "blue") + 
  ggtitle(label = "Gráfico 2: H vs DAP") + 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey"), 
        panel.grid = element_line(colour = "grey92"))

cor(arboles.df$DAP, arboles.df$H)

#Modelo lineal=================================================

mod.lm <- lm(H ~ DAP, data = arboles.sp)
summary(mod.lm)
arboles.sp$estimados <- mod.lm$fitted.values
arboles.sp$residuos.lm <- mod.lm$res
arboles.sp$estimados.lm <- mod.lm$fitted

#Evaluación del Residuo

est.vs.pred <- as.data.frame(arboles.sp) %>% 
  ggplot(aes(y = scale(residuos.lm), x = estimados.lm)) + geom_point() + theme_bw() + 
  ggtitle("Residuos vs Estimados")

norm.res <- as.data.frame(arboles.sp) %>% 
  ggplot(aes(sample = residuos.lm)) + stat_qq() + stat_qq_line() + theme_bw() +
  ggtitle("QQplot")
  
v.res <- variogram(residuos.lm ~ 1, arboles.sp) %>% 
  ggplot(aes(x = dist, y = gamma)) + geom_point() +
  ylim(0, 25) + 
  ylab("Semivarianza") + theme_bw() +
  ggtitle("Semivariograma")

grid.arrange(est.vs.pred, norm.res, v.res, ncol = 3) %>% 
  annotate_figure(top = text_grob("Gráficos 3: Análisis del Residuo del modelo lineal H ~ DAP", 
                                  face = "bold", size = 14))

#Variograma====================================================

v <- variogram(H ~ DAP, arboles.sp)
v$tipo <- "Observado"
v %>% 
  ggplot(aes(x = dist, y = gamma)) + geom_point() + geom_line() + 
  ylab("Semivarianza") + 
  ylim(0, 25) + 
  ggtitle("Gráfico 4: Semivariograma de H ~ DAP") + 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey"), 
        panel.grid = element_line(colour = "grey92"))

#Estimación de los variogramas
vgm.fit <- fit.variogram(v, vgm(50, "Exp", 1000, 10))
vgm.exp <- variogramLine(vgm(13.586, "Exp", 277.12, 7.933), 2500)
vgm.exp$tipo <- "Exp"
v.grap <- bind_rows(dplyr::select(v, dist, gamma, tipo), vgm.exp)

vgm.fit <- fit.variogram(v, vgm(50, "Sph", 1000, 10))
vgm.sph <- variogramLine(vgm(11.9638, "Sph", 700.8334, 9.3550), 2500)
vgm.sph$tipo <- "Sph"
v.grap <- bind_rows(v.grap, vgm.sph)

fit.variogram(v, vgm(24.9962, "Ste", 298.72, 1000))
vgm.ste <- variogramLine(vgm(13.58656, "Ste", 391.7264, 7.933, kappa = 0.5), 2500)
vgm.ste$tipo <- "Ste"
v.grap <- bind_rows(v.grap, vgm.ste)

fit.variogram(v, vgm(24.9962, "Mat", 298.72, 1000))
vgm.mat <- variogramLine(vgm(13.58656, "Ste", 276.2441, 7924, kappa = 0.5), 2500)
vgm.mat$tipo <- "Mat"
v.grap <- bind_rows(v.grap, vgm.mat)

ggplot() + 
  geom_line(data = filter(v.grap, tipo != "Observado"), aes(dist, gamma, color = tipo), size = .75) +
  geom_point(data = filter(v.grap, tipo == "Observado"), aes(dist, gamma, color = tipo)) +
  ylab("semivarianza") + xlab("distancia") +
  ylim(0, 25) +   
  ggtitle("Gráfico 5: Semivariograma de H ~ DAP y modelos ajustados") + 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey"), 
        panel.grid = element_line(colour = "grey92"))

v.dir <- variogram(H ~ DAP, arboles.sp, alpha=(0:3) * 45)

v.dir %>% ggplot(aes(x = dist, y = gamma)) + geom_point() + facet_grid(. ~ dir.hor) + 
  geom_line(data = filter(v.grap, tipo == "Sph"), aes(x = dist, y = gamma)) +
  ylim(0, 25) +
  ylab("Semivariograma") +
  ggtitle("Gráfico 6: Semivariograma de H ~ DAP") + 
  theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "grey"), 
        panel.grid = element_line(colour = "grey92"))

#Grid==============================================

long <- seq(478053, 483163.2, length.out = 100)
lat <- seq(1101949, 1106327, length.out = 100)
grd <- expand.grid(long = long, lat = lat)
grd.sf <-st_as_sf(grd, coords = c("long", "lat"), 
                  crs = crs(mapa.sf)) %>% 
  st_intersection(mapa.sf)

grd.sf$DAP <- sample(x = arboles.sf$DAP, size = nrow(grd.sf), T)
  
mapa.grid <-ggplot(grd.sf) + geom_sf()
mapa <- ggplot(mapa.sf) + geom_sf()

ggarrange(mapa.grid, mapa, ncol = 2)

arboles.sp.sin.dupl <- remove.duplicates(arboles.sp)
arboles.sf.sin.dupl <- st_as_sf(arboles.sp.sin.dupl)
  
#Interpolación==================================================

#univ.krig.grid <- krige(H ~ DAP, arboles.sf.sin.dupl, grd.sf, model = vgm.fit)

idw.out <- idw(H ~ DAP, arboles.sf.sin.dupl, arboles.sf, idp = 2.5)
univ.krig <- krige(H ~ DAP, arboles.sf.sin.dupl, arboles.sf, model = vgm.fit)

#Validación cruzada
idw.cv <- krige.cv(H ~ DAP, arboles.sf.sin.dupl, nmax = 20)
uk.cv <- krige.cv(H ~ DAP, arboles.sf.sin.dupl, vgm.fit, nfold = 5, verbose = FALSE)

R2.idw <- 1 - sum(idw.cv$residual ^ 2) / sum((idw.cv$observed - mean(idw.cv$observed)) ^ 2)
R2.uk <- 1 - sum(uk.cv$residual ^ 2) / sum((uk.cv$observed - mean(uk.cv$observed)) ^ 2)

cv.df <- data.frame(st_coordinates(idw.cv), 
                    Residuo.idw = idw.cv$residual, 
                    Residuo.uk = uk.cv$residual)

cv.df %>% gather(Modelo, Residuo, -X, -Y) %>% 
  st_as_sf(crs = crs(mapa.sf, coord)) %>% 
  ggplot(aes(x = X, y = Y, size = Residuo)) + 
  geom_point(alpha = 1/ 5) + 
  scale_size(range = c(1, 5), name = "Residuo") + 
  facet_grid(. ~ Modelo)
