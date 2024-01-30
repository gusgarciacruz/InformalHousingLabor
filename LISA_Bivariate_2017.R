###Authors: García-Badillo-Aristizábal (2024)
###Title: Labor informality and housing informality in space: in search of the missing links


library(stringr)
library(spdep)
library(rgdal)
library(magrittr)
library(ggplot2)
library(sf)
library(tmap)
library(tidyverse)
library(summarytools)

#======================================================
setwd("")
# 2017
# Load data
#map <- readOGR(dsn="", layer="Medellin_Map")
map <- st_read("Medellin_Map.shp")
#head(map@data)
names(map)

x <- map$housing
y <- map$labor

#======================================================
# Programming some functions

# Bivariate Moran's I
moran_I <- function(x, y = NULL, W){
  if(is.null(y)) y = x
  
  xp <- scale(x)[, 1]
  yp <- scale(y)[, 1]
  W[which(is.na(W))] <- 0
  n <- nrow(W)
  
  global <- (xp%*%W%*%yp)/(n - 1)
  local  <- (xp*W%*%yp)
  
  list(global = global, local  = as.numeric(local))
}


# Permutations for the Bivariate Moran's I
set.seed(123456789)
simula_moran <- function(x, y = NULL, W, nsims = 999){
  
  if(is.null(y)) y = x
  
  n   = nrow(W)
  IDs = 1:n
  
  xp <- scale(x)[, 1]
  W[which(is.na(W))] <- 0
  
  global_sims = NULL
  local_sims  = matrix(NA, nrow = n, ncol=nsims)
  
  ID_sample = sample(IDs, size = n*nsims, replace = T)
  
  y_s = y[ID_sample]
  y_s = matrix(y_s, nrow = n, ncol = nsims)
  y_s <- (y_s - apply(y_s, 1, mean))/apply(y_s, 1, sd)
  
  global_sims  <- as.numeric( (xp%*%W%*%y_s)/(n - 1) )
  local_sims  <- (xp*W%*%y_s)
  
  list(global_sims = global_sims,
       local_sims  = local_sims)
}



#======================================================
# Adjacency Matrix (Queen)

nb <- poly2nb(map)
lw <- nb2listw(nb, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W/rowSums(as.matrix(W)))
W[which(is.na(W))] <- 0


#======================================================
# Calculating the index and its simulated distribution
# for global and local values

m <- moran_I(x, y, W)

# Global Moral
global_moran <- m[[1]][1]
global_moran
#> 0.3796602

# Local values
m_i <- m[[2]] 

# local simulations
local_sims <- simula_moran(x, y, W)$local_sims

# global pseudo p-value  
# get all simulated global moran
global_sims <- simula_moran(x, y, W)$global_sims

# Proportion of simulated global values taht are higher (in absolute terms) than the actual index 
moran_pvalue <- sum(abs(global_sims) > abs( global_moran )) / length(global_sims)
#> 0


# Identifying the significant values 
alpha <- .05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t( apply(local_sims, 1, function(x) quantile(x, probs=probs)))
sig       <- ( m_i < intervals[,1] )  | ( m_i > intervals[,2] )

intervals_global <- quantile(global_sims, probs=probs)
intervals_global
sig_global      <- ( m[[1]] < intervals_global[1] )  | ( m[[1]] > intervals_global[2] )
sig_global

#======================================================
# Preparing for plotting


# Convert shape file into sf object
map_sf     <- st_as_sf(map)
map_sf$sig <- sig


# Identifying the LISA clusters
xp <- scale(x)[,1]
yp <- scale(y)[,1]


patterns <- as.character( interaction(xp > 0, W%*%yp > 0) )
patterns <- patterns %>% 
  str_replace_all("TRUE","High") %>% 
  str_replace_all("FALSE","Low")

patterns[map_sf$sig==0] <- "Not significant"
map_sf$patterns <- patterns


# Rename LISA clusters
map_sf$patterns2 <- factor(map_sf$patterns, 
                           levels=c("High.High", 
                                    "High.Low", 
                                    "Low.High", 
                                    "Low.Low", 
                                    "Not significant"),
                           labels=c("High housing informality - High labor informality", 
                                    "High housing informality - Low labor informality", 
                                    "Low housing informality - High labor informality",
                                    "Low housing informality - Low labor informality", 
                                    "Not significant"))
freq(map_sf$patterns2, totals=FALSE, cumul=F)
### PLOT

bivariate2017 <- ggplot() +
  geom_sf(data=map_sf, aes(fill=patterns2), color="NA") +
  scale_fill_manual(values = c("red", "pink", "light blue", "dark blue", "grey90")) +
  guides(fill = guide_legend(title="")) +
  theme(plot.title = element_text(hjust = 0.5, size=9, margin=margin(0,0,-2,0)),
        panel.background = element_rect(fill = "white"),
        legend.position =c(1.08, .15),
            #    legend.position = c(0.22, 0.13),
            #   legend.key.size = unit(0.2, "cm"),
            #    legend.key.width = unit(0.2,"cm"),
               legend.text=element_text(size=14),
            #    legend.background = element_rect(fill=NA),
            #    legend.spacing.x = unit(0.05, 'cm'),
        axis.text = element_blank(),
        axis.ticks = element_blank())
bivariate2017


ggsave(plot=bivariate2017, "bivariate2017.png", width = 7, height = 9, units = "cm", dpi = 3000)
