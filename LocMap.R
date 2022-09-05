# preliminaries
library(sp)
library(sf)
library(maptiles)
LongLat <- CRS("+proj=longlat +ellps=WGS84 
           +datum=WGS84 +no_defs") # uses Earth ellipsis specs from WGS84 datum
UTM50 <- CRS("+proj=utm +zone=50 +south") # just for Zone 50, S hemisphere!
pal4lite <- c("black", "purple2", "blue2", "cyan4", "forestgreen", 
              "darkorange", "gold3", "red3", "gray80", "white")
pal4liteTransp <- c("#000000","#912CEEb0","#0000EEb0","#008B8Bb0","#228B22b0",
                    "#CDAD00b0", "#FF8C00b0", "#CD0000b0", "#CCCCCC", "#FFFFFF")
pal4dark <- c("white", "pink1", "orange", "khaki1", "lightgreen", "cadetblue1", 
              "deepskyblue", "plum", "gray80", "black")
windowsFonts(nar=windowsFont("Arial Narrow"), monosans=windowsFont("Consolas"))


## make map extent object ####
afr_utm <- SpatialPoints(coords = data.frame(x = c(115.6,117.0),
                         y = c(-32.7,-31.8)), proj4string = LongLat)
extent <- st_as_sf(afr_utm)

# or "OpenStreetMap"
MPtiles <- get_tiles(extent, provider = "CartoDB.VoyagerNoLabels", 
                                crop = TRUE, zoom = 10)

palette(pal4lite)
par(oma=c(3,3,1,1), lend="square")
plot(MPtiles)
abline(h=c(-33,-32), lty=2)
abline(v=c(115,116), lty=2)
axis(1);axis(2)
