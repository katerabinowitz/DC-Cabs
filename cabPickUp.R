require(dplyr)
require(tidyr)
require(bit64)
require(rgdal)
require(lubridate)
require(ggplot2)
require(gridExtra)

### read in, rename, and subset ###
### read in, rename, and subset ###
### read in, rename, and subset ###
cab16 <- fread("TDMS Trips 2016-0101 to 20160831.csv")

colnames(cab16) <- c("fareDate", "pickupDT", "dropoffDT", "pickupGPS", "dropoffGPS", "tripTime", "tripMile", "meterFare",
                     "tip","surcharge", "extras", "tolls", "totalAmt", "payType", "payProvider", "tlms", "tripNo", "pvin",
                     "provider", "wav", "pickupAdd", "pickupCity", "pickupSt", "pickupZip", "dropoffAdd", "dropoffCity",
                     "dropoffSt", "dropoffZip")

cabDTL <- cab16 %>% select(pickupDT, dropoffDT, pickupGPS, dropoffGPS, tlms, tripNo, pickupAdd ,pickupCity, pickupSt,
                           pickupZip, dropoffAdd, dropoffCity, dropoffSt, dropoffZip)
rm(cab16)
### clean up location data ###
### clean up location data ###
### clean up location data ###

#per DVFH, lat + lon should be considered more reliable
cabDTL <- cabDTL %>% separate(pickupGPS, c("pickLat", "pickLon"), sep = "," , fill="right", merge="extra", remove=FALSE) %>%
  separate(dropoffGPS, c("dropLat", "dropLon"), sep = "," , fill="right", merge="extra", remove=FALSE) %>% 
  
  mutate(pickLat = as.numeric(pickLat), pickLon = as.numeric(pickLon),
         dropLat = as.numeric(dropLat), dropLon = as.numeric(dropLon),
         
         #subsetting to reasonable lat and longs for DC area
         llQual = ifelse(((pickLon < 40 & pickLon > 37) & (pickLat > -79 & pickLat < -75)) &
                           ((dropLon < 40 & dropLon > 37) & (dropLat > -79 & dropLat < -75)),"S",
                         ifelse((trunc(pickLat)!= 38 & trunc(pickLat)!= 39) | (trunc(dropLat)!= 38 & trunc(dropLat)!= 39) | 
                                  (trunc(pickLon)!= -77 & trunc(pickLon)!= -76 & trunc(pickLon)!= -78) | 
                                  (trunc(dropLon)!= -77 & trunc(dropLon)!= -76 & trunc(dropLon)!= -78),"B","G")))

table(cabDTL$llQual)

LLswap <- cabDTL %>% filter(llQual=="S")
LLgood <- cabDTL %>% filter(llQual=="G")
LLbad <- cabDTL %>% filter(llQual=="B")

#fix swapped lat lons
LLswap <- LLswap %>% mutate(pickLat2 = ifelse(pickLon < 40 & pickLon > 37, pickLon, pickLat),
                            pickLon = ifelse(pickLat > -79 & pickLat < -75, pickLat, pickLon)) %>% 
                    mutate(pickLat = pickLat2) %>%
                    select(LLswap, -pickLat2)

LLgood <- rbind(LLgood, LLswap)

### clean date time fields ###
### clean date time fields ###
### clean date time fields ###
rm(cabDTL,LLbad, LLswap)

LLT <- LLgood %>% mutate(pickupDT = gsub("\\..*", "", pickupDT),
                         dropoffDT = gsub("\\..*", "", dropoffDT)) %>%
  
  mutate(pickupDT=as.POSIXct(pickupDT, format="%Y-%m-%d %H:%M:%S", tz="America/New_York"),
         dropffDT=as.POSIXct(dropoffDT, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")) %>%
  
  mutate(length=difftime(LLT$dropoffDT,LLT$pickupDT,tz="EST"),
         pHR=hour(pickupDT),
         dHR=hour(dropoffDT),
         pDay=wday(pickupDT),
         dDay=wday(dropoffDT))

#filter out cab rides under 90 seconds
cabMap <- LLT %>% filter(length>90)

write.csv(LLT, file = "/Users/katerabinowitz/Documents/DataLensDC/DC-Transportation/DC Cab Pickups/taxiRides.csv",row.names=FALSE)

### Pick Up Map ###
### Pick Up Map ###
### Pick Up Map ###
minLat<- 38.8100
maxLat<- 38.9961
maxLon<- -76.9099
minLon<- -77.1187

pulC <- count(cabMap,pickLon,pickLat)

ggplot(pulC, aes(x=pickLon, y=pickLat, color=n)) +
  geom_point(size=0.002) +
  scale_x_continuous(limits=c(minLon,maxLon)) +
  scale_y_continuous(limits=c(minLat,maxLat)) + 
  scale_color_gradient(low="#bfbfbf",high="#cc00cc", trans="log") +
  theme(legend.position="none") +
  theme(panel.background=element_rect(fill="black", color="black")) +
  theme(plot.background=element_rect(fill="black", color="black")) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text.x=element_blank()) +
  theme(axis.text.y=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.title.x=element_blank()) +
  theme(plot.title=element_text(family="Helvetica", size=13, face="bold",color="#bfbfbf", margin = margin(t = 15,r = -10))) +
  theme(plot.caption=element_text(family="Helvetica", size=11, color="#bfbfbf",margin = margin(b = 15, l = -20))) +
  labs(title="DC Taxicab Pickups, January-August 2016",
       caption="Source: DataLensDC, DFHV")

ggsave("map.png", w=7, h=8, dpi=300)

### Neighborhood Charts  - Pickup ###
### Neighborhood Charts  - Pickup ###
### Neighborhood Charts  - Pickup ###
rm(LLT,pulC)

pull <-cabMap %>% select(pickLon,pickLat,pHR,pDay)

hood = readOGR("http://ec2-54-235-58-226.compute-1.amazonaws.com/storage/f/2013-05-12T03%3A50%3A18.251Z/dcneighorhoodboundarieswapo.geojson",
               "OGRGeoJSON")

addAll <- SpatialPoints(pull, proj4string=CRS(as.character("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")))
puHood <- over(addAll, hood)

puH <- cbind(pull,puHood)
puH<-readRDS("puH.rds")

puH <- puH %>% mutate(pDay = factor(pDay),
                      pHR=factor(pHR))

levels(puH$pDay)<-c(7,6,5,4,3,2,1)
puH$pDay<-factor(puH$pDay,label=c("Saturday","Friday","Thursday","Wednesday","Tuesday","Monday","Sunday"))

puHC <- count(puH,subhood)
puHCounts <- count(puH, subhood, pDay, pHR)

topHoods <- puHC %>% subset(n>100000) %>%
  arrange(desc(n))

toGraph <- subset(topHoods,subhood!="NA") %>%
  top_n(12)

puHGraph <- subset(puHCounts,puHCounts$subhood %in% toGraph$subhood) %>%
  rename(n = Pickups)

lapply(toGraph$subhood, function(cc) {
  gg <- ggplot(puHGraph %>% filter(subhood==cc),
        aes(x=pHR, y=pDay, fill=Pickups, frame=subhood)) +
        geom_tile(color="white", size=0.1) +
        scale_x_discrete(expand=c(0,0)) +
        scale_y_discrete(expand=c(0,0)) +
        coord_equal() +
        labs(x=NULL, y=NULL, title=sprintf("%s", cc)) +
        theme(axis.ticks=element_blank()) +
        theme(axis.text=element_text(size=6)) +
        theme(axis.text.x=element_text(angle=70)) +
        theme(panel.border=element_blank()) +
        theme(plot.title=element_text(hjust=0, size=9)) +
        theme(legend.title=element_text(size=7)) +
        theme(panel.spacing = unit(0.1,"cm")) +
        theme(legend.title.align=1) +
        theme(legend.text=element_text(size=7)) +
        theme(legend.position="bottom") +
        theme(legend.key.size=unit(0.2, "cm")) +
        theme(legend.key.width=unit(0.7, "cm")) +
        theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
        scale_fill_gradient(low="#bfbfbf", high="#cc00cc") 
}) -> cclist

cclist[["ncol"]] <- 4

do.call(grid.arrange, cclist)