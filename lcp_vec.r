#####
tmp1.euc <- SpatialLines(
    list(
        Lines(
            Line(
                coords = (rbind(sites@coords[1,],
                                sites@coords[2,]
                                )
                )
            ),
            ID="1")
    ),
    proj4string = srtm@crs
)

########
LCP und SpatialLines

x <- sites@coords[,1][1:10] # at the moment only for 10 objects
y <- sites@coords[,2][1:10] # at the moment only for 10 objects

x.comb <- combn(x,2)
y.comb <- combn(y,2)

x.name <- data.frame(sites@data$Nr.[match(x.comb,sites@coords[,1])])


for(i in 1:(length(x.comb)/2)){
    i1 <- i*2-1 # odd numbers
    i2 <- i*2 # even numbers
    s <- c(x.comb[i1],y.comb[i1])
    z <- c(x.comb[i2],y.comb[i2])
    sz <- shortestPath(conduct, s, z, output="SpatialLines") # calculate the shortest path
    zs <- shortestPath(conduct, z, s, output="SpatialLines") # calculate the shortest path
    sz@lines[[1]]@ID <- as.character(paste(x.name[i1,1],x.name[i2,1]))
    zs@lines[[1]]@ID <- as.character(paste(x.name[i2,1],x.name[i1,1]))
    
    if(i==1){sdf <-rbind(sz,zs)}
    if(i>1){sdf <- rbind(sdf,sz,zs,
                         makeUniqueIDs = TRUE)
    }

    if(i==1){df <- cbind(ID = c(1,2),
                         FROM = c(paste(x.name[i1,1]),paste(x.name[i2,1])),
                         TO = c(paste(x.name[i2,1]),paste(x.name[i1,1]))
                         )
    }
    if(i>1){df <- cbind(ID = c(df[,1],i1,i2),
                        FROM = c(df[,2],paste(x.name[i1,1]),paste(x.name[i2,1])),
                        TO = c(df[,3],paste(x.name[i2,1]),paste(x.name[i1,1]))
                        )
    }
}


lcp_df <- as.data.frame(df)
# colnames(lcp_df) <- c("ID","START","TARGET")

lcp_sldf <- SpatialLinesDataFrame(sdf,
                                  lcp_df,
                                  match.ID = FALSE
                                  )

lcp_sldf@data$DIST <- SpatialLinesLengths(lcp_sldf)

writeOGR(lcp_sldf,
         "./results/",
         "Least_cost_paths",
         driver = "ESRI Shapefile",
         overwrite_layer = TRUE
         )
