library(rvest)
library(maps)
library(ggplot2)
library(grid)

### set your own storage location
setwd("/mnt/Storage_2TB/Documents/Rscripts/vtrecruit")

### read data from website
u <- "http://virginiatech.sportswar.com/player/football/-2020/"
u19 <- "http://virginiatech.sportswar.com/player/football/-2019/"
u18 <- "http://virginiatech.sportswar.com/player/football/-2018/"

recruit <- read_html(u)

recruittable.lst <- html_table(html_nodes(recruit, "table"))
recruits <- recruittable.lst[[1]]
recruits <- recruits[-1,]


recruitstars.nodes <- html_nodes(recruit, "td.sportsWarStars")
recruitstars <- sapply(recruitstars.nodes, function(i) length(html_children(i)))

recruitlink <- html_nodes(recruit, "td a") %>% html_attr("href")

### modify data
recruits$Stars <- recruitstars

recruits$commit <- ifelse(recruits$Status %in% recruits$Status[grep("Commit", recruits$Status)], 1, 0)
recruits$signee <- ifelse(recruits$Status %in% recruits$Status[grep("Signee", recruits$Status)], 1, 0)
recruits$vt_commit <- ifelse(recruits$Status=="VT Commit", 1, 0)
recruits$vt_signee <- ifelse(recruits$Status=="VT Signee", 1, 0)
recruits$vt_signee4or5 <- ifelse(recruits$Status=="VT Signee" & recruits$Stars>=4, 1, 0)
recruits$vt_signed <- ifelse(recruits$vt_signee==1,"Signee","No")

recruits$pos_prim <- sapply(recruits$Pos, function(i) strsplit(i, "[,]")[[1]][1])
recruits$pos_prim_f <- as.factor(recruits$pos_prim)

recruits$ht_dec <- sapply(recruits$Ht, function(i) as.numeric(strsplit(i, "[-]")[[1]][1]) + as.numeric(strsplit(i, "[-]")[[1]][2])/12)


#### plot of recruit count by state ####

plotTargets <- function(){
  
  statecount <- table(recruits$State)
  
  statematch <- state.fips$abb
  countindex <- statecount[statematch]
  countindex[is.na(countindex)] <- 0
  cut.count <- cut(countindex, breaks=c(0,.5,2,5,9,99), include.lowest = TRUE)
  cut.names <- c("0","1-2","3-5","6-9","10+")
  levels(cut.count) <- cut.names
  stateindex <- as.numeric(cut.count)
  
  colors <- colorRampPalette(c("white", "brown4"))(5)
  colorindex <- as.numeric(cut.count)
  colors[1] <- "white"
  
  map('state'
           # , region=unique(state.fips$polyname)[state.fips$abb %in% names(statecount)]
           , fill=TRUE
           , col=colors[colorindex])
  title(main="Number of VT Targets, 2020")
  legend("bottomleft", cut.names, horiz=TRUE, fill=colors, cex=0.7)
}

#plotTargets()

#### Signee ratio #####

plotSigneeRatio <- function(){
  
  recruits$vt_signee <- ifelse(recruits$Status=="VT Signee", 1, 0)
  
  signeeratio <- tapply(recruits$vt_signee, recruits$State, mean)
  
  statematch <- state.fips$abb
  ratioindex <- signeeratio[statematch]
  ratioindex[is.na(ratioindex)] <- 0
  cut.ratio <- cut(ratioindex, breaks=c(0,.01,.05,.1,.2,1), include.lowest = TRUE)
  cut.names <- c("0-1%","1-5%","5-10%","10-20%","20%+")
  levels(cut.ratio) <- cut.names
  stateindex <- as.numeric(cut.ratio)
  
  colors <- colorRampPalette(c("white", "brown4"))(5)
  colorindex <- as.numeric(cut.ratio)
  colors[1] <- "white"
  
  
  map('state'
           # , region=unique(state.fips$polyname)[state.fips$abb %in% names(statecount)]
           , fill=TRUE
           , col=colors[colorindex])
  title(main="Ratio of VT Signees to Targets, 2020")
  legend("bottomleft", cut.names, horiz=TRUE, fill=colors, cex=0.7)
}


#### signee 4/5 star ratio by state ####


plot45star  <- function(){
  
  star45ratio <- tapply(recruits$vt_signee4or5, recruits$State, mean)
  
  statematch <- state.fips$abb
  ratioindex <- star45ratio[statematch]
  ratioindex[is.na(ratioindex)] <- 0
  cut.ratio <- cut(ratioindex, breaks=c(0,.01,.02,.05,.1,1), include.lowest = TRUE)
  cut.names <- c("0-1%","1-2%","2-5%","5-10%","10%+")
  levels(cut.ratio) <- cut.names
  stateindex <- as.numeric(cut.ratio)
  
  colors <- colorRampPalette(c("white", "brown4"))(5)
  colorindex <- as.numeric(cut.ratio)
  colors[1] <- "white"
  
  map('state'
           # , region=unique(state.fips$polyname)[state.fips$abb %in% names(statecount)]
           , fill=TRUE
           , col=colors[colorindex])
  title(main="Ratio of VT 4/5 Star Signees to Targets, 2020")
  legend("bottomleft", cut.names, horiz=TRUE, fill=colors, cex=0.7)
}


#### VT signees by POS ####


plotPosition <- function(){
  
  posdata <- table(recruits$pos_prim, recruits$vt_signed)
  posdata <- posdata[order(-posdata[,1]),]
  
  posratio <- round(posdata[,"Signee"]/rowSums(posdata),3)*100
  posratio <- paste0(posratio,"%")
  
  b <- barplot(t(posdata), col=c("lightgrey","maroon"), legend=TRUE)
  text(x=b, y=1, labels = posratio, cex=0.7, col="maroon")
  title("VT Pos. Targets with Pct. Signee, 2020")
  
}
#plotPosition()


#### Targets ht and wt ####
tempdata <- recruits[,c("pos_prim","ht_dec","Wt","vt_signed")]
tempdata$wt_num <- as.numeric(tempdata$Wt)
medianht <- tapply(tempdata$ht_dec, tempdata$pos_prim, median)
medianht <- sort(medianht, decreasing = TRUE)
medianwt <- tapply(tempdata$wt_num, tempdata$pos_prim, median)
medianwt <- sort(medianwt, decreasing = TRUE)

tempdata$pos_prim_f <- factor(tempdata$pos_prim, levels = names(medianht))

plotHeight <- function(){
  
  tempdata <- recruits[,c("pos_prim","ht_dec","Wt","vt_signed")]
  tempdata$wt_num <- as.numeric(tempdata$Wt)
  medianht <- tapply(tempdata$ht_dec, tempdata$pos_prim, median)
  medianht <- sort(medianht, decreasing = TRUE)
  medianwt <- tapply(tempdata$wt_num, tempdata$pos_prim, median)
  medianwt <- sort(medianwt, decreasing = TRUE)
  
  tempdata$pos_prim_f <- factor(tempdata$pos_prim, levels = names(medianht))
  
  ggplot(tempdata, aes(x=pos_prim_f, y=ht_dec, fill=vt_signed)) + 
    geom_boxplot() +
    ggtitle("VT Targets Height, 2020") +
    xlab("Position") +
    ylab("Height")
  
}



plotWeight <- function(){
  
  tempdata <- recruits[,c("pos_prim","ht_dec","Wt","vt_signed")]
  tempdata$wt_num <- as.numeric(tempdata$Wt)
  medianht <- tapply(tempdata$ht_dec, tempdata$pos_prim, median)
  medianht <- sort(medianht, decreasing = TRUE)
  medianwt <- tapply(tempdata$wt_num, tempdata$pos_prim, median)
  medianwt <- sort(medianwt, decreasing = TRUE)
  
  tempdata$pos_prim_f <- factor(tempdata$pos_prim, levels = names(medianwt))
  
  ggplot(tempdata, aes(x=pos_prim_f, y=wt_num, fill=vt_signed)) + 
    geom_boxplot() +
    ggtitle("VT Targets Weight, 2020") +
    xlab("Position") +
    ylab("Weight")
  
}


# Generate Infographic in PNG Format

png("vtrecruit_infographics.png", width = 600, height = 1600)
par(mfrow=c(4,1))
plotTargets()
plotSigneeRatio()
plot45star()
plotPosition()
#plotHeight()
#plotWeight()
dev.off()
