baseTreemap <-
function(indices, values, values2, 
	subindices=NA,
	type="fixed",
	width=convertWidth(unit(1,"npc"),"inches",valueOnly = TRUE),
	height=convertHeight(unit(1,"npc"),"inches",valueOnly = TRUE),
	neg=FALSE,
	sortDat=values,
	legenda=TRUE,
	upperboundText=0.8, 
	lowerboundText=0.6, 
	forcePrint=FALSE,
	sizeTitle="", 
	colorTitle="") {

	plotMargin <- unit(0.5,"cm")
	
	cexLarge <- min(14,(height*3.6), (width*3.6))
	cexSmall <- cexLarge * 0.8

	
	if (legenda) {
		legWidth <- min(unit(5, "inches"), convertWidth(unit(0.9, "npc")-2*plotMargin,"inches"))
		legHeight <- unit(cexSmall * 0.06, "inches")
		
		vpLeg <- viewport(name = "legenda",
		  x = plotMargin,
		  y = 0.5*plotMargin,
		  width = unit(1, "npc") - 2 * plotMargin,
		  height = legHeight,
		  gp=gpar(fontsize=cexSmall),
		  just = c("left", "bottom"))
	
		vpLeg2 <- viewport(name = "legenda2",
		  x = (unit(1, "npc") - legWidth)*0.5,
		  y = legHeight*0.3,
		  width = legWidth,
		  height = legHeight*0.7,
		  gp=gpar(fontsize=cexSmall),
		  just = c("left", "bottom"))
	} else legHeight <- unit(0,"inches")

	vpDat <- viewport(name = "dataregion", 
	  x = plotMargin,
	  y = legHeight + 0.5*plotMargin,
	  width = unit(1, "npc") - 2 * plotMargin,
	  height = unit(1,"npc") - legHeight - plotMargin,
	  gp=gpar(fontsize=cexLarge),
	  just = c("left", "bottom"))
	
	vpDat2 <- viewport(name = "dataregion2", 
	  x = 0,
	  y = 0,
	  width = unit(1, "npc"),
	  height = unit(1,"npc") - unit(1.5,"lines"),
	  gp=gpar(fontsize=cexSmall),
	  just = c("left", "bottom"))
	
	# CREATE DATA FRAME (LOW LEVEL)
	dat <- data.frame(index=indices,value=values,value2=values2, sort=sortDat, subindex=subindices)
	dat$color <- rep(NA,nrow(dat))
	names(dat) <- c("index", "value", "value2", "sort", "subindex", "color")

	dat <- dat[dat$value>0,]
	
	sortID <- sort(dat$sort,decreasing=FALSE,index.return=TRUE)$ix
	dat <- dat[sortID,]
	

	# CREATE DATA FRAME (AGGREGATED LEVEL)
	if (type=="dens") {
		dat$temp <- dat$value * dat$value2

		dat2 <- aggregate(x=data.frame(dat$value,dat$temp, dat$sort), by=list(dat$index), FUN="sum")
		dat2$dat.temp <- dat2$dat.temp / dat2$dat.value
		dat$temp <- NULL
	} else {
		dat2 <- aggregate(x=data.frame(dat$value,dat$value2, dat$sort), by=list(dat$index),FUN="sum")
	}
	names(dat2) <- c("index","value","value2", "sort")
	dat2$subindex <- rep(NA,nrow(dat2))
	dat2$color <- rep(NA,nrow(dat2))
	sortID <- sort(dat2$sort,decreasing=FALSE,index.return=TRUE)$ix
	dat2 <- dat2[sortID,]


	if (nrow(dat)==nrow(dat2)) {
		hierar <- FALSE; dat12 <- dat
	} else {hierar <- TRUE; dat12 <- rbind(dat,dat2)}
	
	if (legenda) {	
		pushViewport(vpLeg)
#		grid.rect()
		grid.text(colorTitle, y = unit(0.5, "lines"))
		pushViewport(vpLeg2)
#		grid.rect()
	}
	
	if (type == "comp") {
		dat12$color <- comp2col(dat12, upperboundText, TRUE, neg)
	} else if (type == "perc") {
		dat12$color <- fill2col(dat12, upperboundText, TRUE, neg)
	} else if (type == "dens") {
		dat12$color <- dens2col(dat12, upperboundText, TRUE, neg)
	} else if (type == "linked") {
		dat12$color <- fixed2col(dat12)
	}
	if (legenda) {	
		popViewport()
		popViewport()
	}

	dat <- dat12[1:nrow(dat),]
	if (hierar) dat2 <- dat12[(nrow(dat)+1):nrow(dat12),]

	pushViewport(vpDat)
#	grid.rect()
	grid.text(sizeTitle, y = unit(1, "npc") - unit(0.5, "lines"))
	pushViewport(vpDat2)
#	grid.rect()
	grid.rect(name="TMrect")
		
	dataRec <- list(X0=0,Y0=0,
		W=convertWidth(unit(1, "grobwidth", "TMrect"),"inches",valueOnly=TRUE),
		H=convertHeight(unit(1, "grobheight", "TMrect"),"inches",valueOnly=TRUE))
	
	if (hierar) {
		recList <- pivotSize(dat2,dataRec)
		
		recList$x0 <- recList$x0 / dataRec$W
		recList$y0 <- recList$y0 / dataRec$H
		recList$w <- recList$w / dataRec$W
		recList$h <- recList$h / dataRec$H
	
		recList2 <- data.frame(ind=integer(0), x0=numeric(0), y0=numeric(0), w=numeric(0), h=numeric(0), 
			color=character(0))
		for (i in 1:nrow(recList)) {
			smallRec <- recList[i,2:5]
			ind <- recList[i,]$ind
			dat3 <- dat[dat$index==ind,]
			sortID <- sort(dat3$sort,decreasing=FALSE,index.return=TRUE)$ix
			dat3 <- dat3[sortID,]
			recList2 <- rbind(recList2, pivotSize(dat3,smallRec))
		}
		
		if (sum(!is.na(dat$subindex))!=0) {
			drawRecGrid(recList2, dat, lwd = 1)
			drawRecTrans(recList)
			writeText(recList, upperbound = upperboundText, lowerbound = lowerboundText, background = TRUE, forcePrint = FALSE)
		} else {
			drawRecGrid(recList2, dat, lwd = 1)
			writeText(recList2, upperbound = upperboundText, lowerbound = lowerboundText, background = FALSE, forcePrint = FALSE)
		}
	} else {
		
		
		recList2 <- pivotSize(dat,dataRec)
		recList2$x0 <- recList2$x0 / dataRec$W
		recList2$y0 <- recList2$y0 / dataRec$H
		recList2$w <- recList2$w / dataRec$W
		recList2$h <- recList2$h / dataRec$H

		drawRecGrid(recList2, dat, lwd = 1)
		writeText(recList2, upperbound = upperboundText, lowerbound = lowerboundText, background = FALSE, forcePrint = FALSE)
	}
	popViewport()
	popViewport()
	return(list(list(vpDat=vpDat,vpDat2=vpDat2),recList2))
}

