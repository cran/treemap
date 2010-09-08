tmPlot <-
function(df, 
	index, 
	subindex=NA,
	vSize, 
	vColor="", 
	sortID="",
	type="auto",
	titles=NA,
	subtitles=NA) {

	#############
	## Process variable names and titles
	#############
	
	## First checks
	if (!exists("df")) stop("Dataframe <df> not defined")
	if (!exists("index")) stop("Attribute <index> not defined")
	if (!exists("vSize")) stop("Attribute <vSize> not defined")
	if (class(df)!="data.frame") stop("Object <df> is not a data.frame")
	if (!index %in% names(df)) stop("<index> is not a column name of <df>")


	## Get size variable(s)
	vSizeVector <- unlist(strsplit(vSize, split="+", fixed=TRUE))
	n <- length(vSizeVector)

	## Checks if all vSizes are valid
	for (i in 1:n) {
		if (!vSizeVector[i] %in% names(df)) stop(paste(vSizeVector[i]," is not a column in <df>", sep=""))
		if (class(df[,vSizeVector[i]])!="numeric" && class(df[,vSizeVector[i]])!="integer") stop(paste("Column ", vSizeVector[i], " is not numeric or integer",sep=""))
		if (any(is.na(df[,vSizeVector[i]]))) stop(paste("Column ", vSizeVector[i], " contains missing values.",sep=""))
		if (min(df[,vSizeVector[i]])<0) stop(paste("Column ", vSizeVector[i], " contains negative values.",sep=""))
	}

	## Checks if titles and subtitles have length n
	if (!is.na(titles) && length(titles) != n) {warning(paste("Number of titles should be ", n, ". Titles will be ignored.", sep="")); titles <- NA}
	if (!is.na(subtitles) && length(subtitles) != n) {warning(paste("Number of subtitles should be ", n, ". Subtitles will be ignored.", sep="")); titles <- NA}
		
	## Determine titles
	if (is.na(titles)) {	
		options(warn=-1) 
		vSizeNames <- mapply(FUN="formatTitle", vSizeVector)
		options(warn=0) 
	} else {
		vSizeNames <- as.character(titles)
	}

	## Process formula for color variables
	vColorAdd <- unlist(strsplit(vColor, split="+", fixed=TRUE))
	vColorDiv <- unlist(mapply(FUN="vColorDivSplit", vColorAdd))
			
	if (vColor=="") {
		vColorVector <- matrix(data=NA,nrow=2,ncol=n)
		vColorVectorBy <- matrix(data=NA,nrow=2,ncol=n)
	} else if (is.vector(vColorDiv)) {
		vColorVector <- unlist(mapply(FUN="vColorMplySplit", vColorDiv))
		vColorVectorBy <- matrix(data=NA,nrow=2,ncol=n)
	} else {
		vColorVector <- unlist(mapply(FUN="vColorMplySplit", vColorDiv[1,]))
		if (is.matrix(vColorVector)) {
			vColorVectorBy <- unlist(mapply(FUN="vColorMplySplit", vColorDiv[2,]))
		}
	}

	## Determine subtitles
	if (is.na(subtitles)) {	
		options(warn=-1) 
		if (!all(is.na(vColorVector))) {
			vColorNames <- mapply(FUN="formatColorTitle", vColorVector[1,],vColorVector[2,],vColorVectorBy[1,],vColorVectorBy[2,])
		} else vColorNames <- rep("",n)
		options(warn=0) 
	} else {
		vColorNames <- as.character(subtitles)
	}
		

	
	##########
	## Determine grid
	##########
	
	width <- par("din")[1]
	height <- par("din")[2]
	
	mx <- n
	numbers <- matrix(rep(1:mx, mx) * rep(1:mx, each=mx), nrow=mx,ncol=mx) 
	optnum <- rep(0,mx)
	
	for (i in 1:mx) {
		optnum[i] <- min(100,which(numbers[i,]>=n))
	}
	optnum <- unique(optnum)
	optnum <- optnum[optnum!=100]
	optn <- length(optnum)
	minAsp <- 0
	for (i in 1:optn){
		rW <- optnum[i]/width
		cH <- optnum[optn+1-i]/height
		aspR <- min(rW/cH, cH/rW)
		if (aspR > minAsp) {
			minAsp <- aspR
			minAspI <- i
		}
	}

	nCol <- optnum[minAspI]
	nRow <- optnum[optn+1-minAspI]
	

	############
	## Determine sorting order
	############
	ascending <- TRUE
	if (substr(sortID,1,1)=="-") {
		ascending <- FALSE
		sortID <- substr(sortID,2,nchar(sortID))
	}

	############
	## Determine treemap type
	############
	
	legenda <- TRUE
	if (type=="auto") {
		if (all(is.na(vColorVector))) {
			type <- "linked"
			legenda <- FALSE
		} else if (!all(is.na(vColorVectorBy))) {
			type <- "dens"
		} else {
			perc <- ((df[vSizeVector] - df[vColorVector[2,]] )/df[vColorVector[2,]])*100
			isNaN <- apply(perc, 2, FUN=is.nan)
			perc[isNaN] <- 0
			
			if (min(perc)<=-60|| max(perc)>=150) {
				if (min(df[vSizeVector])>=0 && max(df[vSizeVector])<=100) {
					type <- "perc"	
				} else type <- "dens"
			} else type <- "comp"
		}
	} else if (type=="linked") {
		legenda <- FALSE
	}
	
	
	###########
	## Agggregate data
	###########
	
	if (is.na(subindex)) {
		varNames <- as.character(na.omit(unique(c(vSizeVector, vColorVector[2,]))))

		df <-aggregate(x=df[varNames], by=list(df[,index]), FUN="sum", na.rm = TRUE)
		names(df) <- c(index, names(df)[-1])
		tempSubindices <- NA 
	} else tempSubindices <- df[subindex]

	
	############
	## Plot treemap(s)
	############

	
	pushViewport(viewport(layout=grid.layout(nRow, nCol)))


	iCol<-1
	iRow<-1
	tm<-list()
	for (i in 1:n) {
		datSize<-df[vSizeVector[i]]
		if (all(is.na(vColorVector))) { 
			datColor<-df[vSizeVector[1]]
		} else {
			datColor<-df[vColorVector[2,i]]/as.numeric(vColorVector[1,i])
			if (!is.na(vColorVectorBy[i])) {
				datColor<-datColor/(df[vColorVectorBy[2,i]]/as.numeric(vColorVectorBy[1,i]))
			}
		}
		pushViewport(viewport(layout.pos.col=iCol, layout.pos.row=iRow))
		
		if (sortID=="size") {
			sortDat <- datSize
		} else if (sortID=="color") {
			sortDat <- datColor
		} else if (sortID=="") {
			sortDat <- 1:nrow(datSize)
			ascending <- FALSE
		} else {
			sortDat <- df[sortID]
		}
		if (!ascending) {
			sortDat <- -sortDat
		}

		
		tm[[i]] <- baseTreemap(
			indices=df[index],
			values=datSize,
			values2=datColor,
			subindices=tempSubindices,
			type=type,
			sortDat=sortDat,
			legenda=legenda,
			sizeTitle=vSizeNames[i],
			colorTitle=vColorNames[i])
			
			
		popViewport()
		iRow<-iRow+1
		if (iRow>nRow) {
			iRow<-1
			iCol<-iCol+1
		}	
	}
	# for (i in 1:2) {
		# click.locn <- grid.locator("npc")
		# print(click.locn)
		# click.row <- nRow - as.numeric(click.locn$y) %/% (1/nRow)
		# click.col <- 1 + as.numeric(click.locn$x) %/% (1/nCol)
		# cat(click.row, " ", click.col, "\n")
		# pushViewport(viewport(layout.pos.col=click.col, layout.pos.row=click.row))
		# grid.rect()
		
		# index <- click.row + (click.col - 1) * nRow
		# cat("index", index)
		# vpDat <- tm[[index]][[1]]$vpDat
		# grid.rect()
		# upViewport(n=2)
		# popViewport()
	# }
	# current.vpTree()
}

