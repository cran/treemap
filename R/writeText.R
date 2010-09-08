writeText <-
function(recList, lowerbound, upperbound, background, forcePrint) {
	textL <- as.character(recList$ind)
	wrapped <- mapply(FUN="layoutText",textL,recList$w,recList$h,upperbound)

	textW <- as.character(wrapped[2,])
	textS <- as.numeric(wrapped[1,])

	# select text that doesn't fit
	outOfBox <- which(textS<lowerbound)
	farOutOfBox <- which(textS<lowerbound*0.7)
	textS[outOfBox] <- lowerbound

	# calculate textbox width and height, x, and y
	wT <- as.numeric(convertWidth(stringWidth(textW), "npc", valueOnly=TRUE))*(textS+0.05)
	hT <- as.numeric(convertHeight(stringHeight(textW), "npc", valueOnly=TRUE))*1.1*(textS+0.05)
	xT <- recList$x0 + .5 * recList$w
	yT <- (recList$y0 + .5 * recList$h)

	# define transparency
	if (background) {
		transp <- c(rep(255,nrow(recList)))
	} else {
		transp <- c(rep(0,nrow(recList)))
		transp[outOfBox] <- 128
	}
	if (!forcePrint) {
		transp[farOutOfBox] <- 0
		textW[farOutOfBox] <- ""
	}
	
	# draw text background
	rgbTextCol <- col2rgb(as.character(recList$color))
	textCol <- rgb(rgbTextCol["red",],rgbTextCol["green",],rgbTextCol["blue",],alpha=transp,max=255)
	grid.rect(x=unit(xT-wT/(4/2),"npc"), y=unit(yT-hT/(4/2),"npc"), width=unit(wT*(4/4),"npc"), 
		height=unit(hT*(4/4),"npc"), just=c("left","bottom"), gp = gpar(fill = as.character(textCol),lty="blank"))

	# determine text color
	maxCol <- mapply(as.integer(rgbTextCol[1,]),as.integer(rgbTextCol[2,]),as.integer(rgbTextCol[3,]),FUN="max")
	minCol <- mapply(as.integer(rgbTextCol[1,]),as.integer(rgbTextCol[2,]),as.integer(rgbTextCol[3,]),FUN="min")
	lightness <- floor(.5*(maxCol+minCol))

	# write text
	tCol <- c(rep("black",nrow(recList)))
	tCol[lightness<128] <- "white"
	grid.text(textW,x=xT, y=yT, gp=gpar(col=tCol,cex=textS))
}

