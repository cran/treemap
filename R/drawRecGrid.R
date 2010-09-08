drawRecGrid <-
function(recList, dat, lwd) {

	transp <- c(rep(255,nrow(recList)))
	rgb <- col2rgb(as.character(recList$color))
	col <- rgb(rgb["red",],rgb["green",],rgb["blue",],alpha=transp,max=255)

	grid.rect(x=unit(recList$x0,"npc"), y=unit(recList$y0,"npc"), width=unit(recList$w,"npc"), 
		height=unit(recList$h,"npc"), just=c("left","bottom"), gp = gpar(lwd=lwd, lex=0.1,fill = as.character(col)))
}

