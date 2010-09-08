drawRecTrans <-
function(recList) {
	grid.rect(x=unit(recList$x0,"npc"), y=unit(recList$y0,"npc"), width=unit(recList$w,"npc"), 
		height=unit(recList$h,"npc"), just=c("left","bottom"), gp = gpar(lwd=2))
}

