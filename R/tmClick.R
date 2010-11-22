tmClick <-
function() {
	# get mouse click (in relative coordinates (from 0 to 1))
	click_window <- grid.locator("npc")
	x <- as.numeric(click_window$x)
	y <- as.numeric(click_window$y)
	return(list(x=x, y=y))
}

