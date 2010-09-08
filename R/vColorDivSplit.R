vColorDivSplit <-
function(vColor) {
	vColorDiv <- unlist(strsplit(vColor, split="/", fixed=TRUE))
	return (vColorDiv)
}

