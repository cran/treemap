vColorMplySplit <-
function(vColor) {
	vColorMply <- unlist(strsplit(vColor, split="*", fixed=TRUE))
	if (length(vColorMply)==1) {
		vColorMply <- c(1,vColorMply)
	}
	return (vColorMply)
}

