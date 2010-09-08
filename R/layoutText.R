layoutText <-
function(text, width, height, upperbound){
	textVector <- list(text,
	strwrap(text, width = floor(nchar(text)/1.5), indent = 0, exdent = 0, prefix = "", simplify = TRUE),
	strwrap(text, width = floor(nchar(text)/2.5), indent = 0, exdent = 0, prefix = "", simplify = TRUE),
	strwrap(text, width = floor(nchar(text)/3.5), indent = 0, exdent = 0, prefix = "", simplify = TRUE))


	textCol <- mapply(FUN=paste,textVector ,collapse="\n")
		
	result <- mapply(FUN=sizeText, textCol, width, height, upperbound)
	sizes <- as.numeric(result[1,])
	biggest <- which(sizes==max(sizes))[1]
	return (result[,biggest])
}

