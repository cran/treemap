formatColorTitle <-
function(sx,x,sdiv,div) {
	string <- formatTitle(x)
	if (sx!=1) {	
		string<-paste(sx,string,sep=" ")
	}
	if (!is.na(div)) {
		stringDiv <- formatTitle(div)
		if (sdiv!=1) {
			stringDiv<-paste(sdiv,stringDiv,sep=" ")
		}
		string<-paste(string,"per",stringDiv,sep=" ")
	}
	string
}

