formatTitle <-
function(x) {
    	s <- strsplit(x, " ")[[1]]
    	string <- paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")

    	if (isnumeric(substring(string,nchar(string)-1,nchar(string)))&&
		!isnumeric(substring(string,nchar(string)-2,nchar(string)))) {
		string <- paste(substring(string,1,nchar(string)-2)," '",substring(string,nchar(string)-1,nchar(string)),sep="")
	} else if (isnumeric(substring(string,nchar(string)-3,nchar(string)))&&
		!isnumeric(substring(string,nchar(string)-4,nchar(string)))) {
		string <- paste(substring(string,1,nchar(string)-4),substring(string,nchar(string)-3,nchar(string)),sep=" ")
	}
	string	
}

