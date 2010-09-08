sizeText <-
function(text, width, height, upperbound) {
	tw <- convertWidth(stringWidth(text), "npc", valueOnly=TRUE)*1.1
	th <- convertHeight(stringHeight(text), "npc", valueOnly=TRUE)*1.1

	cex <- min(width/tw-0.05,height/th-0.05, upperbound)

	return (list(size=cex,text=text))
}

