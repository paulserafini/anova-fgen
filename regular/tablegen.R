function (table, length) {
  cat('<table border="1" bordercolor="808080"')
    for (i in 1:nrow(table)) {
			cat('<tr>')
			for (j in 1:length(table[i,])) {
                            if (j == 1) {
								cat('<td align="center"><b>', unlist(table[i,j]), '</b></td>', sep="")
                            } else {
								cat('<td align="center">', unlist(table[i,j]), '</td>', sep="")
                            }
			}
			cat('</tr>')
		}
	cat('</table>')
}
