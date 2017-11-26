function (table, header) {
  cat('<table border="1" bordercolor="808080"')
    for (i in 1:nrow(table)) {
      if (i == 1 & header == TRUE) {
  			cat('<tr>')
  			for (i in 1:length(table[1,])) {
  				cat('<td align="center">', table[1,i], '</td>', sep="")
			}
			cat('</tr>')

		} else {
			cat('<tr>')
			for (j in 1:length(table[i,])) {
                            if (j == 1) {
                                    if (header == TRUE) {
										cat('<td align="center">', table[i,j], '</td>', sep="")
                                    } else {
										cat('<td>', unlist(table[i,j]), '</td>', sep="")
                                    }
                            } else {
                                if (header == TRUE) {
									cat('<td align="center">', table[i,j], '</td>', sep="")
                                } else {
									cat('<td align="center">', unlist(table[i,j]), '</td>', sep="")
                                }
                            }
			}
			cat('</tr>')
		}
	}
	cat('</table>')
}
