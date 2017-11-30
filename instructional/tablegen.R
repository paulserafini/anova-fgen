function (table, length) {
  cat('<table border="1" bordercolor="808080"')
  	cat('<tr><td align="center"><font color=white>E(MS)</font></td><td align="center" colspan="2"><b>Constant</b></td><td align="center" colspan="',length-3,'"><b>Provisional</b></td></tr>')
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
