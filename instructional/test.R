tablegen <- dget("tablegen.R")
function (factors, factor_type, step) {

  	# Generate vector of 2 way, 3 way, etc. combinations of input
	combogen <- function (input, num_factors) {
	  	output <- c()
		for (i in 1:num_factors) {
		  matrix <- combn(input, i)
		  vector <- lapply(1:ncol(matrix), function(i) matrix[,i])
		  output <- c(output, vector)
		}
		return(output)
	}

	num_factors <- length(factors)
	factor_df <- unlist(lapply(factors, function(v) paste0('(',tolower(v), '-1)')))

	# Make vectors of the effects, their type (0 = random, 1 = fixed), and their degrees of freedom
	effects <- combogen(factors, num_factors)
	effect_type <- combogen(factor_type, num_factors)
	effect_df <- combogen(factor_df, num_factors)

	effect_df <- lapply(effect_df, function(i) paste(i, collapse=""))
	effect_type <- lapply(effect_type, function(effect) prod(effect))

	# Create denominator for random effects
	num_effects <- length(effects)
	levels <- lapply(1:num_effects, function(i) paste(effects[[i]], collapse=""))
	levels <- unlist(lapply(levels, function(v) tolower(v)))

	# Create vector of denominators
	combined <- list(levels, effect_df)
	denominators <- unlist(lapply(1:num_effects, function(x) combined[[ effect_type[[x]]+1 ]] [[x]] ))

	# Create numerator
	numerator <- tolower(paste(factors, collapse=""))
	numerator <- paste0('n',numerator)
	numerators <- rep(numerator, num_effects)
	
	# Create weights
	weights <- c()
	for (i in 1:num_effects) { # step 3
	  if (step > 2) {
  		if (effect_type[[i]] == 0) { # remove common characters from numerator and denominator; if denominator is empty then weight = numerator
  			num_pretrim <- unlist(strsplit(numerators[[i]], ""))
  			den_pretrim <- unlist(strsplit(denominators[[i]], ""))
  			if (step > 3) { # step 4
    			num_trim <- paste(num_pretrim[!(num_pretrim %in% den_pretrim)], collapse="")
    			den_trim <- paste(den_pretrim[!(den_pretrim %in% num_pretrim)], collapse="")
  			} else {
  			  num_trim <- paste(num_pretrim, collapse="")
  			  den_trim <- paste(den_pretrim, collapse="")
  			}
  			if (den_trim == '') {
  			  weight <- num_trim
  			} else {
  			  weight <- paste0('[ <sup>', num_trim, '</sup>&frasl;<sub>', den_trim, '</sub> ]')
  			}
  		} else {
  			numerator <- numerators[[i]]
  			denominator <- denominators[[i]]
  			weight <- paste0('[ <sup>', numerator, '</sup>&frasl;<sub>', denominator, '</sub> ]')
  		}
	  } else {
	    weight <- '______'
	  }
		weights[[i]] <- weight
	}


	effect_strings <- lapply(effects, function(x) paste(x, collapse=""))

	# Assemble weights and variances
	weightplusvariance <- lapply(1:num_effects, function(x) {
		paste0(weights[[x]],'&sigma;<sub>',effect_strings[[x]],'</sub>')
	})

	provisional <- matrix("", nrow=1, ncol=num_effects)
	for (a in 1:num_effects) {

		matt <- cbind(weightplusvariance,tolower(effect_strings))
		matt <- matt[-a,]

		# Rule out effects w/ out the focal effects
		for (e in effects[[a]]) {
			char <- tolower(e)
			matt <- matt[ grepl(char, matt[,2]),, drop=FALSE ]
			matt[,2] <- gsub(char, "", as.character(matt[,2]))
		}
		#matt <- matt[ matt[,2] != "",, drop=FALSE ]

		# Remove fixed effects
		alphabet <- c('a','b','c','d')
		if (step > 1) { # step 2
			for (f in factors) {
				char <- tolower(f)
				num <- match(char, alphabet)
				effect <- effect_type[[num]]
				matt[,2] <- gsub(char, effect, as.character(matt[,2]))
			}
  			matt[ grepl(1, matt[,2]), ] <- paste0('<font color=white>',matt[ grepl(1, matt[,2]), ],'</font>')
		}

		diff <- num_effects - nrow(matt)
		vect <- c(matt[,1], rep("", diff))
		provisional <- rbind(provisional, vect)
	}


	# Format output
	emsu <- paste0('E(MS<sub>U/', paste(factors, collapse=""),'</sub>)')
	row_names <- c(emsu, lapply(effect_strings, function(x) paste0('E(MS<sub>', x, '</sub>)')))
	weights <- c("", paste(weights, '&sigma;<sub>', effect_strings, '</sub>', sep=''))
	output <- cbind(row_names, c('&nbsp;&nbsp;&sigma;&nbsp;&nbsp;'), weights, provisional)
	output <- output[, colSums(output == "") != nrow(output)]

	tablegen(output,ncol(output))
}
