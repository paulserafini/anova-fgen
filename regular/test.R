tablegen <- dget("tablegen.R")
function (factors, factor_type) {
# factors is a vector of letters, e.g. c('A', 'B', 'C')
# factor_type is a vector of 1s and 0s where 1 = fixed and 0 = random, e.g. c(1, 0, 1)

  	# Generate vector of 2 way, 3 way, etc. combinations of input
	combogen <- function (input, num_factors) {
	  	output <- c()
		for (i in 1:num_factors) {
		  matt <- combn(input, i)
		  vect <- lapply(1:ncol(matt), function(i) matt[,i])
		  output <- c(output, vect)
		}
		return(output)
	}

	# Make vectors of the effects, their type , and their degrees of freedom
	num_factors <- length(factors)

	effects <- combogen(factors, num_factors)

	effect_type <- combogen(factor_type, num_factors)
	effect_type <- lapply(effect_type, function(effect) prod(effect))

	factor_df <- lapply(factors, function(v) paste0('(',tolower(v), '-1)'))
	effect_df <- combogen(factor_df, num_factors)
	effect_df <- lapply(effect_df, function(i) paste(i, collapse=""))

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
	for (i in 1:num_effects) {
		if (effect_type[[i]] == 0) { # remove common characters from numerator and denominator; if denominator is empty then weight = numerator
			num_pretrim <- unlist(strsplit(numerators[[i]], ""))
			den_pretrim <- unlist(strsplit(denominators[[i]], ""))
			num_trim <- paste(num_pretrim[!(num_pretrim %in% den_pretrim)], collapse="")
			den_trim <- paste(den_pretrim[!(den_pretrim %in% num_pretrim)], collapse="")
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
		weights[[i]] <- weight
	}

	# Assemble weights and variances
	effect_strings <- lapply(effects, function(x) paste(x, collapse=""))
	weightplusvariance <- paste0(weights,'&sigma;<sub>',effect_strings,'</sub>')
	
	# Create provisionals for each effect
	provisional <- matrix("", nrow=1, ncol=num_effects)
	for (a in 1:num_effects) {

		matt <- cbind(weightplusvariance,tolower(effect_strings))

		# Rule out effects w/ out the focal effects
		for (e in effects[[a]]) {
			char <- tolower(e)
			matt <- matt[ grepl(char, matt[,2]),, drop=FALSE ]
			matt[,2] <- gsub(char, "", as.character(matt[,2]))
		}

		# Remove fixed effects
		alphabet <- c('a','b','c','d')
		for (f in factors) {
			char <- tolower(f)
			num <- match(char, alphabet)
			effect <- effect_type[[num]]
			matt[,2] <- gsub(char, effect, as.character(matt[,2]))
		}
		matt <- matt[ !grepl(1, matt[,2]),, drop=FALSE ]
		matt <- matt[ matt[,2] != "",, drop=FALSE ]

		# Pad vector and add it to provisionals matrix
		diff <- num_effects - nrow(matt)
		vect <- c(matt[,1], rep("", diff))
		provisional <- rbind(provisional, vect)
	}
	provisional <- provisional [-1,]

	# Format output
	top_row <- c(paste0('E(MS<sub>U/', paste(factors, collapse=""),'</sub>)'), '&sigma;', '', rep('', num_effects))
	row_names <- paste0('E(MS<sub>', effect_strings, '</sub>)')
	output <- cbind(row_names, c('&nbsp;&nbsp;&sigma;&nbsp;&nbsp;'), weightplusvariance, provisional)
	output <- rbind(top_row, output)
	output <- output[, colSums(output == "") != nrow(output)]

	tablegen(output,nrow(output))
}
