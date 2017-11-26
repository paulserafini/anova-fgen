tablegen <- dget("tablegen.R")
function (factors, factor_type) {

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

	effects <- combogen(factors, num_factors)
	effect_type <- combogen(factor_type, num_factors)
	effect_df <- combogen(factor_df, num_factors)
	effect_df <- lapply(effect_df, function(i) paste(i, collapse=""))

	num_effects <- length(effects)
	effect_type <- lapply(effect_type, function(effect) prod(effect))

	# Create denominator for random effects
	levels <- lapply(1:num_effects, function(i) paste(effects[[i]], collapse=""))
	levels <- unlist(lapply(levels, function(v) tolower(v)))

	# Assign appropriate denominator to list
	denominators <- c()
	for (i in 1:num_effects) {
		if (effect_type[[i]] == 1) {
			denominators[[i]] <- effect_df[[i]]
		} else {
			denominators[[i]] <- levels[[i]]
		}
	}

	# Create numerator
	numerator <- tolower(paste(factors, collapse=""))
	numerator <- paste0('n',numerator)
	numerators <- rep(numerator, num_effects)
	
	# Create weights
	weights <- c()
	for (i in 1:num_effects) {
		if (effect_type[[i]] == 0) {
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
	
	# Create provisionals
	provisional <- matrix("", nrow=1, ncol=num_effects)
	for (a in 1:num_effects) {

		effect_strings <- lapply(effects, function(x) paste(x, collapse=""))
		matrix <- cbind(weights,tolower(effect_strings),effect_strings)

		# Remove focal effect
		for (e in effects[[a]]) {
			char <- tolower(e)
			matrix <- matrix[ grepl(char, matrix[,2]),, drop=FALSE ]
			matrix[,2] <- gsub(char, "", as.character(matrix[,2]))
		}

		# Remove fixed effects
		alphabet <- c('a','b','c','d')
		for (f in factors) {
			char <- tolower(f)
			num <- match(char, alphabet)
			effect <- effect_type[[num]]
			matrix[,2] <- gsub(char, effect, as.character(matrix[,2]))
		}
		matrix <- matrix[ !grepl(1, matrix[,2]),, drop=FALSE ]
		matrix <- matrix[ matrix[,2] != "",, drop=FALSE ]

		# Bind the provisionals to the matrix
		rows <- nrow(matrix)
		diff <- num_effects - rows
		if (rows > 0) {
		  # Delete unnecessary column first
		  matrix <- matrix[,-2, drop=FALSE]
		  matrix[,1] <- paste(matrix[,1], matrix[,2], sep="&sigma;<sub>")
		  matrix[,1] <- paste(matrix[,1], '</sub>')
		  matrix <- matrix[,-2, drop=FALSE]

			padding <- matrix("", nrow=diff, ncol=1)
			matrix <- rbind(matrix, padding)
		} else {
		  matrix <- matrix("", nrow=diff, ncol=1)
		}
		matrix <- t(matrix)
		provisional <- rbind(provisional, matrix)
	}


	# Format output
	effects <- lapply(effects, function(effect) paste(effect, collapse=""))
	output <- cbind(c("",effects), c("",weights), provisional)
	output[,2] <- paste(output[,2], '&sigma;<sub>', output[,1], '</sub>', sep='')
	output[1,1] <- paste('U/', paste(factors, collapse=""), sep="")
	output[,1] <- paste('E(MS<sub>', output[,1], '</sub>)', sep='')
	output <- output[, colSums(output == "") != nrow(output)]

	tablegen(output,FALSE)
}
