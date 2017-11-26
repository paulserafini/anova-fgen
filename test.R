# sample input
#factors <- c('A', 'B')
#factor_type <- c(1, 0)
# 1 = fixed, 0 = random

tablegen <- dget("tablegen.r")

function (factors, factor_type) {

	# Create a list of the effects from the factors
	num_factors <- length(factors)
	effects <- c()
	for (i in 1:num_factors) {
		iway_factorcombos <- combn(factors, i)
		iway_effects <- lapply(1:ncol(iway_factorcombos), function(i) iway_factorcombos[,i])
		effects <- c(effects, iway_effects)
	}
	num_effects <- length(effects)

	# Create a list with the type for each effect
	effect_type <- c()
	for (i in 1:num_factors) {
		iway_typecombos <- combn(factor_type, i)
		iway_types <- lapply(1:ncol(iway_typecombos), function(c) iway_typecombos[,c])
		effect_type <- c(effect_type, iway_types)
	}
	effect_type <- lapply(effect_type, function(effect) prod(effect))

	# Create denominators for fixed effects
	lowercase <- unlist(lapply(factors, function(v) paste0('(',tolower(v), '-1)')))
	df <- c()
	for (i in 1:num_factors) {
		iway_dfcombos <- combn(lowercase, i)
		iway_df <- lapply(1:ncol(iway_dfcombos), function(i) paste(iway_dfcombos[,i], collapse=""))
		df <- c(df, iway_df)
	}
	
	# Create denominator for random effects
	levels <- lapply(1:num_effects, function(i) paste(effects[[i]], collapse=""))
	levels <- unlist(lapply(levels, function(v) tolower(v)))

	# Assign appropriate denominator to list
	denominators <- c()
	for (i in 1:num_effects) {
		if (effect_type[[i]] == 1) {
			denominators[[i]] <- df[[i]]
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
	provisional <- matrix(NA, nrow=1, ncol=num_effects)
	for (a in 1:num_effects) {

		effect_strings <- lapply(effects, function(x) paste(x, collapse=""))
		matrix <- cbind(weights,tolower(effect_strings),effect_strings)

		# Remove focal factors
		for (e in effects[[a]]) {
			char <- tolower(e)
			matrix <- matrix[ grepl(char, matrix[,2]),, drop=FALSE ]
			matrix[,2] <- gsub(char, "", as.character(matrix[,2]))
		}

		# Remove rows containing fixed factors
		myLetters <- letters[1:26]
		for (f in factors) {
			char <- tolower(f)
			num <- match(char, myLetters)
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

			padding <- matrix(NA, nrow=diff, ncol=1)
			matrix <- rbind(matrix, padding)
		} else {
		  matrix <- matrix(NA, nrow=diff, ncol=1)
		}
		matrix <- t(matrix)
		provisional <- rbind(provisional, matrix)
	}


	# Format output
	effects <- lapply(effects, function(effect) paste(effect, collapse=""))
	output <- cbind(c("",effects), c("",weights), provisional)

	output[,2] <- paste(output[,2], output[,1], sep="&sigma;<sub>")
	output[,2] <- paste(output[,2], '</sub>')
	z <- paste(factors, collapse="")
	output[1,1] <- paste('U/', z, sep="")
	output[,1] <- paste('E(MS<sub>', output[,1], '</sub>)', sep="")
	output[1,2] <- "&sigma;"
	dimnames(output) <- NULL

	output <- output[, colSums(is.na(output)) != nrow(output)]
	
	output[is.na(output)] <- ""
	
	tablegen(output,FALSE)
}
