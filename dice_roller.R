# shadowrunR

roll <- function(n, type=6){
	roll <- sample(1:type, n, replace = T)
	return(sort(roll))
}
roll_dice <- function(n, type=6){
	# Make roll
	roll <- roll(n, type=type)
	misses <- sum(roll == 1)
	hits <- sum(roll %in% c(5,6))

	# Glitch status calculation
	glitch <- (hits - misses) <= 0
	critical_glitch <- misses / n > 0.5 & glitch
	glitch_status <- ifelse(glitch, yes = 1, no = 0)
	glitch_status <- ifelse(critical_glitch, yes = 2, no = glitch_status)

	# Create result list
	res <- c(n, hits, misses, glitch_status)
	names(res) <- c("Pool", "Hits", "Misses", "Glitch")
	return(list(Roll = roll, Result = res))
}

extended_test <- function(pool, type=6, edge = F){
	
	# Prepare result matrices
	m <- matrix(nrow = pool, ncol = 4)
	roll_m <- matrix(nrow = pool, ncol = pool)
	colnames(m) <- c("Pool", "Hits", "Misses", "Glitch")
	rownames(m) <- 1:pool
	colnames(roll_m) <- make.names(1:pool)	
	rownames(roll_m) <- 1:pool
	counter <- 0

	# Populate result matrix
	while(counter < pool){
		roll <- roll_dice(pool-counter, type=type)
		length(roll[["Roll"]]) <- pool
		roll_m[counter+1,] <- roll[["Roll"]]
		m[counter+1, ] <- roll[["Result"]]

		# Glitch reroll prompt
		if(roll[["Result"]]["Glitch"] > 0  & edge == F){
			tm <- m[complete.cases(m), ]
			print(tm)
			ifelse(nrow(tm) > 1, yes = print(colSums(tm)), no = "")
			message("(1) Glitch or (2) Critical Glitch!")
			prompt <- tolower(readline(prompt="Reroll using edge?: [y/N]"))
			if(prompt %in% c("y", "yes")){
				roll <- roll_dice(pool-counter, type=type)
				length(roll[["Roll"]]) <- pool
				m[counter+1, ] <- roll[["Result"]]
				roll_m[counter+1,] <- roll[["Roll"]]
				rownames(m)[counter+1] <- paste(counter+1, "Edge")
				rownames(roll_m)[counter+1] <- paste(counter+1, "Edge")
				edge <- T
			}
		}
		counter <- counter + 1
	}
	sums <- colSums(m)
	return(list(Rolls = roll_m, Result = m, Sums = sums))
}


extended_test(5)