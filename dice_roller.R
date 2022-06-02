# shadowrunR

roll_simple <- function(n, type = 6) {
    roll <- sample(1:type, n, replace = T)
    return(sort(roll))
}


roll <- function(n, type = 6) {
    n <- as.integer(n)
    type <- as.integer(type)
    roll <- sample(1:type, n, replace = T)
    res_tbl <- integer(length = type)
    names(res_tbl) <- paste0("#", 1:type)
    tbl <- table(roll)
    idx <- as.integer(names(tbl))
    res_tbl[idx] <- tbl

    return(res_tbl)
}


edge_roll <- function(n) {
    orig_roll <- roll(n)
    roll_m <- t(as.matrix(orig_roll))
    rownames(roll_m) <- "orig"
    sixes <- orig_roll["#6"]

    if (sixes > 0) {

        # Start rerolling sixes until none left
        while (sixes > 0) {
            rr <- roll(sixes)
            sixes <- rr["#6"]
            roll_m <- rbind(roll_m, rr)
        }

        # Calculate total hits
        hits <- c("#5", "#6")
        cumulative_hits <- colSums(roll_m[, hits])
        orig_roll[hits] <- cumulative_hits
    }

    # Get stats for the modified roll
    stats <- calculate_roll_stats(orig_roll)
    stats[["Roll"]] <- roll_m
    stats[["Sums"]] <- stats[["Result"]]
    return(stats)
}


calculate_roll_stats <- function(roll, miss = 1, hit = c(5, 6)) {

    # Calculate hits and misses
    misses <- sum(roll[miss])
    hits <- sum(roll[hit])
    total <- sum(roll)

    # Glitch status calculation
    glitch <- (hits - misses) < 0
    critical_glitch <- (misses / total > 0.5) & glitch
    glitch_status <- ifelse(glitch, yes = 1, no = 0)
    glitch_status <- ifelse(critical_glitch, yes = 2, no = glitch_status)

    # Create result list
    res <- c(total, hits, misses, glitch_status)
    names(res) <- c("Pool", "Hit", "Miss", "Glitch")
    if (type == 6) {
        return(list(Roll = roll, Result = res))
    } else {
        return(list(Roll = roll, Result = "Not available for this roll yet"))
    }
}


probability_of_hits <- function(hits, pool, p_hit = 2 / 6) {
    num_comb <- choose(pool, hits)
    # Use the binomial probability formula to calculate the
    # probabiltity of rolling exactly the given hit amount
    prob <- num_comb * p_hit^hits * (1 - p_hit)^(pool - hits)
    return(prob)
}


cumulative_prob_of_hits <- function(hits, pool, p_hit = 2 / 6) {
    # Calculates the individual hit results for hits <= x <= pool,
    # giving the probability of reaching at least the given hit amount
    if (hits > pool) {
        prob <- 0
    } else {
        all_probs <- vapply(hits:pool, numeric(1),
            FUN = probability_of_hits,
            pool = pool, p_hit = p_hit
        )
        prob <- sum(all_probs)
    }
    return(prob)
}


extended_test <- function(pool, target = NA) {
    pool <- as.integer(pool)
    # Prepare result matrices by making a throwaway roll
    s <- calculate_roll_stats(roll(1))
    s_roll <- s[["Roll"]]
    s_res <- s[["Result"]]
    m <- matrix(nrow = pool, ncol = length(s_res) + 1)
    roll_m <- matrix(nrow = pool, ncol = length(s_roll))
    colnames(m) <- c(names(s_res), "CumSum")
    rownames(m) <- paste0("R", 1:pool, "|")
    colnames(roll_m) <- names(s_roll)
    rownames(roll_m) <- paste0("R", 1:pool, "|")
    rm(s, s_roll, s_res)

    # Initialize while-loop parameters
    counter <- 1
    edge <- T
    cumsum <- 0

    # Populate result matrix
    while (counter <= pool) {
        # Make the roll
        r <- roll(pool - counter + 1)
        roll <- calculate_roll_stats(r)
        cumsum <- sum(c(cumsum, roll[["Result"]]["Hit"]))
        # Add roll info
        roll_m[counter, ] <- roll[["Roll"]]
        m[counter, ] <- c(roll[["Result"]], cumsum)


        # Glitch reroll prompt
        if ((m[counter, "Glitch"] > 0) & edge) {
            # Give information

            print(m[complete.cases(m), ])
            print(c("Total hits" = sum(m[, "Hit"], na.rm = T), "Total glitches" = sum(m[, "Glitch"] > 0, na.rm = T)))
            message(ifelse(m[counter, "Glitch"] > 1, yes = "Critical Glitch!", no = "Glitch"))
            # Prompt
            prompt <- prompt_function("Reroll using edge?: [y/N]")
            if (prompt %in% c("y", "yes")) {
                # Modify row names to indicate edge use
                rownames(m)[counter] <- paste0("R", counter, "| E")
                rownames(roll_m)[counter] <- paste0("R", counter, "| E")
                edge <- F
                # Add 1 to dice pool = reroll. Happens only once since the edge flag is set to FALSE
                counter <- counter - 1
            }
        }

        # Abort extended test if a critical glitch occurs and is not mitigated by an edge reroll
        if (any(m[, "Glitch"] == 2, na.rm = T)) {
            message("Unmitigated Critical Glitch!")
            break
        }
        # Check if given target has been reached
        if (!is.na(target)) {
            total_sum <- sum(m[, "Hit"], na.rm = T) - sum(m[, "Glitch"], na.rm = T)
            if (total_sum >= target) {
                break
            }
        }
        counter <- counter + 1
    }
    # Remove cumsum column from sum results, useless
    sums <- colSums(m[, -5], na.rm = T)
    sums["Glitch"] <- sum(m[, "Glitch"] > 0, na.rm = T)

    # Remove any full NA rows
    m <- m[complete.cases(m), ]
    roll_m <- roll_m[rowSums(roll_m, na.rm = T) > 0, ]

    res <- list(Roll = roll_m, Result = m, Sums = sums)
    return(res)
}


pretty_print <- function(res) {
    message("---------------")
    message("Roll table:")
    print(res[["Roll"]], quote = F, na.print = "")
    message()
    message("Result table:")
    print(res[["Result"]], quote = F, na.print = "")
    message()
    message("Total sums:")
    print(res[["Sums"]], quote = F, na.print = "")
    message("---------------")
    message()
}


plot_roll <- function(roll) {
    barplot(roll, names.arg = paste0(names(roll), "\n(", roll, ")"))
}


prompt_function <- function(msg = "Default message.") {
    if (interactive()) {
        ans <- readline(prompt = msg)
    } else {
        cat(msg)
        ans <- readLines("stdin", n = 1)
    }
    return(tolower(ans))
}


help_message <- function() {
    message("Param 1: Task to run [help|extended|roll|simple|edge_roll|roll_prob]")
    message("Param 2: Amount of dice to roll")
    message("Param 3: Target hit amount for extended test OR Dice type for roll/simple_roll")
}


if (!interactive()) {
    # 1: task, 2: dice pool, 3: target amount of hits
    args <- commandArgs(trailingOnly = T)
    task <- args[1]
    dice <- as.integer(args[2])
    target <- as.integer(args[3])
    type <- ifelse(is.na(target), yes = 6, no = target)
    x <- switch(task,
        "help"        = help_message(),
        "extended"    = pretty_print(extended_test(pool = dice, target = target)),
        "roll"        = print(calculate_roll_stats(roll(n = dice, type = type))),
        "simple" = print(roll_simple(n = dice, type = type), row.names = F),
        "edge_roll"   = pretty_print(edge_roll(dice)),
        "roll_prob"   = print(cumulative_prob_of_hits(target, dice)),
        help_message()
    )
}