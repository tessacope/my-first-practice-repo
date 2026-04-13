DiceRoller <- function(die_type, number_of_dice, output = "set") {
  rolls <- sample(1:die_type, size = number_of_dice, replace = TRUE)
  
  if (output == "sum") {
    return(sum(rolls))
  } else {
    return(rolls)
  }
}

n_rolls <- 10000

results <- replicate(n_rolls, DiceRoller(4, 2, output = "set"))

results_df <- data.frame(
  die1 = results[1, ],
  die2 = results[2, ]
)

combo_table <- table(results_df$die1, results_df$die2)

relative_freq <- prop.table(combo_table)

cat("Relative Frequencies Table for Two d4 Dice:\n")
print(relative_freq)
