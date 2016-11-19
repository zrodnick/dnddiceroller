#' Roll d20
#'
#' This function allows you to roll 20 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @param DC Allows user to enter a Difficulty Class to beat.
#' @param AC Allows user to enter an Armor Class to beat.
#' @param adv Used for advantage or disadvantage. If TRUE, dice will be rolled using advantage. If FALSE, dice will be rolled using disadvantage. Defaults to NA. This will only work if dice is set to 1.
#' @export
#' @examples roll_d20(1, 5, adv=TRUE)
#' roll_d20()

roll_d20 <- function(dice = 1, bonus = 0, each = FALSE, DC=NULL, AC=NULL, adv=NA){
  if(each==TRUE){roll <- sample(1:20, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d20 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(roll), "\n")
  total <- sum(roll+bonus)
  }

  if(is.na(adv)==FALSE & dice==1){
    if(adv==TRUE){
    writeLines(paste("Rolling 1d20 +", bonus, "with advantage."))
    roll_a <- sample(1:20, 1, replace=TRUE)
    roll_b <- sample(1:20, 1, replace=TRUE)
    writeLines(paste("Rolled", roll_a, "and", roll_b, "taking the highest."))
        if(roll_a>=roll_b){total <- roll_a+bonus}
        else{total <- roll_b+bonus}
    }

    else{
      writeLines(paste("Rolling 1d20 +", bonus, "with disadvantage."))
    roll_a <- sample(1:20, 1, replace=TRUE)
    roll_b <- sample(1:20, 1, replace=TRUE)
    writeLines(paste("Rolled", roll_a, "and", roll_b, "taking the lowest."))
    if(roll_a>=roll_b){total <- roll_b+bonus}
    else{total <- roll_a+bonus}
    }
      }

  if(dice>1 & is.na(adv)==FALSE){
    total <- NULL
    writeLines("You cannot roll more than one d20 when using advantage or disadvantage!")
  }

  if(is.na(adv)==TRUE & each==FALSE){roll <- sample(1:20, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d20 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  total <- sum(roll)+bonus}

 writeLines(paste("Total:", total))

if(!is.null(DC)){
  if(DC>total){cat("Check failed!")}
  else{cat("Check succeeded!")}
}

  if(!is.null(AC)){
    if(AC>total){cat("Missed!")}
    else{cat("Hit!")}
  }
}

#' Roll d4
#'
#' This function allows you to roll 4 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_d4()

roll_d4 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:4, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d4 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:4, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d4 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll d6
#'
#' This function allows you to roll 6 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @examples
#' roll_d6(4, 2, each=FALSE)
#' roll_d6()

roll_d6 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:6, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d6 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:6, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d6 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll d8
#'
#' This function allows you to roll 8 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_d8()

roll_d8 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:8, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d8 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:8, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d8 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll d10
#'
#' This function allows you to roll 10 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_d10()

roll_d10 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:10, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d10 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:10, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d10 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll d12
#'
#' This function allows you to roll 12 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_d12()

roll_d12 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:12, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d12 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:12, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d12 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll d100
#'
#' This function allows you to roll 100 sided dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_d100()

roll_d100 <- function(dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:100, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d100 + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:100, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d100 + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

#' Roll dx
#'
#' This function allows you to roll custom sided dice.
#' @param x The number of sides on the dice.
#' @param dice The number of dice to be rolled. Defaults to 1
#' @param bonus Any bonus amount to be added or subtracted from your roll. Defaults to 0
#' @param each Specifies if the bonus should be added to each individual die roll. If FALSE, the bonus will be added to the total rolls. Defaults to FALSE.
#' @export
#' @example
#' roll_dx(3)

roll_dx <- function(x, dice = 1, bonus = 0, each = FALSE){
  if(each==FALSE){roll <- sample(1:x, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d", x, " + ", bonus, sep="", collapse=""))
  cat(roll, "\n")
  writeLines(paste("Total:", sum(roll) + bonus))}
  else{mod_roll <- sample(1:x, dice, replace=TRUE)
  writeLines(paste("Rolling ", dice, "d", x, " + ", (bonus*dice), sep="", collapse=""))
  cat(as.vector(mod_roll), "\n")
  writeLines(paste("Total:", sum(mod_roll+bonus)))}
}

