# slotMachine.R

# Play with odds

# User input params
# payoutRate: 80 - 99%; or not-set;
# numConsecutivePlays: 1 - 1000

# For calculations
# pricePerPlay: $1.00
# 1/7 per panel;

# prizes: x300
# 3 occurrences of '7' = x300

cap <- factor(c(1, 2, 3, 4, 5, 7))

costPerPlay = 1
grandPrize = costPerPlay * 300
prize = costPerPlay * 10

loser <- 0
winner <- 0
grandWinner <- 0

numPlay <- 1000
for (i in 1:numPlay) {
        box1 <- sample(cap, 1)
        box2 <- sample(cap, 1)
        box3 <- sample(cap, 1)
        
        if (box1 == 7 & box2 == 7 & box3 == 7) {
               grandWinner <- grandWinner + 1 
        } else if (box1 == box2 & box1 == box3) {
                winner <- winner + 1
        } else {
                loser <- loser + 1
        }
}

paste("JACKPOT = ", grandWinner)
paste("WINNER = ", winner)
paste("LOSER = ", loser)

print("-------------------- 2ND -------------------")

box1 <- sample(cap, numPlay, replace=TRUE)
box2 <- sample(cap, numPlay, replace=TRUE)
box3 <- sample(cap, numPlay, replace=TRUE)

grandWinner <- sum(box1 == 7 & box2 == 7 & box3 == 7)
grandWinnerWhich <- which(box1 == 7 & box2 == 7 & box3 == 7)

winner <- sum(box1 != 7 & box1 == box2 & box2 == box3)
winnerWhich <- which(box1 != 7 & box1 == box2 & box2 == box3)

loser <- numPlay - grandWinner - winner

paste("NUM PLAYS = ", numPlay, " --- $ Spent = $", costPerPlay * numPlay)

paste("JACKPOT = ", grandWinner, " --- $ Prize = $", grandPrize * grandWinner)
paste("WINNER = ", winner, " --- $ Prize = $", prize * winner)
paste("LOSER = ", loser, " --- $ Lost = $", costPerPlay * loser)

paste("Player NET = $", (grandPrize * grandWinner) + (prize * winner) )


# ---------------------------------------------------
numPlay <- 300
costPerPlay <- 1        
cap <- factor(c(1, 2, 3, 4, 5, 7))

# Initialize a data.frame to return
dfNet <- data.frame(c(1:numPlay))
dfNet <- cbind(dfNet, c(order(1:(numPlay * costPerPlay - costPerPlay), decreasing = TRUE),0))
names(dfNet) <- c("playNum", "netBalance")

prize1 = costPerPlay * 300
prize2 = costPerPlay * 10
prize3 = costPerPlay

r1 <- sample(cap, numPlay, replace=TRUE)
r2 <- sample(cap, numPlay, replace=TRUE)
r3 <- sample(cap, numPlay, replace=TRUE)

whichPrize1 <- which(r1 == 7 & r2 == 7 & r3 == 7)
whichPrize2 <- which(r1 != 7 & r1 == r2 & r2 == r3)

for (i in 1:numPlay) {
        if (r1[i] == 7 & r2[i] == 7 & r3[i] == 7) {
                dfNet$netBalance[i:numPlay] <- dfNet$netBalance[i:numPlay] + prize1
        } else if (r1[i] != 7 & r1[i] == r2[i] & r2[i] == r3[i]) {
                dfNet$netBalance[i:numPlay] <- dfNet$netBalance[i:numPlay] + prize2                
        } else {
                # Do Nothing
        }       
}

# return a Net Balance dataframe to plot
dfNet <- rbind(c(0, numPlay * costPerPlay), dfNet)


plot(dfNet$playNum, dfNet$netBalance, type="l")



