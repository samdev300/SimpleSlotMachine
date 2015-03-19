
library(ggplot2)
library(shiny)

prize1x <- 300
prize2x <- 20

r1 <- ""
r2 <- ""
r3 <- ""
dfResult <- ""
dfManyMachines <- ""

# ----------------------------------------------------------------------------------------

playSlot <- function(numPlay, costPerPlay) { 
        prize1 <- prize1x * costPerPlay
        prize2 <- prize2x * costPerPlay
        
        reel <- factor(c(1, 2, 3, 4, 5, 6, 7, 8))
        
        # Initialize a data.frame to return
        dfNet <- data.frame(c(1:numPlay), sort(seq(costPerPlay, costPerPlay*numPlay, costPerPlay), decreasing=TRUE), seq(-costPerPlay, -(costPerPlay*numPlay), -costPerPlay))
        names(dfNet) <- c("playNum", "pocketStatus", "netBalance")        
        
        # Do the rolls
        r1 <<- sample(reel, numPlay, replace=TRUE)
        r2 <<- sample(reel, numPlay, replace=TRUE)
        r3 <<- sample(reel, numPlay, replace=TRUE)
                
        for (i in 1:numPlay) {                
                if (r1[i] == 7 & r2[i] == 7 & r3[i] == 7) {
                        dfNet$netBalance[i:numPlay] <- dfNet$netBalance[i:numPlay] + prize1
                        dfNet$pocketStatus[i:numPlay] <- dfNet$pocketStatus[i:numPlay] + prize1
                } else if (r1[i] != 7 & r1[i] == r2[i] & r2[i] == r3[i]) {
                        dfNet$netBalance[i:numPlay] <- dfNet$netBalance[i:numPlay] + prize2
                        dfNet$pocketStatus[i:numPlay] <- dfNet$pocketStatus[i:numPlay] + prize2
                } else {
                        # Do Nothing
                }       
        }
        
        # return a Net Balance dataframe to plot
        dfNet <- rbind(c(0, numPlay * costPerPlay, 0), dfNet)
}

# ----------------------------------------------------------------------------------------

playManySlots <- function(dfMMResult, numMachine, numPlay, costPerPlay, session) {
        prize1 <- prize1x * costPerPlay
        prize2 <- prize2x * costPerPlay
                
        # Show progress
        progress <- shiny::Progress$new(session, min=1, max=10)
        on.exit(progress$close())        
        progress$set(message = "Just a moment...")
        progressDivide <- numMachine / 10
        
        reel <- factor(c(1, 2, 3, 4, 5, 6, 7, 8))
        
        for (i in 1:(numMachine-1)) {
                # Show progress
                progress$set(value=(i %/% progressDivide))
                
                r1 <- sample(reel, numPlay, replace=TRUE)
                r2 <- sample(reel, numPlay, replace=TRUE)
                r3 <- sample(reel, numPlay, replace=TRUE)
                
                numPrize1 <- sum(r1 == 7 & r2 == 7 & r3 == 7)
                numPrize2 <- sum(r1 != 7 & r1 == r2 & r2 == r3)
                
                thisTotalWon <- (numPrize1 * prize1 + numPrize2 * prize2)
                
                dfMMResult <- rbind(dfMMResult, c(i+1, thisTotalWon, numPrize1, numPrize2))
        }
        dfMMResult
}


# ----------------------------------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Machine #1 Plot
  output$netPlot <- renderPlot({
          # Define costPerPlay
          costPerPlay <- as.numeric(input$costPerPlay)
          
          dfResult <<- playSlot(input$numPlay, costPerPlay)
          
        ggplot(data=dfResult, aes(x=playNum, y=netBalance)) +
                geom_line(colour="darkgreen") +
                geom_abline(intercept=0, slope=0, colour="black") +
                labs(title="Net Earnings From Simple Slot Machine", x="Play #", y="Net Balance in Player's Pocket ($)")
  })
   
  # Machine #1 Summary
  output$playSummary <- renderText({
          # Define costPerPlay
          costPerPlay <- as.numeric(input$costPerPlay)
          
          amountSpent <- input$numPlay * costPerPlay 
          netTotal <- dfResult$pocketStatus[input$numPlay+1]
          numPrize1 <- sum(r1 == 7 & r2 == 7 & r3 == 7)
          numPrize2 <- sum(r1 != 7 & r1 == r2 & r2 == r3)
          
          # Display Summary
          paste("# of consecutive plays: ", prettyNum(input$numPlay, big.mark=","), 
                "<br />Cost per play: $", format(costPerPlay, digits=2),
                "<br />Total spent: $", prettyNum(amountSpent, big.mark=","),
                "<br /><strong>Total won: $", prettyNum(netTotal, big.mark=","), "</strong>",
                "<br /><strong>Net total: $", prettyNum(netTotal - amountSpent, big.mark=","), "</strong>",
                "<br />",
                "<br /><strong>Prize 1 Wins = ", numPrize1, "/", input$numPlay, "=", round(numPrize1 / input$numPlay * 100, digits=2), "%</strong>",
                "<br /><strong>Prize 2 Wins = ", numPrize2, "/", input$numPlay, "=", round(numPrize2 / input$numPlay * 100, digits=2), "%</strong>")
  })

  # Plot the summary for Many Machines Simulation
  output$manyMachinesPlot <- renderPlot({ 
          # Define costPerPlay
          costPerPlay <- as.numeric(input$costPerPlay)
          
          dfMMResult <- data.frame(1, dfResult$pocketStatus[input$numPlay+1], sum(r1 == 7 & r2 == 7 & r3 == 7), sum(r1 != 7 & r1 == r2 & r2 == r3))
          dfMMResult <- playManySlots(dfMMResult, input$numMachine, input$numPlay, as.numeric(input$costPerPlay), session)
          names(dfMMResult) <- c("machineNum", "playerResult", "numPrize1", "numPrize2")
                    
          # Assign to global var for an access from other functions
          dfManyMachines <<- dfMMResult
          
          bWidth <- costPerPlay * 10
          ggplot(data=dfMMResult, aes(x=playerResult)) +
                  geom_histogram(binwidth=bWidth, fill="gold2") +
                  geom_abline(intercept=0, slope=0, colour="black") +
                  labs(title="Machine Payouts for Dealer", x="Payouts ($)")
          
  })
  
  # Payout Summary
  output$payoutSummary <- renderText({
          # Define costPerPlay
          costPerPlay <- as.numeric(input$costPerPlay)
          
          totalSpent <- input$numPlay * costPerPlay * input$numMachine
          playerNetTotal <- sum(dfManyMachines$playerResult)
          
          totalNumPlay <- input$numPlay * input$numMachine
          totalNumPrize1 <- sum(dfManyMachines$numPrize1)
          totalNumPrize2 <- sum(dfManyMachines$numPrize2)
          sdWin <- round(sd(dfManyMachines$playerResult), digits=2)
          
          # Display summary
          paste("# of consecutive plays per machine: ", prettyNum(input$numPlay, big.mark=","),
                "<br /># of machines: ", prettyNum(input$numMachine, big.mark=","),
                "<br />Cost per play: $", format(costPerPlay, digits=2),
                "<br />Total spent by the players: $", prettyNum(totalSpent, big.mark=","),
                "<br />Total payout to the players: $", prettyNum(playerNetTotal, big.mark=","),
                "<br />Average payout per machine: $", prettyNum(round(mean(dfManyMachines$playerResult), digits=2), big.mark=","),
                "<br /><strong>Total dealer net gains from the slots: $", prettyNum(totalSpent - playerNetTotal, big.mark=","), "</strong>",
                "<br /><strong>Average dealer net gains from each slot: $", prettyNum(round((totalSpent - playerNetTotal)/input$numMachine, digits=2), big.mark=","), "</strong>",
                "<br /><strong>Total dealer net gains margin:", round(((totalSpent - playerNetTotal)/totalSpent)*100, digits=1), "%",
                "<br />",
                "<br /><strong>Prize 1 Wins = ", totalNumPrize1, "/", totalNumPlay, "=", round(totalNumPrize1 / totalNumPlay * 100, digits=2), "%</strong>",
                "<br /><strong>Prize 2 Wins = ", totalNumPrize2, "/", totalNumPlay, "=", round(totalNumPrize2 / totalNumPlay * 100, digits=2), "%</strong>",  
                "<br />",
                "<br /><h3>So...</h3>",
                "<strong>", input$numMachine, "people played", input$numPlay, "times on one machine per person.",
                "<br /><h3>And...</h3>",
                "", sum(dfManyMachines$playerResult == (input$numPlay * costPerPlay)), "player(s) are right where they started.  :-|",
                "<br />", sum(dfManyMachines$playerResult > (input$numPlay * costPerPlay)), "player(s) are happy. :-)",
                "<br />Of which, ", sum(dfManyMachines$playerResult >= ((input$numPlay * costPerPlay) + 2 * sdWin)), "player(s) have won $", prettyNum(round(2*sdWin, digits=2), big.mark=","), "(2 SD) or more and are SUPER HAPPY.",
                "<br />Meanwhile,", sum(dfManyMachines$playerResult < (input$numPlay * costPerPlay)), "player(s) went home infuriated and vowed revenge.</strong>")
  })
  
  
})


