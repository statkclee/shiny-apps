library(tidyverse)
library(DT)
library(reshape2)


mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
  J <- I / (12 * 100)
  N <- 12 * L
  M <- P * J / (1 - (1 + J)^(-N))
  monthPay <<- M
  # Calculate Amortization for each Month
  if (amort == TRUE) {
    Pt <- P # current principal or amount of the loan
    currP <- NULL
    while (Pt >= 0) {
      H <- Pt * J # this is the current monthly interest
      C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
      Q <- Pt - C # this is the new balance of your principal of your loan
      Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
      currP <- c(currP, Pt)
    }
    monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
    aDFmonth <<- data.frame(
      Month = 1:length(currP),
      Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
      Balance = c(currP[1:(length(currP))]),
      Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
      Principal = monthP,
      Interest = c((monthPay - monthP)[1:(length(monthP))])
    )
    aDFmonth <<- subset(aDFmonth, Year <= L * 12)
    aDFyear <- data.frame(
      Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
      Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
      Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
      Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
      Year = as.factor(na.omit(unique(aDFmonth$Year)))
    )
    aDFyear <<- aDFyear
  }
  if (plotData == TRUE) {
    aDFyear2 <- aDFyear %>%
      rename(
        Interest = Annual_Interest,
        Payment = Annual_Payment,
        Principal = Annual_Principal
      ) 
    
    aDFyear2$Year <- as.factor(aDFyear2$Year)
    aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
    
    aDFyear2 %>% 
      mutate(variable = ifelse(variable == "Principal", "원금", "이자") %>% 
               as.factor(.) %>% forcats::fct_rev(.)) %>%
      ggplot(aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "원금과 이자비율(%)",
             x = "연도") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(legend.title = element_blank(), legend.position = "top") +
        scale_fill_manual(values = c("gray50", "blue"))
  }
}

