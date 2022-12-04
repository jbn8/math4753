#' Title ntickets continuous
#'
#' @param N number of seats on the plane
#' @param gamma probability of plane being overbooked
#' @param p probability of of a "show"
#'
#' @return creates a plot of the objective function cs n
#' @export
#'
#' @examples
#' ntickets_continuous(N=200, gamma= 0.02, p=0.95)
ntickets_continuous<- function(N, gamma, p){


  # Caluclations to find the optimal ticket number to sell

  n<- seq(N+0.5, N+N/10, by=0.1) #vector of our sample n


  q= 1-p
  mu = n * p # formula for mu for a binomial
  sd <- n * p * q
  sd<- sqrt(sd) # formula for sd for a binomial

  objective_function <- N + 0.5 - qnorm(1-gamma, mu, sd)


  # index of the n vector that contains our root

  ind<- which.min(abs(objective_function))

  # Use the index to get the optimized ticket sale value from the n vector
  optimized_ticket_sale<- n[ind]


  # ------- PLOTTING ---------

  # create a vector for the f given n (objective function)

  output<-c()
  for (input in n){

    q= 1-p
    mu = input * p # formula for mu for a binomial

    sd<- sqrt(input*p*q) # formula for sd for a binomial
    val<- 1- gamma-pnorm(N+0.5, mu, sd)
    # add the value to the output vector
    output<- append(output, val)

  }

  # dataframe for n and f(n) =0
  dataFrame<- data.frame(n, output)
  dataFrame


  horizontalLine <- 0.0
  verticleLine <- optimized_ticket_sale

  title<- paste("Objective VS n to find the optimal tickets sold.\n (" , optimized_ticket_sale, ")",
                "gamma=",gamma, "N=", N, "continuous")



  #plot(output~n, mian= title, xlab = "n", ylab = "Objective", pch = 21, cex = 1.0, ,ylim = c(0, 1.0), xlim = c(0, 1.1 *max(n)))
  continuousPlot<- plot(dataFrame, main = title, ylim = c(0, 1.0), xlim = c(N, max(n)),
                        ylab = "Objective", xlab = "n", type = "l", col = "black")



  # add the horizonal lines
  abline(h = horizontalLine, col = "Blue")
  abline (v= verticleLine, col = "Blue")

  # print a named list
  a<- paste("nc= ", optimized_ticket_sale)
  b<- paste("N= ", N)
  c<- paste("p= ", p)
  d<- paste("gamma= ", gamma)

  list(a,b,c,d)

}



