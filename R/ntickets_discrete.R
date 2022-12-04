#' Title ntickets discrete
#'
#' @param N number of seats on the plane
#' @param gamma probability of the plane overbooking
#' @param p probability of a "show"
#'
#' @return  plot of the objective function vs n showing the optimized tickets sold
#' @export
#'
#' @examples tickets_discrete(N=200, gamma=0.02, p=0.95)
ntickets_discrete<- function(N, gamma, p){


  # Generate a our sample; Starting at N, we take steps of 1

  n<- seq(N, floor(N+N/10), by=1) #vector of our sample n

  #Objective Function
  objective<- 1- gamma-pbinom(q=N,
                              size=n,
                              prob=p)

  # index of the n vector that contains our root
  ind<- which.min(abs(objective))

  # get the n value
  optimized_ticket_sale<- n[ind]




  # ---- PLOTTING -----

  title<- paste("Objective VS n to find the optimal tickets sold.\n (" , optimized_ticket_sale, ")", "gamma=", gamma, "N=", N, "discrete")


  # create a vector for the f given n (objective function)


  output<-c()
  for (input in n){
    val<- 1- gamma-pbinom(q=N, size=n, prob=p)

    output<- append(output, val)

  }



  dataFrame<- data.frame(n, output)
  dataFrame



  horizontalLine <- 1- gamma-pbinom(q=N, size=optimized_ticket_sale, prob=p)
  verticleLine <- optimized_ticket_sale




  discretePlot<- plot(dataFrame, main = title, ylim = c(0, 1.0), xlim = c(N, max(n)), ylab = "objective", xlab= "n", col = "blue")
  discretePlot

  abline(h = horizontalLine, col = "RED")
  abline (v= verticleLine, col = "RED")


  # print a named list

  a<- paste("nd= ", optimized_ticket_sale)
  b<- paste("N= ", N)
  c<- paste("p= ", p)
  d<- paste("gamma= ", gamma)

  list(a,b,c,d)
}


