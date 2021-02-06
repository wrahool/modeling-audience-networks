library(igraph)

load("networks/_100_1000_5_3_3_0_.Rdata")
g0 <- g

load("networks/_100_1000_5_3_3_0.2_.Rdata")
g02 <- g

load("networks/_100_1000_5_3_3_0.3_.Rdata")
g03 <- g

load("networks/_100_1000_5_3_3_0.5_.Rdata")
g05 <- g

load("networks/_100_1000_5_3_3_0.7_.Rdata")
g07 <- g

load("networks/_100_1000_5_3_3_1_.Rdata")
g1 <- g

gs <- list(g0, g03, g07, g1)
gs <- list(g0, g03, g1)

for(g in gs) {

  few_iterations_layout <- layout_with_fr(g, niter=30)
  many_iterations_layout <- layout_with_fr(g, niter=1000)
  # l <- layout_with_fr(g, niter=1000)

  par(mfrow=c(1,2))
  par(mar=c(0,0,0,0)+.1)
  
  plot(g,
     vertex.size = 3,
     vertex.label = NA,
     edge.color="gray",
     edge.width = 0.00000000000001,
     layout = few_iterations_layout)
  
  
  plot(g,
       vertex.size = 3,
       vertex.label = NA,
       edge.color="gray",
       edge.width = 0.00000000000001,
       layout = many_iterations_layout)
  
  x <- readline(prompt="Press any key to continue ")
}
