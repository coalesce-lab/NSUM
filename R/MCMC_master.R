nsum.simulate <- function(n, known, unknown, N, model="degree", ...)
{
  if(model=="degree") .simulate.rd(n, known, unknown, N, ...)
  else if(model=="barrier") .simulate.bar(n, known, unknown, N, ...)
  else if(model=="transmission") .simulate.trans(n, known, unknown, N, ...)
  else if(model=="combined") .simulate.comb(n, known, unknown, N, ...)
  else stop('Error: model must be one of \"degree\", \"barrier\", \"transmission\", or \"combined\"')
}  

nsum.mcmc <- function(dat, known, N, indices.k=(length(known)+1):(dim(dat)[2]), 
                      iterations=1000, burnin=100, size=iterations, 
                      model="degree", ...)
{
  cl <- match.call()
  model <- match.arg(model, c("degree", "barrier", "transmission", "combined"))
  structure(
    switch(
      model,
      degree = .mcmc.rd(dat, known, N, indices.k, iterations, burnin, size, ...),
      barrier = .mcmc.bar(dat, known, N, indices.k, iterations, burnin, size, ...),
      transmission = .mcmc.trans(dat, known, N, indices.k, iterations, burnin, size, ...),
      combined = .mcmc.comb(dat, known, N, indices.k, iterations, burnin, size, ...)
    ),
    call = cl,
    class = "NSUM"
  )
}
