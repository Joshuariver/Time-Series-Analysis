# COVID-19: The Case of Germany
# https://www.r-bloggers.com/covid-19-the-case-of-germany/


library(deSolve)

# <a class="vglnk" href="https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Germany#Statistics" rel="nofollow"><span>https</span><span>://</span><span>en</span><span>.</span><span>wikipedia</span><span>.</span><span>org</span><span>/</span><span>wiki</span><span>/</span><span>2020</span><span>_</span><span>coronavirus</span><span>_</span><span>pandemic</span><span>_</span><span>in</span><span>_</span><span>Germany</span><span>#</span><span>Statistics</span></a>
Infected <- c(16, 18, 21, 26, 53, 66, 117, 150, 188, 240, 349, 534, 684, 847, 1112, 1460, 1884, 2369, 3062, 3795, 4838, 6012)
Day <- 1:(length(Infected))
N <- 83149300 # population of Germany acc. to Destatis

old <- par(mfrow = c(1, 2))
plot(Day, Infected, type ="b")
plot(Day, Infected, log = "y")
abline(lm(log10(Infected) ~ Day))
title("Total infections COVID-19 Germany", outer = TRUE, line = -2)

# SIR Model

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
##      beta     gamma 
## 0.6428120 0.3571881

t <- 1:80 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour

matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
## Warning in xy.coords(x, y, xlabel, ylabel, log = log): 1 y value <= 0
## omitted from logarithmic plot

points(Day, Infected)
legend("bottomright", c("Susceptibles", "Infecteds", "Recovereds"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model Covid-19 Germany", outer = TRUE, line = -2)



par(old)

R0 <- setNames(Opt_par["beta"] / Opt_par["gamma"], "R0")
R0
##       R0 
## 1.799646

fit[fit$I == max(fit$I), "I", drop = FALSE] # height of pandemic
##          I
## 54 9769398

max_infected <- max(fit$I)
max_infected / 5 # severe cases
## [1] 1953880

max_infected * 0.06 # cases with need for intensive care
## [1] 586163.9

# <a class="vglnk" href="https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/" rel="nofollow"><span>https</span><span>://</span><span>www</span><span>.</span><span>newscientist</span><span>.</span><span>com</span><span>/</span><span>article</span><span>/</span><span>mg24532733</span><span>-</span><span>700</span><span>-</span><span>why</span><span>-</span><span>is</span><span>-</span><span>it</span><span>-</span><span>so</span><span>-</span><span>hard</span><span>-</span><span>to</span><span>-</span><span>calculate</span><span>-</span><span>how</span><span>-</span><span>many</span><span>-</span><span>people</span><span>-</span><span>will</span><span>-</span><span>die</span><span>-</span><span>from</span><span>-</span><span>covid</span><span>-</span><span>19</span><span>/</span></a>
max_infected * 0.007 # deaths with supposed 0.7% fatality rate
## [1] 68385.78