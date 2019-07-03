library(plotly)

m <- 10:100
N <- 774
Xm <- est <- rep(0, length(m))
IC_10 <- IC_05 <- IC_01 <- matrix(0, length(m), 2)

for (i in 1:length(m)) {
    
    Xm[i] <- max(round(runif(m[i], min = 1, max = N)))
    est[i] <- Xm[i] * ((m[i] + 1) / m[i]) - 1
    IC_10[i, 1] <- IC_05[i, 1] <- IC_01[i, 1] <- Xm[i]
    
    alpha <- 0.1
    IC_10[i, 2] <- Xm[i] * alpha^(-1 / m[i])
    
    alpha <- 0.05
    IC_05[i, 2] <- Xm[i] * alpha^(-1 / m[i])
    
    alpha <- 0.01
    IC_01[i, 2] <- Xm[i] * alpha^(-1 / m[i])

}

p <- plot_ly() %>%
  add_lines(x = m, y = est,
            color = I("blue"), name = "Est. puntual") %>%
  add_lines(x = m, y = Xm - 5,
            color = I("green"), name = "Máximo Xm") %>%
  add_ribbons(x = m, ymin = IC_10[, 1], ymax = IC_10[, 2],
              color = I("gray50"), name = "IC 90%") %>%
  add_ribbons(x = m, ymin = IC_05[, 1], ymax = IC_05[, 2],
              color = I("gray70"), name = "IC 95%") %>%
  add_ribbons(x = m, ymin = IC_01[, 1], ymax = IC_01[, 2],
              color = I("gray85"), name = "IC 90%") %>%
  add_lines(x = m, y = rep(N, length(m)), name = "Licencias reales",
            color = I("red"))