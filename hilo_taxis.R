
library(plotly)

# Probability of Xm = N
N <- 774
M <- 1:N

fig <- plot_ly()
fig <- fig %>% add_trace(x = M, y = M / N) %>%
  layout(title = "Probabilidad de observar la licencia más alta (N)",
         xaxis = list(title = "Taxis observados"),
         yaxis = list(title = "Probabilidad"))
fig


# Behaviour of (m + 1) / m
M <- seq(1, 1e2, l = 1e3)

fig2 <- plot_ly()
fig2 <- fig2 %>% add_trace(x = M, y = (M + 1)/M) %>%
  layout(title = "Gráfica de ALGO = (m + 1)/m",
         xaxis = list(title = "m"),
         yaxis = list(title = "ALGO = (m + 1)/m"))
fig2

# Point estimation
M <- 1:50 # Number of observed taxis
N <- 774 # Total number of taxis
MC <- 1e2 # number of simulations
Xm <- est <- matrix(0, MC, length(M))

p <- plot_ly() 
for (k in 1:MC) {
  for (i in 1:length(M)) {
    Xm[k, i] <- max(round(runif(M[i], min = 1, max = N)))
    est[k, i] <- Xm[k, i] * ((M[i] + 1) / M[i]) - 1
  }  
  
  p <- p %>%
    add_trace(x = M, y = est[k, ], type = "scatter", mode = "lines",
              line = list(color = "rgba(50, 133, 246, 0.4)", width = 0.7,
                          dash = "dash"),
              showlegend = FALSE) %>%
    add_trace(x = M, y = Xm[k, ], type = "scatter", mode = "lines",
              line = list(color = "rgba(243, 136, 14, 0.4)", width = 0.7,
                          dash = "dash"),
              showlegend = FALSE)
  
}

p <- p %>%
  add_trace(x = M, y = colSums(est) / MC, type = "scatter", mode = "lines",
            name = "Estimación media",
            line = list(color = "rgba(50, 133, 246, 1)", width = 3)) %>%
  add_trace(x = M, y = colSums(Xm) / MC, type = "scatter", mode = "lines",
            name = "Licencia más alta media",
            line = list(color = "rgba(243, 136, 14, 1)", width = 3)) %>%
  add_trace(x = M, y = rep(N, length(M)), type = "scatter", mode = "lines",
            name = "Licencias reales",
            line = list(color = "rgba(247, 53, 5, 0.8)", width = 4)) %>%
  layout(xaxis = list(title = "Taxis observados", range = c(1, 50)),
         yaxis = list(title = "Estimaciones", range = c(300, 1200)))


