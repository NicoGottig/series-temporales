p = 4
q = 4
d = 1
P = 0
D = 0 
Q = 0

regx = NULL
x <- df.data$lamb

aic_matrix <- matrix(nrow = p+1, ncol = q+1)

if (is.null(regx)) {
  for (p in 0:p) {
    for (q in 0:q) {
      aic = arima(x, order = c(p, d, q), seasonal = c(P, D, Q))$aic
      aic_matrix[p+1,q+1] = aic
    }
  }
} else{
  for (p in 0:p) {
    for (q in 0:q) {
      aic = arima(x, order = c(p, d, q), seasonal = c(P, D, Q), xreg = regx)$aic
      aic_matrix[p+1,q+1] = aic
    }
  }
}

colnames(aic_matrix) <- paste0("MA", seq(0:q)-1)
rownames(aic_matrix) <- paste0("AR", seq(0,p))

aic_df <- data.frame(aic_matrix)
aic_df$AR <- rownames(aic_matrix)

aic_pivot <- aic_df %>% 
  pivot_longer(-AR, names_to = "MA", values_to = "AIC")

aic_pivot$AR <- factor(aic_pivot$AR, levels =  paste0("AR", rev(seq(p:0))-1), ordered = T)
aic_pivot$MA <- factor(aic_pivot$MA, levels = paste0("MA", seq(0:q)-1), ordered = T)

plt <- ggplot(aic_pivot, aes(x = MA, y = AR, fill = AIC, label = round(AIC, 2))) +
  geom_tile(alpha=1) +
  geom_text(color="Black") +
  scale_fill_gradient(low = "white", high="#4c7b8f") +
  scale_x_discrete(position="top",expand=c(0,0)) +
  # coord_fixed() +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 10)) +
  xlab("") +
  ylab("") +
  labs(title = paste0("ARIMA(", p, ",", d, ",", q, ")(", P, ",", D, ",", Q, ")"))
plt
