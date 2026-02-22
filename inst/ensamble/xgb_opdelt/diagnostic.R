library(ggplot2)
library(patchwork)

plot_predictions <- function(preds, test_data, market_name) {
  plot_data <- preds |> extract_fit_subset(test_data)

  ggplot(plot_data, aes(x = id)) +
    geom_line(aes(y = target.x, color = "Actual"), alpha = 0.7) +
    geom_line(aes(y = target.y, color = "Predicted"), alpha = 0.7) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    labs(title = market_name, y = "Target", x = "ID", color = "Legend") +
    theme_minimal()
}

p1 <- plot_predictions(predictions_A, testing_split, "Market A")
p2 <- plot_predictions(predictions_B, testing_split, "Market B")
p3 <- plot_predictions(predictions_C, testing_split, "Market C")
p4 <- plot_predictions(predictions_D, testing_split, "Market D")
p5 <- plot_predictions(predictions_E, testing_split, "Market E")
p6 <- plot_predictions(predictions_F, testing_split, "Market F")

(p1 + p2 + p3) / (p4 + p5 + p6) + plot_layout(guides = "collect")
