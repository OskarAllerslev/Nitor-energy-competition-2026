
training_data <- load_full_dataset()




p1 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market A"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 

p2 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market B"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 

p3 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market C"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 
p4 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market D"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 
p5 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market E"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 
p6 <- ggplot2::ggplot(
  data = training_data  |> dplyr::filter(market == "Market F"), 
  mapping = ggplot2::aes(x = id, y = target)
) +
  ggplot2::geom_line() 

library(patchwork)

(p1 + p2 + p3) / (p4 + p5 + p6)
