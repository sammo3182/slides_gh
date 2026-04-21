ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  ylab("Mileage per Gallon") +
  xlab("Weight")
