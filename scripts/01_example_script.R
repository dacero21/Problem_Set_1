
# Clean the workspace -----------------------------------------------------
rm(list=ls())
library(ggplot2)


df <- data.frame(
  x = c(1:10)
)

saveRDS(df,file = 'stores/df.rds')


plot <- ggplot(df)

ggsave('views/img.png')


