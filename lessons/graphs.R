# Pre Requisites
{
  if (!require(pacman)) install.packages('pacman', repos = 'https://cran.rstudio.com')
  pacman::p_load(tidyverse, 
                 data.table, 
                 here, 
                 xaringan, 
                 rmarkdown,
                 kableExtra,
                 tinytex)
  tinytex::install_tinytex()
  remotes::install_github("mitchelloharawild/icons")
  here()
}

# Lesson 2 Graphs
{
  x = seq(0,20,.01)
  df = data.frame(x)
  
  ggplot(df, aes(x)) +
    stat_function(fun = function(x) ((x^2-6*x+14)/x), color = 'black', size = 1) +
    labs(x = "X", y = "f(x)") +
    xlim(0,20) + ylim(0,20) +
    coord_fixed() +
    theme(axis.title.y.left = element_text(angle = 0, vjust = 0.5),
          axis.title.y.right = element_text(angle = 0, vjust = 0.5),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color = "black"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal")
  
  ggsave(here('lessons/img/constraint.png'))
}

```{r pref, out.width = '80%', fig.align='center', warning=FALSE}
my_function <- function(x1, x2) {
  
  final_value = x1^(1/2) * x2^(1/2)
}


x1 <- seq(0, 10,0.01)
x2 <- seq(0, 10,0.01)


expand.grid(X1 = x1, X2 = x2) %>%
  mutate(Z = my_function(X1, X2)) %>%
  ggplot(aes(X1, X2, z = Z)) +
  geom_contour() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  coord_fixed() +
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  theme(axis.title.y.left = element_text(angle = 0, vjust = 0.5),
        axis.title.y.right = element_text(angle = 0, vjust = 0.5),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 20))
```



```{r bc, out.width = '80%', fig.align='center', warning=FALSE}
x = seq(0,20,.01)
df = data.frame(x)

ggplot(df, aes(x)) +
  stat_function(fun = function(x) 8-x, color = 'gold', size = 2) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab(bquote(X[1]))+
  ylab(bquote(X[2]))+
  xlim(0,10) + ylim(0,10) +
  coord_fixed()+
  theme(axis.title.y.left = element_text(angle = 0, vjust = 0.5),
        axis.title.y.right = element_text(angle = 0, vjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        text = element_text(size = 20))
```

