
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)

# 1. 정규분포와 t-분포 -------------------
## 1.1. Function to fit Model ------------
fn_InputData <- function(pmean, psd, df1, p, p_tail){
  
  
  sstat <- data.frame(pmean = pmean, psd = psd)
  
  xmin <- -5
  xmax <- 5
  norm_xlim <- c(xmin, xmax)
  
  tr <- rt(n = 10000, df = df1)
  t_xlim <- c(min(tr), max(tr))
  if(max(abs(t_xlim)) > 15) t_xlim <- c(-15, 15)
  rm(tr)
  
  z_q_out <- switch(EXPR = p_tail,
                    lower = qnorm(p = p, mean = pmean, sd = psd, lower.tail = TRUE),
                    upper = qnorm(p = p, mean = pmean, sd = psd, lower.tail = FALSE),
                    both = c(qnorm(p = p/2, mean = pmean, sd = psd, lower.tail = TRUE),
                             qnorm(p = p/2, mean = pmean, sd = psd, lower.tail = FALSE)))
  
  zstat <- c(p = p,  q_out = round(z_q_out, 2))
  
  tail <- c(p_tail = p_tail)
  
  t_q_out <- switch(EXPR = p_tail,
                    lower = qt(p = p, df = df1, lower.tail = TRUE),
                    upper = qt(p = p, df = df1, lower.tail = FALSE),
                    both  = c(qt(p = p/2, df = df1, lower.tail = TRUE),
                              qt(p = p/2, df = df1, lower.tail = FALSE)))
  
  tstat <- c(df1 = df1, p = p, q_out = round(t_q_out, 2))
  
  if(p_tail == 'lower' | p_tail == 'upper'){
    q_out_txt <- paste0('  p = ', zstat['p'], '; q = ', zstat['q_out'] )
    xpos1 <- zstat['q_out']
  } else {
    q_out_txt <- paste0('  p = ', zstat['p'],
                        ';  q = ', round(z_q_out[1], 2), ', ', round(z_q_out[2], 2) )
    xpos1 <- zstat['q_out1']
  }
  
  
  qText = q_out_txt
  annotateText <- c('pText')
  z_annotateDF <- data.frame(
    xpos = c(xpos1),
    ypos =  c(Inf),
    annotateText = c(qText),
    hjustvar = c(0) ,
    vjustvar = c(2)) #<- adjust
  
  
  if(p_tail == 'lower' | p_tail == 'upper'){
    q_out_txt <- paste0('  p = ', tstat['p'], '; q = ', tstat['q_out'] )
    xpos1 <- tstat['q_out']
  } else {
    q_out_txt <- paste0('  p = ', tstat['p'],
                        ';  q = ', round(t_q_out[1], 2), ', ', round(t_q_out[2], 2) )
    xpos1 <- tstat['q_out1']
  }
  
  
  qText = q_out_txt
  annotateText <- c('pText')
  t_annotateDF <- data.frame(
    xpos = c(xpos1),
    ypos =  c(Inf),
    annotateText = c(qText),
    hjustvar = c(0) ,
    vjustvar = c(2)) #<- adjust
  
  dTitle <- paste0( ' 표준 정규분포 ', round(pmean,2), ', SD = ', round(psd,2) )
  
  tTitle <- paste0( " t-분포 ", ', df = ', df1 )
  
  txtTitle <- c(dTitle = dTitle, tTitle = tTitle)
  
  out <- list(sstat = sstat, tail = tail,
              zstat = zstat, tstat = tstat,
              norm_xlim = norm_xlim, t_xlim = t_xlim,
              z_annotateDF = z_annotateDF,
              t_annotateDF = t_annotateDF,
              txtTitle = txtTitle)
  
  
  return(out)
  
  
}



## 1.2. Density plot: Combined Normal & t distribution --------------


fn_dnorm_dt <- function(inputData){
  
  
  list2env(inputData, envir = environment())
  rm(inputData)
  
  pmean <- sstat$pmean[1]
  psd <- sstat$psd[1]
  
  df1 <- unname(tstat['df1'])
  
  
  dTitle <- bquote( ' 표준 정규분포: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) ~ '; ' ~
                      " t-분포 : df = " ~ .(df1))
  
  
  g <- ggplot(data = NULL, mapping = aes(norm_xlim))
  
  g <- g + geom_area(stat = 'function', fun = dnorm,
                     args = list(mean = pmean, sd = psd),
                     xlim = norm_xlim, fill = '#F8766D', alpha = 0.3)
  
  g <- g + geom_area(stat = 'function', fun = dt,
                     args = list(df = df1),
                     xlim = norm_xlim, fill = '#00BFC4', alpha = 0.3)
  
  
  g <- g + geom_vline(xintercept = 0, size = 1, linetype = 2, colour = 'darkred')
  
  
  g <- g + labs(title = dTitle, x = 'X', y = 'P(X)')
  
  
  aDF <- data.frame(xpos = 0, ypos = Inf,
                    txt = c(' 표준 정규분포', " t-분포"),
                    hjustvar = c(0, 0), vjustvar = c(2, 4))
  
  g <- g + geom_text(data = aDF,
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = txt),
                     colour = c('#F8766D','#00BFC4'), size = 6)
  
  
  xscale <- seq(from = t_xlim[1], to = t_xlim[2], length.out = 15)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale)
  
  
  g <- g + theme_bw()
  
  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
  
  
  print(g)
  
}



# 2. Normal distribution: Density plot -----------------------------

fn_dnorm <- function(inputData){
  
  
  list2env(inputData, envir = environment())
  rm(inputData)
  
  pmean <- sstat$pmean[1]
  psd <- sstat$psd[1]
  
  p_tail <- unname(tail['p_tail'])
  p <- unname(zstat['p'])
  
  annotateDF <- z_annotateDF
  
  if(p_tail == 'both'){
    q_out <- unname(c(zstat['q_out1'], zstat['q_out2']))
  } else {
    q_out <- unname(zstat['q_out'])
  }
  
  dTitle <- bquote( ' 표준 정규분포: ' ~
                      mu == .(pmean) ~ ', ' ~
                      sigma == .(psd) )
  
  
  g <- ggplot(data = NULL, mapping = aes(norm_xlim))
  
  
  if(p_tail == 'lower'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }
  
  
  if(p_tail == 'upper'){
    norm_xlim1 <- c(norm_xlim[1], q_out)
    norm_xlim2 <- c(q_out, norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }
  
  
  if(p_tail == 'both'){
    norm_xlim1 <- c(norm_xlim[1], q_out[1])
    norm_xlim2 <- c(q_out[1], q_out[2])
    norm_xlim3 <- c(q_out[2], norm_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dnorm,
                       args = list(mean = pmean, sd = psd), colour = 'darkred',
                       xlim = norm_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }
  
  
  g <- g + geom_vline(xintercept = pmean, size = 1, linetype = 1, colour = 'blue')
  
  g <- g + labs(title = dTitle, x = 'z', y = 'Density')
  
  
  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)
  
  
  xscale <- seq(from = norm_xlim[1], to = norm_xlim[2], by = 2)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale, limits = norm_xlim)
  
  
  g <- g + theme_bw()
  
  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))
  
  
  print(g)
  
}

## 2. Standard t Density: Plot1 with Type 1 error ---------------------


fn_dt <- function(inputData){
  
  
  list2env(inputData, envir = environment())
  rm(inputData)
  
  
  annotateDF <- t_annotateDF
  
  
  p_tail <- unname(tail['p_tail'])
  p <- unname(tstat['p'])
  
  if(p_tail == 'both'){
    q_out <- unname(c(tstat['q_out1'], tstat['q_out2']))
  } else {
    q_out <- unname(tstat['q_out'])
  }
  
  
  df1 <- unname(tstat['df1'])
  
  tTitle <- unname(txtTitle['tTitle'])
  
  g <- ggplot(data = NULL, mapping = aes(t_xlim))
  
  if(p_tail == 'lower'){
    t_xlim1 <- c(t_xlim[1], q_out)
    t_xlim2 <- c(q_out, t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }
  
  
  if(p_tail == 'upper'){
    t_xlim1 <- c(t_xlim[1], q_out)
    t_xlim2 <- c(q_out, t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out, size = 1, linetype = 2, colour = 'orange')
  }
  
  
  if(p_tail == 'both'){
    t_xlim1 <- c(t_xlim[1], q_out[1])
    t_xlim2 <- c(q_out[1], q_out[2])
    t_xlim3 <- c(q_out[2], t_xlim[2])
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim1, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim2, fill = '#ffff00', alpha = 0.7)
    g <- g + geom_area(stat = 'function', fun = dt,
                       args = list(df = df1), colour = 'darkred',
                       xlim = t_xlim3, fill = '#ff0000', alpha = 0.5)
    g <- g + geom_vline(xintercept = q_out[1], size = 1, linetype = 2, colour = 'orange')
    g <- g + geom_vline(xintercept = q_out[2], size = 1, linetype = 2, colour = 'orange')
  }
  
  
  g <- g + geom_vline(xintercept = 0, size = 1, linetype = 1, colour = 'blue')
  
  g <- g + labs(title = tTitle, x = 't', y = 'Density')
  
  
  g <- g + geom_text(data = annotateDF[1,],
                     aes(x = xpos, y = ypos,
                         hjust = hjustvar, vjust = vjustvar,
                         label = annotateText),
                     colour = c('blue'), size = 4)
  
  
  xscale <- seq(from = t_xlim[1], to = t_xlim[2], by = 1)
  xscale <- round(xscale, digits = 1)
  g <- g + scale_x_continuous(breaks = xscale, limits = t_xlim)
  
  
  g <- g + theme_bw()
  
  g <- g + theme(axis.text.x = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 0),
                 axis.text.y = element_text(face = 'plain', color = 'blue',
                                            size = 14, angle = 90, vjust = 0.5),
                 axis.title.x = element_text(size = 16, colour = 'purple'),
                 axis.title.y = element_text(size = 16, colour = 'purple'),
                 title = element_text(face = 'plain', color = 'blue',
                                      size = 16, angle = 0))
  
  
  print(g)
  
  
}

#_________________________________________________________________________________________



