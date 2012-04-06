
find.time.var <- function
### Determine which variable should be used for animation.
(p
### ggplot object.
 ){
  for(i in seq_along(p$layers)){
    L <- p$layers[[i]]
    expr <- L$mapping$time
    if(!is.null(expr)){
      values <- eval(expr,L$data)
      return(list(variable=expr,values=sort(unique(values))))
    }
  }
  NULL
### An expression to be used for a time variable, or NULL if none
### found.
}

gganim <- structure(function
### Convert a ggplot to an animation.
(anim
### ggplot containing time aesthetic. Individual values of the time
### aesthetic will be used for each frame of the animation.
 ){
  time.var <- find.time.var(anim)
  df.list <- lapply(anim$layers,function(L){
    tryCatch({
      split.vals <- eval(time.var$variable,L$data)
      split(L$data,split.vals)
    },error=function(e){
      L$data ## if the time variable is not here, then return all data
    })
  })
  ## print a frame for every value of the time variable
  for(time.value.i in seq_along(time.var$values)){
    time.value <- time.var$values[time.value.i]
    subplot <- anim
    for(layer.i in seq_along(df.list)){
      list.element <- df.list[[layer.i]]
      subplot$layers[[layer.i]]$data <- if(is.data.frame(list.element)){
        list.element
      }else{
        list.element[[time.value.i]]
      }
    }
    print(subplot)
  }
},ex=function(){
  library(nicholsonppp)
  a <- sim.drift.selection(s=0.1)
  dimnames(a$simulated.freqs) <-
    list("locus"=NULL,"population"=NULL,"generation"=NULL)
  library(reshape2)
  freq <- melt(a$sim)


  sorted.s <- a$s[with(a$s,order(S,ancestral)),]
  sorted.s$sorted.id <- 1:nrow(sorted.s)
  sorted.s <- sorted.s[order(sorted.s$locus),]

  loc.pop.colors <- as.matrix(sorted.s[,grepl("color",names(sorted.s))])
  dimnames(loc.pop.colors) <- list(locus=NULL,population=NULL)
  colors.df <- melt(loc.pop.colors)

  ## quick merge and check
  test <- data.frame(freq,colors.df)
  with(test,sum(locus!=locus.1))
  with(test,sum(population!=population.1))
  merged <- with(test,{
    data.frame(locus,population,generation,value,
               color=value.1,
               sorted.id=sorted.s$sorted.id)
  })

  ## freq vs time plot
  l.id <- subset(a$s,type=="positive")$locus[1]
  l1 <- subset(merged,locus==l.id)
  color.key <- c(blue="blue",red="red",neutral="turquoise")
  library(ggplot2)
  ggplot(l1,aes(generation,value))+
    geom_line(aes(group=population,colour=color))+
    scale_colour_manual(values=color.key)+
    opts(title=with(a$s[l.id,],sprintf("locus %d s=%f selection=%s",
               l.id,s,type)))

  ## freq dotplot for 1 generation
  g1 <- subset(merged,generation==50)
  ggplot(g1,aes(sorted.id,value))+
    geom_point(aes(y=ancestral),data=sorted.s,pch=21)+
    geom_point(aes(colour=color))+
    scale_colour_manual(values=color.key)+
    ylim(0,1)+
    ylab("blue allele frequency")

  ## animation
  anim <- ggplot(,aes(sorted.id))+
    geom_point(aes(y=ancestral),data=sorted.s,pch=21)+
    geom_point(aes(y=value,colour=color,time=generation),data=merged)+
    scale_colour_manual(values=color.key)+
    ylim(0,1)+
    ylab("blue allele frequency")
  
  print(anim) ## this ignores the time aesthetic
  library(animation)
  saveHTML({
    gganim(anim)
  })
})
