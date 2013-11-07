---
layout: default
title: Turkopticon
ghurl: https://github.com/leeper/MTurkR/blob/gh-pages
---

# Turkopticon #

While MTurk provides a number of features for requesters to manage workers (statistics, qualifications, blocks, etc.), the worker-facing application offers little to help workers decide for whom to work. This can be good for new requesters, so that workers do not unfairly avoid their tasks, but it can also be exploitative for at least two reasons. First, without a way to track requesters, workers can be persistently burned by requesters who unfairly reject their work, misrepresent the amount of effort required to complete a HIT, or simply pay too little. Second, requesters are able to change their display name easily, so that workers can be misguided into believing that a requester is new when in fact they have simply changed their worker-facing identity.

In response, researchers [Lilly Irani](https://quote.ucsd.edu/lirani/) and [M. Six Silberman](http://wtf.tw/) developed [Turkopticon](http://turkopticon.differenceengines.com/), a review website - with associated Firefox and Chrome plugins - that allows workers to review requesters and makes those reviews directly visible inside the MTurk worker-facing website. Unlike worker fora like [mturk forum](http://mturkforum.com/) or [TurkerNation](http://www.turkernation.com/), Turkopticon provides a highly visible mechanism for workers to communicate with one another (and with requesters) about the quality of HITs and requesters in general.

I highly recommend that requesters regularly visit Turkopticon when they have active HITs to evaluate how their work is being received by the worker community. Turkopticon allows requesters to comment on reviews, providing requesters with a mechanism for correcting mistakes and using MTurk in an accountable and responsible fashion.

## Seeing reviews ##

Recently, Turkopticon added a simple API to retrieve average evaluations for one or more requesters on the four Turkopticon evaluation criteria: fair, fast, pay, and communication (each is scored on a five-point scale). These details can be retrieved in your browser by visiting:

```
http://turkopticon.differenceengines.com/aves/WorkerId
```

where `WorkerId` is an MTurk WorkerId (which is the same as a RequesterId). You can also see all review details by visiting:

```
http://turkopticon.differenceengines.com/WorkerId
```

Obtaining your Turkopticon reviews in R is similarly easy, but relies on parsing the JSON response from the API rather than parsing the HTML pages just described. Here's some code to access requester ratings:

```R
turkopticon <- function(id){
    if(is.null(id))
        stop('Must supply id or vector of ids as character strings')
    require(rjson)
    if(length(id)==1){
        url <- paste('http://api.turkopticon-devel.differenceengines.com/attrs.php?id',id,sep='=')
        r <- readLines(url, warn=FALSE)
        j <- list(fromJSON(r))
        names(j) <- j[[1]]$id
    }
    else if(length(id)>1){
        ids <- paste(id,collapse=',')
        url <- paste('http://api.turkopticon-devel.differenceengines.com/multi-attrs.php?ids',ids,sep='=')
        r <- readLines(url, warn=FALSE)
        j <- fromJSON(r)
    }
    class(j) <- 'turkopticonreview'
    return(j) # response as list
}

# Here's a function to nicely print the results to the console
print.turkopticonreview <- function(x,...){
    cat('## Turkopticon Requester Reviews ##\n\n')
    for(i in 1:length(x)){
        names(x[[i]]$attrs) <- gsub('pay',' pay',names(x[[i]]$attrs))
        out <-
        c(paste(names(x)[i],': ', x[[i]]$name,'\n', sep=''),
          paste('Reviews:   ', x[[i]]$reviews,'\n', sep=''),
          paste('ToS Flags: ', x[[i]]$tos_flags,'\n', sep=''),
          #paste('Ratings\n', sep=''),
          paste(names(x[[i]]$attrs), x[[i]]$attrs, collapse='\n', sep=':      '))
        cat(out,'\n\n',sep='')
    }
}

# Here's an example for an arbitrary requester:
turkopticon('A35829O3SICVR1')
# Here's an example with two requester ids:
turkopticon(c('A35829O3SICVR1','A1WYUGIFUHQZW7'))
```
