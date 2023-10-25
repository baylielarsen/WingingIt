
library(tidyverse)
library(Momocs)

f <- list.files("lep_examples", pattern=".txt|.csv", full.names=TRUE)

out <- read_delim(f[1],"\t") %>%
     as.matrix()

out %>% 
     list() %>% 
     Out() %>% 
     coo_flipx() %>% 
     stack()

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename",progress=F)

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

outs.l %>% 
     Out() %>% 
     stack()

#make a large df with vroom
out.df <- vroom::vroom(f, id = "filename")

#add wing info
out.df <- out.df %>% 
     mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(filename))) %>% 
     na.omit()

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(filename==x) %>% select(X,Y) %>% as.matrix)

#extract wing info
wings <- gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(names(outs.l)))

outs <-  outs.l %>% 
     Out(fac=list(wing=wings)) %>% 
     coo_flipx()

forewings <- outs %>% 
     filter(wing=="forewing")

hindwings <- outs %>% 
     filter(wing=="hindwing")

forewings %>% 
     stack()

hindwings %>% 
     stack()
