library(tidyverse)
library(Momocs)

f <- list.files("class_out_data_f23",pattern=".txt|.csv",full.names = TRUE)

#make a large df with vroom
out.df <- vroom::vroom(f, id = "class_out_data_f23")

#add wing info
out.df <- out.df %>% 
     mutate(wing=gsub("XY_.+_(hindwing|forewing)\\..+","\\1",basename(class_out_data_f23))) %>% 
     na.omit()

#make list
outs.l <- sapply(f,function(x) out.df %>% filter(class_out_data_f23==x) %>% select(X,Y) %>% as.matrix)

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

fore.min <- forewings %>% 
     coo_nb() %>% 
     min()

forewings <- forewings %>%
     coo_interpolate(fore.min) %>% 
     fgProcrustes() %>% 
     stack()

hind.min <- hindwings %>% 
     coo_nb() %>% 
     min()

hindwings %>% 
     coo_interpolate(hind.min) %>% 
     coo_slide(id=1) %>% 
     coo_align()  %>%
     fgProcrustes() %>%
     stack()

forewings %>%
     coo_interpolate(fore.min) %>% 
     coo_align() %>%
     fgProcrustes() %>% 
     efourier(norm=FALSE) 

hindwings %>% 
     coo_interpolate(hind.min) %>% 
     coo_align() %>%
     fgProcrustes() %>% 
     efourier(norm=FALSE) 

forewing.pca <- forewings %>%
     coo_interpolate(fore.min) %>%
     coo_align()  %>%
     coo_slide(id=1) %>% 
     fgProcrustes() %>% 
     efourier(norm=FALSE) %>% 
     PCA()

hindwing.pca <-hindwings %>% 
     coo_interpolate(hind.min) %>% 
     coo_align()  %>%
     coo_slide(id=1) %>% 
     fgProcrustes() %>% 
     efourier(norm=FALSE) %>% 
     PCA()

forewing.pca %>% 
     plot_PCA(title = "forewings")

hindwing.pca %>% 
     plot_PCA(title = "hindwings")

library(ape)

lep.tree <- ape::read.tree("lep_tree2.tre")

plot(lep.tree,cex=0.1)

lep.tree <- ladderize(lep.tree)
plot(lep.tree,cex=0.1)

lep.tree$tip.label <- gsub("_"," ",lep.tree$tip.label)

basename(names(outs))[1:5]

lep.sp <- read_csv("lep_image_data.csv")

head(lep.sp)

head(lep.sp$identifier)

out.data <- tibble(xy.file=basename(names(outs))) %>% 
     mutate(identifier=gsub("XY_|_hindwing|_forewing|.txt","",xy.file)) %>% 
     left_join(lep.sp)

head(out.data)

head(hindwing.pca$x,1)

head(forewing.pca$x,1)

hindwing.pca2 <-  tibble(xy.file=basename(rownames(hindwing.pca$x)),PC1=hindwing.pca$x[,1],PC2=hindwing.pca$x[,2]) %>%
     left_join(out.data)

forewing.pca2 <-  tibble(xy.file=basename(rownames(forewing.pca$x)),PC1=forewing.pca$x[,1],PC2=forewing.pca$x[,2]) %>%
     left_join(out.data)

#evolutionary rates
drops <- c(
     setdiff(unique(out.data$species),lep.tree$tip.label), #sp in tree not in data
     setdiff(lep.tree$tip.label,unique(out.data$species)) #sp in data, not in tree
)

lep.tree2 <- drop.tip(lep.tree,drops)

plot(lep.tree2,cex=0.1)

#PC1s
hind.pc1 <- hindwing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC1=mean(PC1)) %>% 
     pull

names(hind.pc1) <-  hindwing.pca2%>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC1=mean(PC1)) %>% 
     pull(species)

fore.pc1 <- forewing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC1=mean(PC1)) %>% 
     pull(PC1)

names(fore.pc1) <-  forewing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC1=mean(PC1)) %>% 
     pull(species)

#PC2s
hind.pc2 <- hindwing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC2=mean(PC2)) %>% 
     pull(PC2)

names(hind.pc2) <-  hindwing.pca2%>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>%
     summarize(PC2=mean(PC2)) %>% 
     pull(species)

fore.pc2 <- forewing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC2=mean(PC2)) %>% 
     pull(PC2)

names(fore.pc2) <-  forewing.pca2 %>% 
     filter(species%in% lep.tree2$tip.label) %>% 
     group_by(species) %>% 
     summarize(PC2=mean(PC2)) %>% 
     pull(species)

library(phytools)

forePC1.BM<-brownie.lite(lep.tree2,fore.pc1*10)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc1*10)

forePC2.BM<-brownie.lite(lep.tree2,fore.pc2*10)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc2*10)

forePC1.BM$sig2.single

#shape evolution correlation
hindPC1.pic <- pic(hind.pc1,phy = lep.tree2)
forePC1.pic <- pic(fore.pc1,phy = lep.tree2)

PC1.pic <- tibble(
     hind=hindPC1.pic,
     fore=forePC1.pic
)

PC1.pic %>% 
     ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")

summary(lm(hind~fore,PC1.pic))

