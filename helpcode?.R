drops <- lep.tree$tip.label[!lep.tree$tip.label%in%unique(out.data$species)]
lep.tree2 <- drop.tip(lep.tree,drops)
plot(lep.tree2,cex=0.1)
#PC1s
hind.pc1 <- hindwing.pca2 %>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC1=mean(PC1)) %>%
  pull
names(hind.pc1) <- hindwing.pca2%>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC1=mean(PC1)) %>%
  pull(species)
fore.pc1 <- forewing.pca2 %>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC1=mean(PC1)) %>%
  pull(PC1)
names(fore.pc1) <- forewing.pca2 %>%
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
names(hind.pc2) <- hindwing.pca2%>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>%
  pull(species)
fore.pc2 <- forewing.pca2 %>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>%
  pull(PC2)
names(fore.pc2) <- forewing.pca2 %>%
  filter(species%in% lep.tree2$tip.label) %>%
  group_by(species) %>%
  summarize(PC2=mean(PC2)) %>%
  pull(species)
forePC1.BM<-brownie.lite(lep.tree2,fore.pc110)
hindPC1.BM<-brownie.lite(lep.tree2,hind.pc110)
forePC2.BM<-brownie.lite(lep.tree2,fore.pc210)
hindPC2.BM<-brownie.lite(lep.tree2,hind.pc210)
forePC1.BM$sig2.single

hindPC1.pic <- pic(hind.pc1,phy = lep.tree2)
forePC1.pic <- pic(fore.pc1,phy = lep.tree2)
PC1.pic <- tibble(
  hind=hindPC1.pic,
  fore=forePC1.pic
)

PC1.pic %>%
  ggplot(aes(x=fore,y=hind))+geom_point()+geom_smooth(method="lm")
summary(lm(hind~fore,PC1.pic))