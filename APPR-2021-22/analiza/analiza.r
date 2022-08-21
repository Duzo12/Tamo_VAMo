# 4. faza: Analiza podatkov
library("data.table")   
library(cowplot)


graf1 <- ggplot(data = SkupnaTabela) +
  geom_bar(aes(x=reorder(Country, desc(Dohodek.2019)), y=Dohodek.2019), stat = "Identity", show.legend = F) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=8)) +
  xlab("Country") + ylab("Dohodek") + ggtitle("Višina dohodka po državah") +
  geom_hline(aes(yintercept = mean(Dohodek.2019)), size=1.5)

obdrzi_stolpce <- c("Country","Kriminal.2013", "Kriminal.2014", "Kriminal.2015", "Kriminal.2016", "Kriminal.2017", "Kriminal.2018", "Kriminal.2019")
Leto <- c(2013,2014,2015,2016,2017,2018,2019)

Slovenija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Slovenia")
Slovenija <- Slovenija[, -1]
Slovenija <- transpose(Slovenija)
colnames(Slovenija)[1] <- "Slovenia"
Slovenija <- cbind(Leto, Slovenija)

Avstrija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Austria")
Avstrija <- Avstrija[, -1]
Avstrija <- transpose(Avstrija)
colnames(Avstrija)[1] <- "Austria"
Avstrija <- cbind(Leto, Avstrija)

Italija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Italy")
Italija <- Italija[, -1]
Italija <- transpose(Italija)
colnames(Italija)[1] <- "Italy"
Italija <- cbind(Leto, Italija)

Nemèija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Germany")
Nemèija <- Nemèija[, -1]
Nemèija <- transpose(Nemèija)
colnames(Nemèija)[1] <- "Germany"
Nemèija <- cbind(Leto, Nemèija)

Francija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "France")
Francija <- Francija[, -1]
Francija <- transpose(Francija)
colnames(Francija)[1] <- "France"
Francija <- cbind(Leto, Francija)

Islandija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Iceland")
Islandija <- Islandija[, -1]
Islandija <- transpose(Islandija)
colnames(Islandija)[1] <- "Iceland"
Islandija <- cbind(Leto, Islandija)

Švedska <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Sweden")
Švedska <- Švedska[, -1]
Švedska <- transpose(Švedska)
colnames(Švedska)[1] <- "Sweden"
Švedska <- cbind(Leto, Švedska)

Grèija <- SkupnaTabela[, obdrzi_stolpce] %>% filter(Country == "Greece")
Grèija <- Grèija[, -1]
Grèija <- transpose(Grèija)
colnames(Grèija)[1] <- "Greece"
Grèija <- cbind(Leto, Grèija)

graf2 <- ggplot()+
  geom_line(data = Slovenija, aes(x=Leto, y=Slovenia, colour = "Slovenia"))+
  geom_line(data = Avstrija, aes(x=Leto, y=Austria, colour = "Austria"))+
  geom_line(data = Italija, aes(x=Leto, y=Italy, colour = "Italy"))+
  geom_line(data = Nemèija, aes(x=Leto, y=Germany, colour = "Germany"), size = 1.5)+
  geom_line(data = Francija, aes(x=Leto, y=France, colour = "France"))+
  geom_line(data = Islandija, aes(x=Leto, y=Iceland, colour = "Iceland"))+
  geom_line(data = Švedska, aes(x=Leto, y=Sweden, colour = "Sweden"), size = 1.5)+
  geom_line(data = Grèija, aes(x=Leto, y=Greece, colour = "Greece"))+
  scale_colour_manual("",
                      breaks = c("Slovenia", "Austria", "Italy", "Germany", "France", "Iceland", "Sweden", "Greece"),
                      values = c("Slovenia" = "blue", "Austria" = "red", "Italy" = "brown", "Germany" = "black", "France" = "green", "Iceland" = "gray", "Sweden" = "yellow", "Greece" = "orange"))+
  xlab("Leto") + ylab("Indeks kriminala") 


Slovenija2 <- Dohodek %>% filter(Country == "Slovenia")
Slovenija2 <- Slovenija2[,-1]
Slovenija2 <- transpose(Slovenija2)
colnames(Slovenija2)[1] <- "Slovenia_Dohodek"
Slovenija2 <- cbind(Leto, Slovenija2)

Slovenija3 <- Population %>% filter(Country == "Slovenia")
Slovenija3 <- Slovenija3[,-1]
Slovenija3 <- transpose(Slovenija3)
colnames(Slovenija3)[1] <- "Slovenia_Population"
Slovenija3 <- cbind(Leto, Slovenija3)

Slovenija4 <- Izobrazba %>% filter(Country == "Slovenia")
Slovenija4 <- Slovenija4[,-1]
Slovenija4 <- transpose(Slovenija4)
colnames(Slovenija4)[1] <- "Slovenia_Izobrazba"
Slovenija4 <- cbind(Leto, Slovenija4)

Švica2 <- Dohodek %>% filter(Country == "Switzerland")
Švica2 <- Švica2[,-1]
Švica2 <- transpose(Švica2)
colnames(Švica2)[1] <- "Switzerland_Dohodek"
Švica2 <- cbind(Leto, Švica2)

Švica3 <- Population %>% filter(Country == "Switzerland")
Švica3 <- Švica3[,-1]
Švica3 <- transpose(Švica3)
colnames(Švica3)[1] <- "Switzerland_Population"
Švica3 <- cbind(Leto, Švica3)

Švica4 <- Izobrazba %>% filter(Country == "Switzerland")
Švica4 <- Švica4[,-1]
Švica4 <- transpose(Švica4)
colnames(Švica4)[1] <- "Switzerland_Izobrazba"
Švica4 <- cbind(Leto, Švica4)


graf3 <- ggplot(data = Slovenija2, aes(x=Leto, y=Slovenia_Dohodek,colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+ theme(legend.position = "none") +
  ylab("Dohodek") 

graf4 <- ggplot(data = Slovenija3, aes(x=Leto, y=Slovenia_Population, colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point() + theme(legend.position = "none") +
  ylab("Populacija") 

graf5 <- ggplot(data = Slovenija4, aes(x=Leto, y=Slovenia_Izobrazba, colour = "Slovenia"), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+ theme(legend.position = "none") +
  ylab("Izobrazba") 


graf6 <- ggplot(data = Švica2, aes(x=Leto, y=Switzerland_Dohodek), colour = "black", stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Dohodek") 

graf7 <- ggplot(data = Švica3, aes(x=Leto, y=Switzerland_Population), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Populacija") 

graf8 <- ggplot(data = Švica4, aes(x=Leto, y=Switzerland_Izobrazba), stat = "Identity", show.legend = F) +
  geom_line() + geom_point()+
  ylab("Izobrazba") 

skupek1 <- plot_grid(graf3, graf4, graf5, graf6, graf7, graf8)


graf_DV_2013 <- ggplot(Tabela2013, aes(x=Dohodek.2013, y = Varnost.2013))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2013") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2014 <- ggplot(Tabela2014, aes(x=Dohodek.2014, y = Varnost.2014))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2014") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2015 <- ggplot(Tabela2015, aes(x=Dohodek.2015, y = Varnost.2015))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2015") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2016 <- ggplot(Tabela2016, aes(x=Dohodek.2016, y = Varnost.2016))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2016") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2017 <- ggplot(Tabela2017, aes(x=Dohodek.2017, y = Varnost.2017))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2017") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2018 <- ggplot(Tabela2018, aes(x=Dohodek.2018, y = Varnost.2018))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2018") + xlab("Višina plaèe") + ylab("Indeks varnosti")

graf_DV_2019 <- ggplot(Tabela2019, aes(x=Dohodek.2019, y = Varnost.2019))+
  geom_point() +
  geom_smooth(method = loess) +
  ggtitle("2019") + xlab("Višina plaèe") + ylab("Indeks varnosti")

skupek2 <- plot_grid(graf_DV_2013,graf_DV_2015,graf_DV_2017, graf_DV_2019)








