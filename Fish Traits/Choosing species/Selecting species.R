library(ggplot2)

fishcom<-read.csv(file="C:/Users/Chris/documents/Stream fish Metapopulations/Fish Traits/Choosing species/traits.csv", header=T)
names(fishcom)

hist(fishcom$Rouge_Total, breaks=12)

p.rouge <- ggplot(fishcom, aes(x = reorder(Species, -Rouge_Total), y = Rouge_Per)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.edon <- ggplot(fishcom, aes(x = reorder(Species, -EastDon_Total), y = EastDon_Per)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.mim <- ggplot(fishcom, aes(x = reorder(Species, -Mimico_Total), y = Mimico_Per)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.high <- ggplot(fishcom, aes(x = reorder(Species, -Highland_Total), y = Highland_Per)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.etob <- ggplot(fishcom, aes(x = reorder(Species, -Etob_Total), y = Etob_Per)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.wsheds <- ggplot(fishcom, aes(x = reorder(Species, -Wshed), y = Wshed)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

getwd()

p.AspRatio <- ggplot(fishcom, aes(x = reorder(Species, -AspRatio), y = AspRatio)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.MAXTL <- ggplot(fishcom, aes(x = reorder(Species, -MAXTL), y = MAXTL)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.TLASP <- ggplot(fishcom, aes(x = MAXTL, y = AspRatio)) +
    geom_point(aes(colour=Candidate, size=5))

p.fecun <- ggplot(fishcom, aes(x = reorder(Species, -FECUNDITY), y = log(FECUNDITY))) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.mat <- ggplot(fishcom, aes(x = reorder(Species, -MATUAGE), y = MATUAGE)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))

p.long <- ggplot(fishcom, aes(x = reorder(Species, -LONGEVITY), y = LONGEVITY)) +
  geom_bar(stat = "identity", aes(colour = Candidate, fill = Candidate)) +
  theme(axis.text.x = element_text(angle = 90))




