# R code for the probability table above
data <- read.delim( 'http://kjc-fs-cluster.kjc.uni-heidelberg.de/dcs/data/syllables/syllables.dat', sep=";", header=TRUE, comment.char="#", stringsAsFactors=FALSE);
sums <-
  d <- data[order(data$Syllable),];
sums <- colSums( data[,2:6] );
groups <- data.frame(rbind(
  c("Gutturals", "k|g|G"),
  c("Palatals",  "c|j|J"),
  c("Retroflexes", "T|D|N"),
  c("Dentals",    "t|d|n"),
  c("Labials",    "p|b|m")
), stringsAsFactors=FALSE)

Y <- data.frame();
for(i in 1:nrow(groups))
{
  g <- grep(groups[i,2], data$Syllable, ignore.case=FALSE)
  sub <- data[g,]
  sub.sums <- colSums(sub[,2:6])
  Y <- rbind( Y,
              cbind(  data.frame( group=c( groups$X1[i] ) ), data.frame( t( sub.sums / sums ) ) )
  );
};
Y