# Read file
data <- read.delim( 'http://kjc-fs-cluster.kjc.uni-heidelberg.de/dcs/data/syllables/syllables.dat', sep=";", header=TRUE, comment.char="#", stringsAsFactors=FALSE);
# Groups - ignore the bad data for now
#groups <- data.frame(rbind(
#	c("Gutturals", "k|g|G"),
#	c("Palatals",  "c|j|J"),
#	c("Retroflexes", "T|D|N"),
#	c("Dentals",    "t|d|n"),
#	c("Labials",    "p|b|m")
#), stringsAsFactors=FALSE);

# В правильной последовательности выводятся
groups <- data.frame(rbind(
  c("Лабиальные",    "p|b|m"),
  c("Дентальные",    "t|d|n"),
  c("Церебральные", "T|D|N"), 
  c("Палатальные",  "c|j|J"), 
  c("Гуттуральные", "k|g|G")
), stringsAsFactors=FALSE);

# Null matrix of pvalues
matpvals <- matrix( numeric(), 0, 5 );
# The total number of syllables for each period
ns <- as.vector( colSums( data[,2:6] ) );
for(i in 1:nrow(groups))
{
	# Filter rows with this group
	g <- grep(groups[i,2], data$Syllable, ignore.case=FALSE);
	sub <- data[g,];
	# The number of syllables containing syllable of this group
	xs <- as.vector( colSums( sub[,2:6] ) );
	# Combined test across all periods
	pval1 <- prop.test( xs, ns )$`p.value`;
	# Test across Early and Epic periods
	pval2 <- prop.test( xs[1:2], ns[1:2] )$`p.value`;
	# Test across Epic and Classical periods
	pval3 <- prop.test( xs[2:3], ns[2:3] )$`p.value`;
	# Test across Classical and Medieval periods
	pval4 <- prop.test( xs[3:4], ns[3:4] )$`p.value`;
	# Test across and Late periods
	pval5 <- prop.test( xs[4:5], ns[4:5] )$`p.value`;
	# Combine test results into one vector
	pvalues <- c( pval1, pval2, pval3, pval4, pval5 );
	# Add row for this group to the matrix
	matpvals <- rbind( matpvals, pvalues );
};
# Matrix to data frame
matpvals <- data.frame( matpvals, row.names=groups$X1 );
# Assign row names
# names( matpvals ) <- c( 'All periods', 'Early & Epic', 'Epic & Classical', 'Classical & Medieval', 'Medieval & Late' );

# Русские названия эпох
#names( matpvals ) <- c( 'Все эпохы санскрита', 'Веды и эпический', 'Эпический и классический', 'Классический и средневековий', 'Средневековий и поздний' );
names( matpvals ) <- c( 'I', 'II', 'III', 'IV', 'V' );
# Display matrix
matpvals

# Gasuns plot
data_matrix <- data.matrix(matpvals)
data_heatmap <- heatmap(data_matrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
# http://seqanswers.com/forums/showthread.php?t=12022
data_heatmap <- heatmap(data_matrix, Rowv=NA, Colv=NA, col = colorRampPalette(c("white","grey","red"))(256), scale="column", margins=c(5,10))
# Different aprroach
# heatmap.2(as.matrix(data_matrix),col =
#            colorRampPalette(c("white","green","green4","violet","purple"))(100))
