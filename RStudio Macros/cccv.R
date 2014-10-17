# Read file

data <- read.delim( 'http://kjc-fs-cluster.kjc.uni-heidelberg.de/dcs/data/syllables/syllables.dat', sep=";", header=TRUE, comment.char="#", stringsAsFactors=FALSE);

# Delete Avagrahas

data$Syllable <- gsub( "'", "", data$Syllable );

# There are some rows with bad syllable data (< 0.02%) - retain only rows with clean data

data <- data[ !grepl( '[^athAHsnevidmyMkuroSpzUjIbgcNDlTRGJL]', data$Syllable ),];

# Map to consonants and vowels

data$Syllable2 <- data$Syllable;

data$Syllable2 <- gsub( "[kgcjTDpbtd]h", "C", data$Syllable2 );

data$Syllable2 <- gsub( "[kgcjTDpbBtdDGhJlmnNrsSvVyz]", "C", data$Syllable2 );

data$Syllable2 <- gsub( "[aA][iIuU]", "V", data$Syllable2 );

data$Syllable2 <- gsub( "[aAiIuURLeo]", "V", data$Syllable2 );

# Count of triple conjuncts

sum( data[grep( "[C]{3}", data$Syllable2 ),]$total )

# Count of total syllables

sum( data$total )

# % of syllables with triple conjuncts

sum( data[grep( "[C]{3}", data$Syllable2 ),]$total ) / sum( data$total )
