# Ryan Hubert
# October 25, 2016


# Set your working directory
setwd("~/Dropbox/GitHub/FL23-conspiracy-replication/")


# Import data
# Raw data available from: 
#   http://dos.myflorida.com/elections/data-statistics/elections-data/precinct-level-election-results/
bro <- read.table("BRO_PctResults20160830.txt", sep="\t") # Broward County Precincts
dad <- read.table("DAD_PctResults20160830.txt", sep="\t") # Miami-Dade County Precincts


# This fixes an error caused by precincts being treated as factors on import
bro[,6] <- as.character(bro[,6])
dad[,6] <- as.character(dad[,6])


# Merge into single dataframe for CD23
cd23 <- rbind(bro,dad)


# Code variable names using the key:
#   http://dos.myflorida.com/media/694099/precinct-level-results-data-definition-field-codes.pdf
cols <- c("county",
          "county_full",
          "election_no",
          "election_date",
          "election_name",
          "precinct_id",
          "precinct_poll_loc",
          "precinct_RV",
          "precinct_RV_R",
          "precinct_RV_D",
          "precinct_RV_O",
          "contest_name",
          "contest_district",
          "contest_code",
          "cand_name",
          "cand_party",
          "cand_id",
          "doe_code",
          "vote")
colnames(cd23) <- cols


# Which race is DWS versus Tim Canova?
race <- unique(cd23$contest_code[grep("Wasserman",cd23$cand_name)])
if(length(race)==1){
  race <- race[1]
}else{
  print("Problem!")
  }


# Subset to the Dem primary
cd23 <- cd23[cd23$contest_code==race,]


# Drop unused variables
cd23 <- cd23[,c("precinct_id","precinct_RV","contest_code","cand_name","vote")]


# Sort by precint RV count
cd23 <- cd23[order(cd23$precinct_RV),]


# Create DWS-only and TC-only datasets
dws = cd23[grep("Wasserman",cd23$cand_name),]
tc = cd23[grep("Canova",cd23$cand_name),]


# Check that we have an obs for each candidate in each precinct
if(all(dws$precinct_id == tc$precinct_id)){
  print("All good!")
}else{
  print("Problem")
}


# Merge DWS and Tim Canova data
cd23 <- rbind(dws,tc)


# Calculate cumulative votes and percentages
cd23$dwscum <- cumsum(cd23$vote[grep("Wasserman",cd23$cand_name)])
cd23$tccum <- cumsum(cd23$vote[grep("Canova",cd23$cand_name)])
cd23$totalcum <- cd23$dwscum + cd23$tccum

cd23$dwscumpct <- cd23$dwscum/cd23$totalcum
cd23$tccumpct <- cd23$tccum/cd23$totalcum


# Generare a plot
pdf("FL23_plot.pdf")
plot(c(cd23$totalcum,cd23$totalcum),
     c(cd23$dwscumpct,cd23$tccumpct),
     pch=".",
     type="n",
     xlab="cumulative vote (counting smallest to largest precincts by registered voters)",
     ylab="candidates' percentage of reported vote",
     xlim=c(0,51000),
     ylim=c(0,1),
     main = "Florida 23rd Congressional District Primary, 2016")
lines(cd23$totalcum,cd23$dwscumpct,col="blue",lwd=2.5)
lines(cd23$totalcum,cd23$tccumpct,col="red",lwd=2.5)
legend(25000,0.9,c("Wasserman Schultz","Canova"),lty=c(1,1),lwd=c(2.5,2.5),col=c("blue","red"))
abline(h=c(0.5),lty=2)
dev.off()