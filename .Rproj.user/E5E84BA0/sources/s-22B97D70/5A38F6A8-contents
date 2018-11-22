 
# read in data
ns = 10
data_dir = '_data/raw_data'

rawdata = c();
for (s in 1:ns) {
    sub_file = file.path(data_dir, sprintf('sub%02i/raw_data_sub%02i.txt',s,s))
    sub_data = read.table(sub_file, header = T, sep = ",")
    rawdata = rbind(rawdata, sub_data)
}
rawdata$accuracy = (rawdata$choice == rawdata$correct) * 1.0 # choosing higher rewProb

# check the data type
class(rawdata)

save(file = '_data/rawdata.RData', rawdata)

# write to txt for hBayesDM
write.table(rawdata, file = '_data/rawdata.txt', sep = "\t", dec = ".", col.names = TRUE,row.names = F)

#######################################################################
# prepare data List for Stan
NA_rows_all = which(is.na(rawdata), arr.ind = T)  # rows with NAs
NA_rows = unique(NA_rows_all[, "row"])
if (length(NA_rows) > 0) {
    rawdata = rawdata[-NA_rows, ]
    cat("The number of rows with NAs=", length(NA_rows), ". They are removed prior to modeling the data. \n", sep="")
}

# Individual Subjects
subjList <- unique(rawdata[,"subjID"])  # list of subjects x blocks
numSubjs <- length(subjList)  # number of subjects

# Information for user
cat(" # of subjects                 = ", numSubjs, "\n")

Tsubj <- as.vector( rep( 0, numSubjs ) ) # number of trials for each subject

for ( i in 1:numSubjs )  {
    curSubj  <- subjList[ i ]
    Tsubj[i] <- sum( rawdata$subjID == curSubj )  # Tsubj[N]
}

# Setting maxTrials
maxTrials <- max(Tsubj)

# Information for user continued
cat(" # of (max) trials per subject = ", maxTrials, "\n\n")

choice  <- array(1, c(numSubjs, maxTrials) )
outcome <- array(0, c(numSubjs, maxTrials) )

for (i in 1:numSubjs) {
    curSubj      <- subjList[i]
    useTrials    <- Tsubj[i]
    tmp          <- subset(rawdata, rawdata$subjID == curSubj)
    choice[i, 1:useTrials] <- tmp$choice
    outcome[i, 1:useTrials] <- sign(tmp$outcome)  # use sign
}

dataList <- list(
    subjID  = subjList,
    N       = numSubjs,
    T       = maxTrials,
    Tsubj   = Tsubj,
    choice  = choice,
    outcome = outcome)

save(file = '_data/dataList.RData', dataList)


