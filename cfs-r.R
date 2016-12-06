library(data.table)

options(warn=-1)

round_numbers <- function(test) {
	# round shipment weight to tons
 	test$SHIPMT_WGHT <- round(test$SHIPMT_WGHT/1000)
	# round routed distance to hundreds of miles
	test$SHIPMT_DIST_ROUTED <- round(test$SHIPMT_DIST_ROUTED/100)
	test
}

# want to add labels of origin, dest, industry
add_labels <- function(test) {
	library(plyr)
	cfs_area <- fread("~/mse273/cfs_area1.txt", sep=":", header=FALSE)
	setkey(test, ORIG_CFS_AREA)
	setkey(cfs_area, V1)
	test <- test[cfs_area,nomatch=0]
	test <- rename(test, c("V2"="ORIG_CFS_AREA_STR"))
	setkey(test, DEST_CFS_AREA)
	test <- test[cfs_area,nomatch=0]
	test <- rename(test, c("V2"="DEST_CFS_AREA_STR"))
	naics <- fread("~/mse273/naics.txt", sep=":", header=FALSE)
	setkey(naics, V1)
	setkey(test, NAICS)
	test <- test[naics,nomatch=0]
	test <- rename(test, c("V2"="NAICS_STR"))
	test
}

# top industries by number of shipments
top_industries_by_num <- function(test) {
	result <- test[,sum(WGT_FACTOR), by=NAICS_STR]
	setkey(result, V2)
	result
}

# top industries by tons of shipments
top_industries_by_tons <- function(test) {
	result <- test[,sum(WGT_FACTOR*SHIPMT_WGHT), by=NAICS_STR]
	setkey(result, V1)
	result
}

# top industries by tons of shipments * distance routed
top_industries_by_ton_miles <- function(test) {
	result <- test[,sum(WGT_FACTOR*SHIPMT_WGHT*SHIPMT_DIST_ROUTED), by=NAICS_STR]
	setkey(result, V1)
	result
}

# top lanes by number of shipments
top_lanes_by_num <- function(test) {
	result <- test[,sum(WGT_FACTOR), by=c(ORIG_CFS_AREA_STR, DST_CFS_AREA_STR)]
	setkey(result, V2)
	result
}

# top lanes by tons of shipments
top_lanes_by_tons <- function(test) {
	result <- test[,sum(WGT_FACTOR*SHIPMT_WGHT), by=c('ORIG_CFS_AREA_STR', 'DEST_CFS_AREA_STR')]
	setkey(result, V1)
	result
}

# top lanes by tons of shipments * distance routed
top_lanes_by_ton_miles <- function(test) {
	result <- test[,sum(WGT_FACTOR*SHIPMT_WGHT*SHIPMT_DIST_ROUTED), by=c(ORIG_CFS_AREA_STR, DST_CFS_AREA_STR)]
	setkey(result, V1)
	result
}

# number/tons and PDF of shipments by weight/ length/ industry etc.
pdf_num_by_col <- function(data, col) {
 shipments <- data[,sum(WGT_FACTOR), by=col]
 total <- shipments[,sum(V1)]
 shipments_pdf <- shipments
 shipments_pdf$V1 <- shipments_pdf$V1/total
 setkeyv(shipments_pdf, col)
 shipments_pdf
 
 }


pdf_tons_by_col <- function(data, col) {
 shipments <- data[,sum(WGT_FACTOR*SHIPMT_WGHT), by=col]
 total <- shipments[,sum(V1)]
 shipments_pdf <- shipments
 shipments_pdf$V1 <- shipments_pdf$V1/total
 setkeyv(shipments_pdf, col)
 shipments_pdf
}
 
num_by_col <- function(data, col) {
 data[,sum(WGT_FACTOR), by=col]
 }
tons_by_col <- function(data, col) {
 data[,sum(WGT_FACTOR*SHIPMT_WGHT), by=col]
 }

lanes_between <- function(data, low, high) {
	data[(SHIPMT_DIST_ROUTED > low & SHIPMT_DIST_ROUTED <= high)]
} 

long_lanes <- function(data) {
 	data[(SHIPMT_DIST_ROUTED > 10 & SHIPMT_DIST_ROUTED <= 30)]
 }
 medium_lanes <- function(data) {
 	data[(SHIPMT_DIST_ROUTED > 6 & SHIPMT_DIST_ROUTED <= 10)]
 }
 short_lanes <- function(data) {
 	data[(SHIPMT_DIST_ROUTED > 2 & SHIPMT_DIST_ROUTED <= 6)]
 }
 light_shipments <- function(data) {
 	data[(SHIPMT_WGHT > 5 & SHIPMT_WGHT <= 10)]
 }
 heavy_shipments <- function(data) {
 	data[(SHIPMT_WGHT > 10 & SHIPMT_WGHT <= 15)]
 }
 very_heavy_shipments <- function(data) {
 	data[(SHIPMT_WGHT > 15 & SHIPMT_WGHT <= 20)]
 }

total_num_shipments <- function(data) {
	data[,sum(WGT_FACTOR)]
}

total_tons_shipments <- function(data) {
	data[,sum(WGT_FACTOR*SHIPMT_WGHT)]
}

# all shipments
cfs <- fread("~/mse273/cfs_2012_pumf_csv.txt", header=TRUE)
cfs <- round_numbers(cfs)

cat("Total shipment data")
cat("\nTotal number of shipments: ")
total_num_shipments(cfs)
cat("\nTotal tons of shipments: ")
total_tons_shipments(cfs)

# filter out LTL
shipments <- cfs[EXPORT_YN=='N' & (MODE==3 | MODE==4 | MODE==5) & !(SCTG >= 15 & SCTG <= 19) & HAZMAT=='N' & (SHIPMT_WGHT > 5 & SHIPMT_WGHT <= 20) & (SHIPMT_DIST_ROUTED > 2 & SHIPMT_DIST_ROUTED <= 30)]
# & (ORIG_CFS_AREA != DEST_CFS_AREA)]

cat("\nLTL shipment data")
cat("\nTotal number of shipments: ")
total_num_shipments(shipments)
cat("\nTotal tons of shipments: ")
total_tons_shipments(shipments)

labeled_shipments <- add_labels(shipments)
# top industries by ton*miles
cat("\nTop industries by ton-miles:\n")
r1 <- top_industries_by_ton_miles(labeled_shipments)
r1

min_distance_list <- c(2,6,10)
max_distance_list <- c(6, 10, 30)
distances <- data.frame(x=min_distance_list, y=max_distance_list)
num_distances <- dim(distances)[1]

for (distance_type in c(1:num_distances)) { 
	min_distance <- distances[distance_type,1]
	max_distance <- distances[distance_type,2]

	cat("\nTop (", min_distance*100, "-", max_distance*100, " mi) lanes by tons:\n")
	long_lanes_shipments <- lanes_between(labeled_shipments, min_distance, max_distance)
	top_long_lanes <- top_lanes_by_tons(long_lanes_shipments)
	top_long_lanes <- rename(top_long_lanes, c("V1"="TONS"))
	num_long_lanes <- dim(top_long_lanes)[1]
	topx <- min(num_long_lanes,20)
	print(top_long_lanes[(num_long_lanes):(num_long_lanes-topx),])

	# shipments in lane1
	for (index in c(0:topx)) {
	cat("\nTop (", min_distance*100, "-", max_distance*100, " mi) lane #", index, " by tons:\n")
	lane <- top_long_lanes[num_long_lanes-index,]
	tons_forward <- lane[1,TONS]
	print(lane[1,])
	setkey(lane,ORIG_CFS_AREA_STR,DEST_CFS_AREA_STR )
	setkey(labeled_shipments, ORIG_CFS_AREA_STR,DEST_CFS_AREA_STR)
	lane_shipments <- labeled_shipments[lane]

	sample_distance <- lane_shipments[1,SHIPMT_DIST_ROUTED]

	# reverse lane characteristics
	cat("\nTons of shipments on reverse lane #", index)
	lane_reverse <- labeled_shipments[DEST_CFS_AREA_STR==(lane[1,ORIG_CFS_AREA_STR]
			) & ORIG_CFS_AREA_STR==(lane[1,DEST_CFS_AREA_STR])]
	tons_reverse <- total_tons_shipments(lane_reverse)

	sample_reverse_distance <- lane_reverse[1,SHIPMT_DIST_ROUTED]
	ratio <- round(tons_reverse*100/tons_forward)
	cat("\nTons forward ", tons_forward, " (" , sample_distance*100, "mi) and tons reverse ", 
		tons_reverse, " (" , sample_distance*100, "mi) ratio ", ratio, " %\n")

	if (ratio > 50 & ratio < (100)) {
	# top industries by tons in lane1
	cat("\nTop 5 industries in lane #", index)
	top_industries_in_lane <- top_industries_by_tons(lane_shipments)
	num_industries <- dim(top_industries_in_lane)[1]
	topx <- min(num_industries,5)
	print(top_industries_in_lane[(num_industries):(num_industries-topx),])

	# size distribution
	cat("\nSize distribution (by number) in lane #", index)
	p <- pdf_num_by_col(lane_shipments, 'SHIPMT_WGHT')
	print(p)
	}
}
}

cat("\nNumber of long lanes (1000-3000 mi) that are heavy enough (10% of best): XX\n")
cat("\nNumber of medium lanes (600-1000 mi) that are heavy enough (10% of best): XX")

