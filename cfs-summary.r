library(data.table)

options(warn=-1)

MIN_LANE_TONS_PER_YEAR = 62500
HEAVY_LANE_TONS_PER_YEAR = 125000
BALANCED_LANE_LOWER = 50
BALANCED_LANE_UPPER = 100
LBS_PER_TON = 2000
MIN_WGHT = 4 
MAX_WGHT = 15 #15

round_numbers <- function(test) {
	# round shipment weight to tons
 	test$SHIPMT_WGHT <- test$SHIPMT_WGHT/LBS_PER_TON
	# round routed distance to hundreds of miles
	test$SHIPMT_DIST_ROUTED <- test$SHIPMT_DIST_ROUTED/100
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

is_long_lane <- function(distance) {
	distance > 10 & distance <= 30
 	#data[(SHIPMT_DIST_ROUTED > 10 & SHIPMT_DIST_ROUTED <= 30)]
 }
 is_medium_lane <- function(distance) {
 	distance > 6 & distance <= 10
 	#data[(SHIPMT_DIST_ROUTED > 6 & SHIPMT_DIST_ROUTED <= 10)]
 }
 is_short_lane <- function(distance) {
 	distance > 2 & distance <= 6
 	#data[(SHIPMT_DIST_ROUTED > 2 & SHIPMT_DIST_ROUTED <= 6)]
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

total_length_shipments <- function(data) {
	data[,sum(WGT_FACTOR* SHIPMT_DIST_ROUTED)]
}

# all shipments
cfs <- fread("~/mse273/cfs_2012_pumf_csv.txt", header=TRUE)
cfs <- round_numbers(cfs)
n<-total_num_shipments(cfs)
l <- 100 * total_length_shipments(cfs)
t <- total_tons_shipments(cfs)

cat("Total shipment data")
cat("\nTotal number of shipments: ", n)
cat("\n Total tons of shipments: ", t)
cat("\n Total length of shipments: ", l)
cat("\nAvg length of shipments: ", (l/n))
cat("\nAvg weight of shipments: ", (t/n))

parcel_shipments <- cfs[MODE ==14]
n<-total_num_shipments(parcel_shipments)
l <- 100 * total_length_shipments(parcel_shipments)
t <- total_tons_shipments(parcel_shipments)

cat("\nParcel-only shipment data")
cat("\nTotal number of shipments: ", n)
cat("\n Total tons of shipments: ", t)
cat("\n Total length of shipments: ", l)
cat("\nAvg length of shipments: ", (l/n))
cat("\nAvg weight of shipments: ", (t/n))

od_shipments <- cfs[(MODE==3 | MODE==4 | MODE==5 | MODE ==14) & !(SCTG >= 15 & SCTG <= 19) & HAZMAT=='N']
n<-total_num_shipments(od_shipments)
l <- 100 * total_length_shipments(od_shipments)
t <- total_tons_shipments(od_shipments)
cat("\nOld Dominion shipment data")
cat("\nTotal number of shipments: ", n)
cat("\n Total tons (2000 lbs) of shipments: ", t)
cat("\n Total length of shipments: ", l)
cat("\nAvg length of shipments: ", (l/n))
cat("\nAvg weight of shipments: ", (t/n))

th_shipments <- cfs[EXPORT_YN=='N' & (MODE==3 | MODE==4 | MODE==5 | MODE ==14) & !(SCTG >= 15 & SCTG <= 19) & HAZMAT=='N']
n<-total_num_shipments(th_shipments)
l <- 100 * total_length_shipments(th_shipments)
t <- total_tons_shipments(th_shipments)
cat("\n Parcel and Truck, Domestic shipment data")
cat("\nTotal number of shipments: ", n)
cat("\n Total tons of shipments: ", t)
cat("\n Total length of shipments: ", l)
cat("\nAvg length of shipments: ", (l/n))
cat("\nAvg weight of shipments: ", (t/n))

# filter out LTL
shipments <- cfs[EXPORT_YN=='N' & (MODE==3 | MODE==4 | MODE==5) & !(SCTG >= 15 & SCTG <= 19) & HAZMAT=='N' & (SHIPMT_WGHT > MIN_WGHT & SHIPMT_WGHT <= MAX_WGHT) & (SHIPMT_DIST_ROUTED > 2 & SHIPMT_DIST_ROUTED <= 30)]
# & (ORIG_CFS_AREA != DEST_CFS_AREA)]
n<-total_num_shipments(shipments)
l <- 100 * total_length_shipments(shipments)
t <- total_tons_shipments(shipments)
cat("\nLTL shipment data")
cat("\nTotal number of shipments: ", n)
cat("\n Total tons of shipments: ", t)
cat("\n Total length of shipments: ", l)
cat("\nAvg length of shipments: ", (l/n))
cat("\nAvg weight of shipments: ", (t/n))

labeled_shipments <- add_labels(shipments)
# top industries by ton*miles
cat("\nTop industries by ton-miles:\n")
r1 <- top_industries_by_ton_miles(labeled_shipments)
r1

min_distance_list <- c(2)
max_distance_list <- c(30)
distances <- data.frame(x=min_distance_list, y=max_distance_list)
num_distances <- dim(distances)[1]
	long_heavy_tons = 0
	long_tons = 0
	short_tons = 0
	long_lanes = 0
	short_lanes = 0
	long_heavy_lanes = 0
	medium_heavy_lanes = 0
	short_heavy_lanes = 0
	medium_lanes = 0
	medium_tons = 0
	one_way_tons = 0
	short_heavy_tons = 0
	medium_heavy_tons = 0
	total_distance_lanes = 0
	total_num_lanes = 0
	
for (distance_type in c(1:num_distances)) { 
	min_distance <- distances[distance_type,1]
	max_distance <- distances[distance_type,2]

	cat("\nTop (", min_distance*100, "-", max_distance*100, " mi) lanes by tons:\n")
	long_lanes_shipments <- lanes_between(labeled_shipments, min_distance, max_distance)
	top_long_lanes <- top_lanes_by_tons(long_lanes_shipments)
	top_long_lanes <- rename(top_long_lanes, c("V1"="TONS"))
	
	
	#topx <- min(num_long_lanes,20)
	heavy_lanes <- top_long_lanes[TONS > MIN_LANE_TONS_PER_YEAR]
	num_long_lanes <- dim(heavy_lanes)[1]


 for (index in c(0:num_long_lanes)) {
	 lane <- heavy_lanes[num_long_lanes-index,]
 	tons_forward <- lane[1,TONS]
 	setkey(lane,ORIG_CFS_AREA_STR,DEST_CFS_AREA_STR )
 	setkey(labeled_shipments, ORIG_CFS_AREA_STR,DEST_CFS_AREA_STR)
 	lane_shipments <- labeled_shipments[lane]
 
 	sample_distance <- lane_shipments[1,SHIPMT_DIST_ROUTED]
 	lane_reverse <- labeled_shipments[DEST_CFS_AREA_STR==(lane[1,ORIG_CFS_AREA_STR]
 			) & ORIG_CFS_AREA_STR==(lane[1,DEST_CFS_AREA_STR])]
 	tons_reverse <- total_tons_shipments(lane_reverse)
 	num_forward <- total_num_shipments(lane_shipments)
	num_reverse <- total_num_shipments(lane_reverse)

	avg_forward = 0
	avg_reverse = 0
	if (num_forward > 0) {avg_forward = (1.0*tons_forward)/num_forward}
	if (num_reverse > 0) {avg_reverse = (1.0*tons_reverse)/num_reverse}

 	#{cat("\n weird lane", origin, " ", dest, " ", ratio, " ", tons_forward, " ", sample_distance)}
 	# {cat(tons_forward, " ", tons_reverse, " ", ratio, "\n")}
 	kind = "na"
	if (tons_reverse > 0 & tons_forward > 0) {
 		ratio <- round(tons_reverse*100/tons_forward)
 		if (ratio> BALANCED_LANE_LOWER & ratio< BALANCED_LANE_UPPER) { 
		total_distance_lanes = total_distance_lanes + sample_distance
		total_num_lanes = total_num_lanes + 1
 		if (is_long_lane(sample_distance)) {
		        kind = "long"
 			long_lanes = long_lanes + 1
 			long_tons = long_tons + tons_forward+tons_reverse
 			if (tons_forward > HEAVY_LANE_TONS_PER_YEAR) {
			        kind = "long heavy"
 				long_heavy_tons = long_heavy_tons + tons_forward+tons_reverse
 				long_heavy_lanes = long_heavy_lanes + 1
 				}
 			} else if (is_medium_lane(sample_distance)) {
 			       	kind = "medium"
				medium_lanes = medium_lanes + 1
 				medium_tons = medium_tons + tons_forward+tons_reverse
 				if (tons_forward > HEAVY_LANE_TONS_PER_YEAR) {
				        kind = "medium heavy"
 					medium_heavy_lanes = medium_heavy_lanes + 1	
 					medium_heavy_tons = medium_heavy_tons + tons_forward+tons_reverse
 				}
 			} else if (is_short_lane(sample_distance)) {
			        kind = "short"
 				short_lanes = short_lanes + 1
 				short_tons = short_tons + tons_forward+tons_reverse
 				if (tons_forward > HEAVY_LANE_TONS_PER_YEAR) {
				 kind = "short heavy"
 					short_heavy_lanes = short_heavy_lanes + 1	
 					short_heavy_tons = short_heavy_tons + tons_forward+tons_reverse
 					}
 				}
 	origin <- lane[1, ORIG_CFS_AREA_STR]
 	dest <- lane[1, DEST_CFS_AREA_STR]
 	cat("\n", sample_distance*100, " mi", origin, " ", dest, " ", ratio, "% ", tons_forward, " ", avg_forward, " ", tons_reverse, " ", avg_reverse, " ", sample_distance*100, " mi",  " ", kind)
 	} else {
 		one_way_tons = one_way_tons + tons_forward+tons_reverse
 	}
  }
 }
}

cat("\nTotal number of lanes ", total_num_lanes, " avg. lane distance ", round(total_distance_lanes/total_num_lanes))
cat("\nNumber of long lanes (1000-3000 mi) that are heavy (>",(HEAVY_LANE_TONS_PER_YEAR/1000),"K tons /yr):", long_heavy_lanes,"\n")
cat("\nNumber of medium lanes (600-1000 mi) that are heavy (>",(HEAVY_LANE_TONS_PER_YEAR/1000),"K tons /yr):", medium_heavy_lanes,"\n")
cat("\nNumber of short lanes (200-600 mi) that are heavy (>",(HEAVY_LANE_TONS_PER_YEAR/1000),"K tons /yr):", short_heavy_lanes,"\n")

cat("\nNumber of long lanes (1000-3000 mi)  (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", long_lanes,"\n")
cat("\nNumber of medium lanes (600-1000 mi) (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", medium_lanes,"\n")
cat("\nNumber of short lanes (200-600 mi)   (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", short_lanes,"\n")

total_tons = long_tons + medium_tons + short_tons
cat("\nFraction of tons on heavy long lanes (1000-3000 mi)  (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (long_heavy_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")
cat("\nFraction of tons on long lanes (1000-3000 mi)  (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (long_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")
cat("\nFraction of tons on heavy medium lanes (1000-3000 mi)  (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (medium_heavy_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")
cat("\nFraction of tons on medium lanes (600-1000 mi) (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (medium_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")
cat("\nFraction of tons on heavy short lanes (1000-3000 mi)  (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (short_heavy_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")
cat("\nFraction of tons on short lanes (200-600 mi)   (>",(MIN_LANE_TONS_PER_YEAR/1000),"K tons /yr):", (short_tons*100/total_tons),"% of ", total_tons ," tons/ yr\n")

total_any_ratio_tons = total_tons + one_way_tons
cat("\nFraction of tons on balanced lanes :", (total_tons*100/total_any_ratio_tons),"% of ", total_any_ratio_tons ," tons/ yr\n")

