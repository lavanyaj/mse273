library(data.table)
cfs <- fread("~/Downloads/cfs_2012_pumf_csv.txt")

# round shipment weight to tons
cfs$SHIPMT_WGHT <- round(cfs$SHIPMT_WGHT/1000)
# round routed distance to hundreds of miles
cfs$SHIPMT_DIST_ROUTED <- round(cfs$SHIPMT_DIST_ROUTED/100)

# domestic
# EXPORT_YN=='N
# truck, for-hire or private. not parcel. not truck combined with other modes.
# (MODE==3 | MODE==4 | MODE==5)]
# not oil and gas 
# !(SCTG >= 15 & SCTG <= 19)
nonpetro_cfs <- cfs[EXPORT_YN=='N' & (MODE==3 | MODE==4 | MODE==5) & !(SCTG >= 15 & SCTG <= 19) & HAZMAT=='N']

shipments_by_industry <- nonpetro_cfs[,sum(WGT_FACTOR), by=NAICS]
setkey(shipments_by_industry,V1)
setkey(naics, V1)
setkey(shipments_by_industry, NAICS)
shipments_by_industry[naics,nomatch=0]

# want to make sure tons makes sense, in terms of number of hsipments, value and total weight
ltl_nonpetro_cfs <- nonpetro_cfs[SHIPMT_WGHT > 5000 & SHIPMT_WGHT <= 20000]



# number/tons and PDF of shipments by weight/ length/ industry etc.
pdf_num_by_col <- function(data, col) {
 shipments <- data[,sum(WGT_FACTOR), by=col]
 setkey(shipments, V1)
 total <- shipments[,sum(V1)]
 shipments_pdf <- shipments
 shipments_pdf$V1 <- shipments_pdf$V1/total
 plot(shipments_pdf) 
 }

pdf_tons_by_col <- function(data, col) {
 shipments <- data[,sum(WGT_FACTOR*SHIPMT_WEIGHT), by=col]
 setkey(shipments, V1)
 total <- shipments_by[,sum(V1)]
 shipments_pdf <- shipments
 shipments_pdf$V1 <- shipments_pdf$V1/total
 plot(shipments_pdf) 
}
 
num_by_col <- function(data, col) {
 data[,sum(WGT_FACTOR), by=col]
 }
tons_by_col <- function(data, col) {
 data[,sum(WGT_FACTOR*SHIPMT_WGHT), by=col]
 }

