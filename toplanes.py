f = open("FAFdata_9_Plastics_2015.csv", "r")
headers = f.readline().rstrip().lstrip("\"").rstrip("\"").split("\",\"")
OD_val = {}
index = {}
for i in range(len(headers)):
    index[headers[i]] = i
print index
# "DMS_ORIG","DMS_DEST","SCTG2","DMS_MODE","Total KTons in 2015"
for line in f.readlines():
    words = line.split(",")
    Origin = words[index["DMS_ORIG"]]
    Destination = words[index["DMS_DEST"]]
    if Origin != Destination:
        OD =  Origin + "-->" + Destination
        tons = float(words[index["Total KTons in 2015"]].rstrip().lstrip("\"").rstrip("\""))
        OD_val[OD] = {"tons": tons}

for OD in OD_val.keys():
    OD_val[OD]["reverse_tons"] = 0
    OD_val[OD]["total"] = 0
    OD_val[OD]["average"] = 0
    OD_val[OD]["balanced"] = 0
    
    tons = OD_val[OD]["tons"]    
    words = OD.split("-->")
    DO = words[1] + "-->" + words[0]
    if DO in OD_val:
        reverse_tons = OD_val[DO]["tons"]
        total = tons + reverse_tons
        OD_val[OD]["reverse_tons"] = reverse_tons
        OD_val[DO]["total"] = total
        OD_val[DO]["average"] = total/2.0
        OD_val[DO]["balanced"] = min(tons, reverse_tons)/max(tons, reverse_tons)

balanced_OD = [od for od in OD_val.keys() if OD_val[od]["balanced"] > 0.5]
OD_sorted_by_total = sorted(balanced_OD, key=lambda k: OD_val[k]["total"], reverse=True)
for i in range(50):
    OD = OD_sorted_by_total[i]
    Origin, Destination = OD.split("-->")
    print Origin + ", " + Destination + ", " +\
        str(OD_val[OD]["tons"]) + ", " +\
        str(OD_val[OD]["reverse_tons"])  + ", " +\
        str(OD_val[OD]["total"]) + ", " +\
        str(OD_val[OD]["average"]) + ", " +\
        str(OD_val[OD]["balanced"])

print(len(OD_val.keys()))    
