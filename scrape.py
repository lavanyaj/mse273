#import simplejson, urllib
# cat 123load_Oct22.txt  | sed $'s/<div/\\\n<div/g'
import re
import sys

def get_patterns():
    patterns = {}
    patterns["a_origin-city"] = re.compile("<div class=\"originData\">(.*), .*</div></div>")
    patterns["b_origin-state"] = re.compile("<div class=\"originData\">.*, (.*)</div></div>")
    patterns["c_dest-city"] = re.compile("<div class=\"destData\">(.*), .*</div></div>")
    patterns["d_dest-state"] = re.compile("<div class=\"destData\">.*, (.*)</div></div>")
    patterns["e_pickup"] = re.compile("<div class=\"pickupData\".*>(.*)</div>")
    patterns["f_length"] = re.compile("<div class=\"lengthData\">&nbsp;(.*)</div></div></div>")
    patterns["g_dist"] = re.compile("<div class=\"distData\"><span class=\"divLink\">(.*) mi</span></div>")
    patterns["h_weight"] = re.compile("<div class=\"weightData\">&nbsp;(.*) lb</div></div>")
    return patterns

def get_new_keys():
    new_keys = {}
    new_keys["origin-dest"] = ["b_origin-state", "d_dest-state"]
    return new_keys

def make_new_line(line):
    newRowPat = re.compile("<div value=\"(.*)\"")
    return (len(newRowPat.findall(line)) > 0)

def complete_row(row):
    should_have =  ["a_origin-city","b_origin-state","c_dest-city","d_dest-state","e_pickup","g_dist","h_weight"]
    return all([key in row for key in should_have])

# DISTANCEMATRIX_BASE_URL = 'https://maps.googleapis.com/maps/api/geocode/json'
# def get_time(origin, dest):
#     url = DISTANCEMATRIX_BASE_URL + '?' \
#         + "origins=" +  urllib.urlencode(origin)\
#         + "&destination=" + urllib.urlencode(dest)\
#         + "&mode=driving"
#     result = simplejson.load(urllib.urlopen(url))

#     print simplejson.dumps(result)
#     return 0
    
def parse_to_csv(filename="tmp-lb.txt", header=False):
    f = open(filename, "r")

    download_date = ""
    download_date_pattern = re.compile("123load.Oct(\d+).*")
    arr = download_date_pattern.findall(filename)
    if len(arr) > 0:
        download_date = "10/%s"%arr[0]

    #print "download date is " + download_date
    
    origins = {}
    dests = {}
    
    rows = []
    last_row = {}

    patterns = get_patterns()
    new_keys = get_new_keys()

    sorted_new_keys = sorted(new_keys.keys())
    sorted_pattern_keys = sorted(patterns.keys())

    if (header):
        # print header- new keys followed by existing
        for key in sorted_new_keys:
            print (key + ","),

        for key in sorted_pattern_keys:
            print key + ",",
        print "i_download-date,",
        print ""

    for line in f.readlines():
        if make_new_line(line) and complete_row(last_row):
            # print new keys (when valid)
            origin = ",".join([last_row["a_origin-city"],\
                                  last_row["b_origin-state"]])
            dest = ",".join([last_row["c_dest-city"],\
                                   last_row["d_dest-state"]])
            origins[str.upper(origin)] = True
            dests[str.upper(dest)] = True
            for key in sorted_new_keys:
                subkeys = new_keys[key]
                valid = all([subkey in last_row for subkey in subkeys])
                value = ""
                if valid:
                    value = "-".join([last_row[subkey] for subkey in subkeys])
                print (value + ","),
                # end for key in sorted_new_keys
                
            for key in sorted_pattern_keys:
                value = ""
                if key in last_row:
                    value = last_row[key]
                print (value + ","),
                # end for key in sorted_pattern
            print (download_date + ","),
            print ""
            last_row = {}
            # end if make_new_line

        for key in sorted_pattern_keys:
            pat = patterns[key]
            arr = pat.findall(line)
            if len(arr) > 0:
                last_row[key] = str(arr[0])
                if "div" in str(arr[0]):
                    print line + ": searching for " \
                        + key + ": found " + str(arr[0])                
                    # end
             # end
        # end
    # end
# end


                    # end if "div"
                    # end if len(arr) > 0
                    # end for key in sorted_pattern
                    # end for line in ..

#         print "pairwise distances"
#         for origin in sorted(origins.keys()):
#             for dest in sorted(dests.keys()):
#                 print "%s, %s" % (origin, dest)
#                 print "%s, %s" % (dest, origin)
# https://maps.googleapis.com/maps/api/distancematrix/json?origins=Vancouver+BC&destinations=San+Francisco&mode=driving        
#f = open("123load_Oct22.txt", "r")

parse_to_csv(sys.argv[1]) #"123load_Oct22.txt")

#get_time("Delhi, India", "Bangalore, India")
