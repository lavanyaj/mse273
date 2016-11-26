import data
from shipment_generator import *
from shipment_packer import *

import argparse

parser = argparse.ArgumentParser()
#parser.add_argument("--num_shipments", type=int, default=10)
parser.add_argument("--tons", type=int, default=200)
parser.add_argument("--pdf_file", type=str, default="pdf1.txt")
parser.add_argument("--num_runs", type=int, default=1000)
parser.add_argument("--lane", type=str, default="n/a")
parser.add_argument("--show_trucks", action="store_true", default=False)

args = parser.parse_args()

def run_once(generator, tons):
    shipment_list = []
    index = 0
    weight = 0
    while True:
        s = generator.getShipment()
        weight += s.weight
        if (weight < tons):
            shipment_list.append(s)
        else:
            break

    truck = Truck(20)

    packer = ShipmentPacker(truck)
    truck_list = packer.pack(shipment_list)
    total_weight = sum([s.weight for s in shipment_list])
    num_trucks = len(truck_list)
    average_fill = round((total_weight/(num_trucks * 20))*100)
    average_shipments = round((len(shipment_list)/(1.0*num_trucks)))
    max_shipments = max([len(t.shipment_list) for t in truck_list])
    return num_trucks, average_fill, average_shipments, max_shipments

def main():
    origin = "L.A."
    dest = "N.Y."
    day = "11-26-2016"
    time_of_day = "Morning"

    num_runs = args.num_runs
    lane = args.lane
    tons = args.tons
    pdf_file = args.pdf_file

    if lane in data.tons and lane in data.size_files:
        origin,dest = lane.rstrip().split("-")
        tons = (data.tons[lane])/365.0
        pdf_file = data.size_files[lane]
        print ("sample for %s - %s, %d tons/day ~ %s"\
                   % (origin, dest, tons, pdf_file))

    generator = ShipmentGenerator(\
        origin, dest, pdf_file,\
            day, time_of_day)

    total_num_trucks = 0
    total_average_fill = 0
    total_average_shipments = 0
    total_max_shipments = 0
    for i in range(num_runs):
        num_trucks, average_fill,\
            average_shipments, max_shipments\
            = run_once(generator, tons)
        total_num_trucks += num_trucks
        total_average_fill += average_fill
        total_average_shipments += average_shipments
        total_max_shipments += max_shipments

    mean_num_trucks = round(total_num_trucks/num_runs)
    mean_average_fill = round(total_average_fill/num_runs)
    mean_average_shipments = round(total_average_shipments/num_runs)
    mean_max_shipments = round(total_max_shipments/num_runs)

    print str(mean_num_trucks) + " trucks for average fill " +\
        str(mean_average_fill) + "%. " +\
        str(mean_average_shipments) + " avg. shipments per truck, " +\
        str(mean_max_shipments) + " max. shipments per truck. " +\
        " Averaged over "+ str(num_runs) + " runs."
#     shipment_list = []
#     index = 0
#     weight = 0
#     while True:
#         s = generator.getShipment()
#         weight += s.weight
#         if (weight < tons):
#             shipment_list.append(s)
#         else:
#             break

#     truck = Truck(20)

#     packer = ShipmentPacker(truck)
#     truck_list = packer.pack(shipment_list)
#     index = 0
#     total_weight = sum([s.weight for s in shipment_list])
#     num_trucks = len(truck_list)
#     average_fill = round((total_weight/(num_trucks * 20))*100)


#    if args.show_trucks:
#        for t in truck_list:
#            print "Truck "+\
#                str (index) + " " +\
#                t.getString()
#            index += 1
        
#     for i in range(10):
#         s = generator.getShipment()
#         s.show()
        
#    packer = GreedyPacker()
#    packer.example()

main()
