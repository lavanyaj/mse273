from shipment_generator import *
from shipment_packer import *

import argparse

parser = argparse.ArgumentParser()
#parser.add_argument("--num_shipments", type=int, default=10)
parser.add_argument("--tons", type=int, default=200)

parser.add_argument("--pdf_file", type=str, default="pdf1.txt")

args = parser.parse_args()

def main():
    origin = "L.A."
    dest = "N.Y."
    day = "11-26-2016"
    time_of_day = "Morning"


    generator = ShipmentGenerator(\
        origin, dest, args.pdf_file,\
            day, time_of_day)

    shipment_list = []
    index = 0
    weight = 0
    while True:
        s = generator.getShipment()
        weight += s.weight
        if (weight < args.tons):
            shipment_list.append(s)
        else:
            break

    truck = Truck(20)

    packer = ShipmentPacker(truck)
    truck_list = packer.pack(shipment_list)
    index = 0
    total_weight = sum([s.weight for s in shipment_list])
    num_trucks = len(truck_list)
    print str(num_trucks) + " trucks for average fill " +\
        str(round((total_weight/(num_trucks * 20))*100))\
        + "%"
    
    for t in truck_list:
        print "Truck "+\
            str (index) + " " +\
            t.getString()
        index += 1
        
#     for i in range(10):
#         s = generator.getShipment()
#         s.show()
        
#    packer = GreedyPacker()
#    packer.example()

main()
