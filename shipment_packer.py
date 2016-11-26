from shipment_generator import *

class Truck:
    def __init__(self, weight):
        self.weight = weight
        self.shipment_list = []
        self.shipment_weight = 0
        return

    def getString(self):
        return "shipment_weight: " +\
            str(self.shipment_weight)+\
            " {"+\
            ",".join(s.getString()\
                         for s in self.shipment_list)+\
                         "}"
    
    def show(self):
        print self.getString()
        
class ShipmentPacker:
    def __init__(self, truck):
        self.truck = truck
        return

    def pack(self, shipment_list):
        bin_size = self.truck.weight
        sizes = [s.weight for s in shipment_list]
        packer = GreedyPacker()
        packing_list = packer.getPacking(sizes, bin_size)

        #print ("shipment sizes " + str(sizes) +\
        #           " packed like " +\
        #           str(packing_list))
        
        truck_list = []
        for packing in packing_list:
            truck = Truck(self.truck.weight)
            truck.shipment_list = [shipment_list[i] for i in packing]
            truck.shipment_weight = sum([s.weight for s in truck.shipment_list])
            truck_list.append(truck)
        return truck_list

class GreedyPacker:
    def __init__(self):
        return

    def getPacking(self, sizes, bin_size):
        indexed_sizes = [(i, sizes[i]) for i in range(len(sizes))]
        sorted_sizes = sorted(indexed_sizes, key=lambda k: k[1], reverse=True)
        bins_list = []
        item_list = []
        bin_filled = 0
        for item in sorted_sizes:
            # print "item " + str(item),
            if (bin_filled + item[1] <= bin_size):
                item_list.append(item[0])
                bin_filled += item[1]
                #print " goes in existing bin " + str(item_list) +\
                #    "(" + str(bin_filled) + "/" + str(bin_size) + ")"
            else:
                bins_list.append(item_list)
                item_list = [item[0]]
                bin_filled = item[1]
                #print " goes in new bin " + str(item_list)
        bins_list.append(item_list)
        return bins_list

    def example(self):
        sizes = [10,15,15,12,3,2,10,12]
        bin_size = 20
        packer = GreedyPacker()
        bins_list = packer.getPacking(sizes, bin_size)
        print "sizes " + str(sizes)
        print " bin size " + str(bin_size)
        print " packing: " + str(bins_list)

