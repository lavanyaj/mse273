import random

class Shipment:
    def __init__(self, origin, dest, weight, day, time_of_day):
        self.origin = origin
        self.dest = dest
        self.weight = weight
        self.day = day
        self.time_of_day = time_of_day

    def getString(self):
        return ("[weight: " + str(self.weight) + "]")

    def show(self):
        print self.getString()
class ShipmentGenerator:
    def __init__(self, origin, dest, weight_pdf, day, time_of_day):
        self.origin = origin
        self.dest = dest
        self.weight_pdf = Pdf(weight_pdf)
        self.day = day
        self.time_of_day = time_of_day

    def getShipment(self):
        u = random.random()
        weight = self.weight_pdf.getNext(u)
        while weight > 15:
            u = random.random()
            weight = self.weight_pdf.getNext(u)
        #print u, " ", weight
        return Shipment(self.origin, self.dest, weight,\
                            self.day, self.time_of_day)

class Pdf:
    def __init__(self, pdfFile=None):
        self.epsilon = 1e-6
        self.cdf = []
        self.values = []
        if pdfFile is not None:
            f = open(pdfFile, "r")
            cdf = 0
            for line in f.readlines():
                words = line.rstrip('\n').split()
                pdf = float(words[1])
                val = float(words[0])
                cdf += pdf
                self.cdf.append(cdf)
                self.values.append(val)
            assert(cdf >= 1 - self.epsilon)
            assert(cdf <= 1 + self.epsilon)
        return
    
    def getNext(self, u):
        assert(u <=1)
        assert(u > 0)
        index = 0
        for pr in self.cdf:
            if (u <= pr):
                break
            index += 1
        assert(u <= self.cdf[index])
        return self.values[index]
        


        
