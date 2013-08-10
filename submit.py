import os
import sys
import time
import numpy as np
#error:
#response: Too many requests

class OverLimit(Exception):
    pass

def commandCheck(f): #never passes raises error - bug
    with open("status.txt","w") as create:
        pass
    def checkOutput(command):
        os.system("%s > status.txt"%(command))
        with open("status.txt","r") as openedLog:
            print openedLog.readlines()
            if "response: Too many requests\n" in openedLog.readlines():
                raise OverLimit()
    return checkOutput

@commandCheck
def runcmd(command):
    return 0

def foo():
    for j in range(30):
        if j==25:
            raise OverLimit()
        else:
            runcmd("echo hello...")

def main():
    if "get" in sys.argv:
        n = int(sys.argv[2])
        size = sys.argv[3]
        i = 0
        while i<n:
            try:
                runcmd("./get_training %s"%(size))
                i+=1
            except OverLimit:
                print "Too many requests! Trying again..."
                time.sleep(1)
                i+=1
    if "solve" in sys.argv:
        size = sys.argv[2]
        probIDs = [name for name in os.listdir('trainings/%s'%(size))]
        for problem in probIDs:
            runcmd("./make_guess %s %s"%(size, problem))


#        ./make_guess 3 iwxYDVp0AsouTAvEEKBgwy1E
if __name__ == '__main__':
    main()
        


