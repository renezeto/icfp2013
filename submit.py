import os
import sys
import time
import numpy as np

def main():
    if "get" in sys.argv:
        n = int(sys.argv[2])
        size = sys.argv[3]
        i = 0
        while i<n:
            os.system("./get_training %s"%(size))
            i+=1
    if "solve" in sys.argv:
        problems_str = ""
        size = sys.argv[2]
        dirname = 'trainings/%s' % (size)
        probIDs = [name for name in os.listdir(dirname)]
        if "problems" in sys.argv:
            problems_str = "problem"
            dirname = 'problems/%s' % (size)
            probIDs = [name for name in os.listdir(dirname)]
        for problem in probIDs:
            if os.path.exists(os.path.join(dirname, problem, "solved")):
                print "problem %s is already solved" % problem
            else:
                os.system("./make_guess %s %s %s"%(size, problem, problems_str))
                time.sleep(10)

#        ./make_guess 3 iwxYDVp0AsouTAvEEKBgwy1E
if __name__ == '__main__':
    main()
        


