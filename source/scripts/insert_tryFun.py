#!/usr/bin/python

tag = "tryFunction"
tagString1 = "tryFunction("
tagString2 = ", place = 'co')\n"


import sys

print """
converting
"""

filename = sys.argv[1]
print "file {}".format(filename)

print ""
print "writing {}.bak".format(filename)
file = open(filename, "r")
lines = file.readlines()
file.close()

file = open(filename + '.bak', "w")
file.write("".join(lines))
file.close()

n = len(lines)
print "{} lines".format(n)
print ""

parC = 0
start = 0
end = 0

for i in range(0, n):
    line = lines[i]
    nTag = line.count(tag)
    if nTag == 1:
        print "tag found in line {}".format(i)
        lines[i] = tagString1
        start = i + 1
        print "start in line {}".format(start)
        line = lines[start]
        parC = line.count("(") - line.count(")")
        j = start
        while parC > 0:
            j = j + 1
            line = lines[j]
            parC = parC + line.count("(") - line.count(")")
            
        end = j
        print "end in line {}".format(end)
        lines[end] = "{}   {}".format(lines[end].rstrip(), tagString2)
        
    


print ""
print "writing {}".format(filename)
file = open(filename, "w")
file.write("".join(lines))
file.close()



print """
done
"""
