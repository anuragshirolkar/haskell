


filename = input()

file = open(filename+".obj", "r")
file_clean = open(filename+"_clean.obj", "w")
for x in file:
    if x[:2] in ["v ", "vt", "vn", "f ", "# "] :
        file_clean.write(x)

