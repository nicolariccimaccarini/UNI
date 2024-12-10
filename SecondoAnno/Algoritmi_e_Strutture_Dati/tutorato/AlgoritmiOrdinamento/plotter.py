import matplotlib.pyplot as plt

with open("report.txt", "r") as f:
    lines = f.readlines()

algos = ["INSERTION", "MERGE", "HYBRIDMERGE"]

x_data = list(map(int, lines[0].split()))
y_data = [list(map(float, line.split())) for line in lines[1:]]

# per ogni elemento che c'e' in y_data ieriamo tenendoci traccia di un intero (i-esimo collezione che stiamo considerando) e il tempo corrispondente
for i, time in enumerate(y_data):
    plt.plot(x_data, time, label=algos[i])

plt.xlabel("Size")
plt.ylabel("Elapsed time [s]")
plt.title("Sorting experiments")
plt.legend()
plt.show()