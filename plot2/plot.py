import matplotlib.pyplot as plt
import numpy as np
from pandas import read_csv
import os
import re

data_file_list = np.array(os.listdir("./data"))
mask = [os.path.isfile(f"./data/{p}") for p in data_file_list]
data_file_list = sorted(data_file_list[mask])
fig, ax = plt.subplots()

for data_file in data_file_list:
    data = read_csv(f"./data/{data_file}", header=None)
    match = re.search("gen_data_\s?(\d*.\d*)\.csv", data_file)
    radius = float(match.group(1))
    mass = round(((4/3) * 19 * np.pi * (radius**3))/1_000)
    gen_data = list(data.iloc[:, 0])
    gen = list(data.iloc[:, 1]/ 10_000)
    ax.plot(gen_data, gen, label=f"Radius: {radius}cm, Mass: {mass}kg")
ax.legend(loc="upper right")
ax.set_title("Criticality of Split U-235 Sphere at Different Separations.")
ax.set_xlabel("neutron - generation")
ax.set_ylabel("nautrons / N(0)")
plt.yscale("log")
fig.savefig("./img/Criticality_1.png", dpi=600)
plt.show()
