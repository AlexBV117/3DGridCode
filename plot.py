import matplotlib.pyplot as plt
import numpy as np
from pandas import read_csv
import os
import re

data_file_list = np.array(os.listdir("./data"))
mask = [os.path.isfile(f"./data/{p}") for p in data_file_list]
data_file_list = data_file_list[mask]

fig, ax = plt.subplots()

for data_file in data_file_list:
    # read in data
    # True = "./data/gen.csv"
    data = read_csv(f"./data/{data_file}", header=None)
    match = re.search("gen_data_\s?(\d*.\d*)\.csv", data_file)
    radius = float(match.group(1))
    mass = round(((4/3) * 19 * np.pi * (radius**3))/1_000)
    gen_data = list(data.iloc[:, 0])
    gen = list(data.iloc[:, 1]/10_000)
# # data = np.fromfile(file, dtype=np.float64, sep="")
# # set the number of voxels here, e.g (nxg, nyg, nzg)
# # voxels = (201, 201, 201)
# # data = data.reshape(voxels, order="F")
#
# # plot a slice though middle of fluence grid
# gens = np.linspace(1, 16, 16, dtype=np.int16)
# gens_binned = []
# for gen in gens:
#     gens_binned.append(gen_data.count(gen))
    ax.plot(gen_data, gen, label=mass)
ax.legend(loc="upper right")
plt.yscale("log")
plt.show()
