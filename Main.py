import numpy as np
import matplotlib.pyplot as plt
import math


def sample_compound_poisson_paths(lam: float, z: callable, T: float, n: int, z_args=None):
    if z_args is None:
        z_args = {}
    path_data = [[(0, 0)] for i in range(n)]

    for ipath in range(n):
        current_time = 0
        current_path_value = 0

        while current_time < T:
            increment_time = np.random.exponential(1/lam)
            current_time += increment_time

            current_path_value += z(**z_args)
            path_data[ipath].append((current_time, current_path_value))

    return path_data


def simulate_brownian_motion(T, Nt, N, W0: float = 0):
    Wt = W0 * np.ones((Nt, N))
    h = T / Nt

    Wtpredj = 0

    for i in range(Nt):
        dWt = np.random.randn(N) * (h**0.5)
        Wt[i, :] = Wtpredj + dWt
        Wtpredj = Wt[i, :]

    return Wt


def convert_sample_poisson_to_discrete_time(path_data, Nt, h=1):
    values = np.zeros((len(path_data), Nt))

    for i, path in enumerate(path_data):
        current_path_data_index = 0

        for time_step in range(Nt):
            upper_time_step_time = (Nt+1) * h

            total_time_step_value = 0
            traversed_time_steps = 0

            while (path[current_path_data_index][0] < upper_time_step_time) and (current_path_data_index < len(path)-1):
                total_time_step_value += path[current_path_data_index][1]
                current_path_data_index += 1
                traversed_time_steps += 1

            if traversed_time_steps > 0:
                average_time_step_value = total_time_step_value/traversed_time_steps
            else:
                average_time_step_value = values[i, time_step-1]

            values[i, time_step] = average_time_step_value

    return values


def one_function():
    return 1

path_data = sample_compound_poisson_paths(1,  one_function, 10, 3)
max_len = max(len(lst) for lst in path_data)
padded_arrays = [lst + [[np.nan, np.nan]] * (max_len - len(lst)) for lst in path_data]

path_data_arrays = []
for path in padded_arrays:
    path_data_arrays.append(np.array(path))

path_data_arrays = np.array(path_data_arrays)
export_paths = path_data_arrays.reshape(-1, path_data_arrays.shape[-1])
header = "Time, Value"
np.savetxt("PoissonSim" + ".csv", export_paths, delimiter=',', fmt='%f', header=header)


print(export_paths)
print(export_paths.shape)




# export_poisson_array = np.array(padded_arrays)
# print(export_poisson_array.shape)
#
#

#discretised_path_data = convert_sample_poisson_to_discrete_time(path_data, 10, 1)

for path in path_data:
    x, y = zip(*path)
    plt.plot(list(x[:-1]) + [x[-1]], y, drawstyle='steps-post')

plt.show()

# for path in discretised_path_data:
#     x = np.arange(0, 50, 1)
#     plt.plot(x, path)
#
# plt.show()
#
#
# path_data2 = sample_compound_poisson_paths(0.5, np.random.exponential, 20, 3)
# for path in path_data2:
#     x, y = zip(*path)
#     plt.plot(x, y, drawstyle='steps-post')

plt.show()





#
# brownian_path_data = simulate_brownian_motion(1000, 1000, 3)
# brownian_time_data = np.arange(0, 1000, 1)
#
# print(brownian_path_data.shape)
#
# for j in range(brownian_path_data.shape[1]):
#     plt.plot(brownian_time_data, brownian_path_data[:, j])
#
# plt.show()
#
# np.savetxt("BrownianSim" + ".csv", brownian_path_data, delimiter=',', fmt='%f')


