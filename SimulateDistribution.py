import scipy as sp
import numpy as np
import matplotlib.pyplot as plt

from abc import ABC, abstractmethod


class LevyTriplet:
    def __init__(self, drift, sigma, levy_measure):
        self.drift = drift
        self.sigma = sigma
        self.levy_measure = levy_measure


class Distribution:
    def __init__(self, levy_triplet: LevyTriplet, pars_dict: dict):
        self.levy_triplet = levy_triplet

        self.pars_dict = pars_dict

    def get_levy_interval_moment(self, n: int, lb: float, ub: float):
        def moment_func(x):
            return x**n * self.levy_triplet.levy_measure(x)

        return sp.integrate.quad(moment_func, lb, ub)[0]


class JumpSimulation:
    def __init__(self, distribution: Distribution, epsilon: float, positive_interval_count: int, ub: float, T: float, N: int):
        self.distribution = distribution
        self.epsilon = epsilon
        self.positive_interval_count = positive_interval_count
        self.ub = ub  # We are assuming a symmetric distribution where the intervals are centred at 0
        self.T = T
        self.N = N

        self.increment_length = self.T / self.N
        self.simulated_values = None

    def compute_poisson_intensities_jumps(self):
        poisson_intensities = np.zeros(self.positive_interval_count*2, dtype=float) # Can change this back to empty (easier to see errors)
        poisson_jumps = np.zeros(self.positive_interval_count*2, dtype=float)

        step_size = (self.ub-self.epsilon) / self.positive_interval_count
        current_i_lb = self.epsilon

        for i in range(self.positive_interval_count):
            current_i_ub = current_i_lb + step_size
            n_i = self.positive_interval_count - i - 1 # This is different from in R, check this
            p_i = self.positive_interval_count + i

            poisson_intensities[n_i] = self.distribution.get_levy_interval_moment(0, -1*current_i_ub,
                                                                                  -1*current_i_lb)
            poisson_jumps[n_i] = -1 * self.compute_jump_size(poisson_intensities[n_i], -1*current_i_ub, -1*current_i_lb)

            poisson_intensities[p_i] = self.distribution.get_levy_interval_moment(0, current_i_lb, current_i_ub)
            poisson_jumps[p_i] = self.compute_jump_size(poisson_intensities[p_i], current_i_lb, current_i_ub)

            current_i_lb += step_size

        return poisson_intensities, poisson_jumps

    def compute_jump_size(self, interval_intensity, lb: float, ub: float):
        variance = self.distribution.get_levy_interval_moment(2, lb, ub)

        return np.sqrt(variance / interval_intensity)  # Why is this wiggling at me?

    def simulate_jump_data(self, intensities, jump_sizes):
        jump_data = np.array([]).reshape(0, 2)

        for i in range(len(intensities)):
            jump_times = self.get_poisson_jump_times(intensities[i])

            if jump_times is not None:
                combined_data = np.hstack((jump_times.reshape(-1 , 1), np.full((len(jump_times), 1), jump_sizes[i])))
                jump_data = np.vstack((jump_data, combined_data))

        return jump_data[jump_data[:, 0].argsort()]

    def get_poisson_jump_times(self, l: float):
        N = np.random.poisson(l*self.T, size=1)
        if not N:
            return None

        uniform_samples = np.random.uniform(size=N) * self.T # Unsorted for efficiency

        return uniform_samples

    def discretise_poisson_jump_data(self, poisson_jump_time_array):
        scaling_factor = self.increment_length

        jump_intervals = np.ceil(poisson_jump_time_array[:, 0] / scaling_factor)
        poisson_jump_time_array = np.hstack((poisson_jump_time_array, jump_intervals.reshape(-1, 1)))

        compressed_jump_time_array = self.compress_time_jump_data(poisson_jump_time_array)  # First column increments, second column discritised time

        poisson_values = np.empty(self.N)

        current_value = 0
        n = 1

        for i in range(len(compressed_jump_time_array)):
            while n < compressed_jump_time_array[i, 1]:  # Check how to deal with n since it is different in the r version
                poisson_values[n-1] = current_value
                n += 1

            current_value += compressed_jump_time_array[i, 0]

        while n <= self.N:
            poisson_values[n-1] = current_value
            n += 1

        return poisson_values


    @staticmethod
    def compress_time_jump_data(poisson_jump_time_array):
        unique_values, inverse_indices = np.unique(poisson_jump_time_array[:, 2], return_inverse=True)
        summed_values = np.zeros_like(unique_values, dtype=float)
        np.add.at(summed_values, inverse_indices, poisson_jump_time_array[:, 1])

        return np.column_stack((summed_values, unique_values))

    def simulate_brownian_motion(self, sig):
        interval_length = self.increment_length
        normal_samples = np.random.normal(loc=0, scale=np.sqrt(interval_length), size=self.N)

        return np.cumsum(normal_samples) * sig

    def simulate_small_jump_compensation(self, intensities, jump_sizes):
        values = np.arange(1, self.N+1) * self.increment_length

        jump_sizes[jump_sizes < 1] = 0
        combined_term = -1 * sum(jump_sizes * intensities)

        return values * combined_term

    def small_jump_brownian_motion_sim(self, sig): # TODO make sig an attribute
        #var = self.distribution.get_levy_interval_moment(2, -self.epsilon, self.epsilon)
        #var = 0.00001468341676331476
        var = 0
        #var = 0.00000007256188740861436*2
        sig = np.sqrt(sig**2 + var)

        simulated_values = self.simulate_brownian_motion(sig)
        return simulated_values

    def simulate_drift(self):
        return np.linspace(0 + self.increment_length, self.T, self.N) * self.distribution.levy_triplet.drift

    def run_simulation(self):
        intensities, jumps = self.compute_poisson_intensities_jumps()
        jump_data = self.simulate_jump_data(intensities, jumps)  # First column is jump time, second is jump size

        discritised_jump_data = self.discretise_poisson_jump_data(jump_data) # Same as before with 3rd col of discritised time

        compensated_jumps = self.simulate_small_jump_compensation(intensities, jumps)
        small_jump_brownian = self.small_jump_brownian_motion_sim(self.distribution.levy_triplet.sigma)  # Includes normal brownian term

        linear_drift = self.simulate_drift()

        self.simulated_values = linear_drift + discritised_jump_data + compensated_jumps + small_jump_brownian

        return self.simulated_values

    def plot_simulation(self, exponential=False):
        if self.simulated_values is None:
            raise ValueError("Simulated values are not initialized")

        if exponential:
            plt.plot(range(0, self.N), self.get_exponential_path())
        else:
            plt.plot(range(0, self.N), self.simulated_values)

        plt.show()

    def export_simulation_to_csv(self, filename):
        np.savetxt(filename, self.simulated_values, delimiter=',', fmt='%f', header='values')

    def get_exponential_path(self):
        return np.exp(self.simulated_values)


class MeixnerDistribution(Distribution):
    def __init__(self, m, a, b, d):
        pars = {"m": m, "a": a, "b": b, "d": d}
        levy_triplet = LevyTriplet(self.drift(m, a, b, d), 0, self.levy_measure)
        super().__init__(levy_triplet, pars)

    def levy_measure(self, x):
        numerator = self.pars_dict["d"] * np.exp(self.pars_dict["b"] * x / self.pars_dict["a"])
        denominator = x * np.sinh((np.pi * x) / self.pars_dict["a"])

        return numerator / denominator

    def drift(self, m, a, b, d):
        t1 = a * d * np.tan(b/2) + m  # I think m only affects the drift part

        def integrand(x):
            return np.sinh(b * x / a) / np.sinh(np.pi * x / a)

        intcomp = -0.3412704536142938#sp.integrate.quad(integrand, 1, np.inf)[0]
        print(intcomp)
        print("here")
        print(t1 - 2*d * intcomp)
        #return t1 - 2*d * intcomp
        return -1.645176e-05


class StableDistribution(Distribution):
    def __init__(self, a, b, g, d):
        pars = {"a": a, "b": b, "g": g, "d": d}  # Here we assume the L1 parametrisation
        levy_triplet = LevyTriplet(pars["d"], 0, self.levy_measure)

        super().__init__(levy_triplet, pars)
        self.pars_dict |= self.get_additional_parameters()

        print(self.pars_dict)

    def levy_measure(self, x):
        print(type(self))
        print(x)
        i = 1 if x < 0 else 0

        return (i * self.pars_dict["gamma_m"] / (abs(x)**self.pars_dict["a"]) +
                (1-i) * self.pars_dict["gamma_p"] / (abs(x)**self.pars_dict["a"]))

    def get_additional_parameters(self):
        z = np.cos(np.pi * self.pars_dict["a"]/2) * sp.special.gamma(1-self.pars_dict["a"])
        gamma_p = (1+self.pars_dict["b"]) * self.pars_dict["g"] / (2*z)
        gamma_m = (1-self.pars_dict["b"]) * self.pars_dict["g"] / (2*z)

        print(gamma_p, gamma_m)

        return {"z": z, "gamma_p": gamma_p, "gamma_m": gamma_m}

    def get_drift_term(self):
        def h(x):
            return x/(1+x**2)

        def change_drift_integrand(x):
            i = 1 if abs(x) < 1 else 0
            return h(x) - x*i * self.levy_measure(x)

        return sp.integrate.quad(change_drift_integrand, -np.inf, np.inf)[0]







# meixner_distribution = MeixnerDistribution(0, 0.01690119, 0, 0.35156535)
# meixner_simulation = JumpSimulation(meixner_distribution, 0.004, 100, 0.06, 10000, 10000)
# meixner_simulation.run_simulation()
# meixner_simulation.plot_simulation()

meixner_distribution = MeixnerDistribution(0, 0.01704519, -0.008522595000009334, 0.22649881)  # this is the 'correct' one
meixner_simulation = JumpSimulation(meixner_distribution, 0.00001, 10000, 3, 10000, 10000)
meixner_simulation.run_simulation()
meixner_simulation.plot_simulation()
meixner_simulation.plot_simulation(exponential=True)



#stable_distribution = StableDistribution(1.5841320038, 0.0055478219, 0.0039250669, -0.0001131727)
# stable_distribution = StableDistribution(1.5841320038, 0, 0.0039250669, 0)
#
# stable_simulation = JumpSimulation(stable_distribution, 0.002, 5000, 10, 100000, 100000)
# stable_simulation.run_simulation()
# stable_simulation.plot_simulation()
# stable_simulation.export_simulation_to_csv("gbpusd_simulation.csv")
#
# differences = np.diff(stable_simulation.simulated_values)
#
# print(max(abs(differences)))





# x_values = np.linspace(0.1, 1, 100)
# vectorized_levy_measure = np.vectorize(stable_distribution.levy_measure)
# y_values = vectorized_levy_measure(x_values)
# plt.plot(x_values, y_values)
# plt.show()

#L1 GBP/USD params: 1.5841320038  0.0055478219  0.0039250669 -0.0001131727




