from abc import ABC, abstractmethod
import numpy as np
from scipy.fft import fft
from matplotlib import pyplot, pyplot as plt


class Distribution(ABC):
    @abstractmethod
    def characteristic_function(self, u: complex):
        pass


class MeixnerDistribution:
    def __init__(self, m, a, b, d):
        self.m = m
        self.a = a
        self.b = b
        self.d = d

    def characteristic_function(self, u):
        numerator = np.cos(self.b/2)
        denominator = np.cosh((self.a*u - complex(0, -self.b))/2)

        result = (numerator / denominator) ** (2 * self.d) * np.exp(1j * u * self.m)
        #print(result)

        return result


class TestDistribution(Distribution):
    def characteristic_function(self, u):
        return u


class CharacteristicOpPricing:
    def __init__(self, rfr: float, initial_asset_value: float, T: float, eta, N): # some of the eta, N are redundant
        self.rfr = rfr  # The risk-free rate
        self.initial_asset_value = initial_asset_value
        self.T = T
        self.eta = eta
        self.N = N  # Should be a power of 2 for efficiency
        self.lam = 2 * np.pi/(self.eta * self.N)
        print("lam", self.lam)
        self.b = 1/2 * self.N * self.lam  # The upper bound of log strike prices/forward price (not inclusive)
        print("b", self.b)

    def get_forward_price(self) -> float:
        return self.initial_asset_value * np.exp(self.rfr * self.T)

    @staticmethod
    def calculate_c_hat(distribution: Distribution, alpha: float, u: float):
        numerator = distribution.characteristic_function(u - 1j * (1 + alpha))
        denominator = alpha ** 2 + alpha - u ** 2 - 1j * (2 * alpha + 1) * u

        result = numerator / denominator
        plt.plot(u, np.real(result))
        plt.show()
        plt.plot(u, np.imag(result))
        plt.show()
        print(result.shape)

        return numerator/denominator

    def get_sequence(self, distribution: Distribution, alpha: float):
        def dirac_delta(k: np.ndarray):
            return (np.array(k) == 0).astype(int)

        js = np.arange(1, self.N+1)
        ujs = (js-1) * self.eta

        c_hats = self.calculate_c_hat(distribution, alpha, ujs)

        simpsons_part = (self.eta / 3) * (3 + (-1)**(js) - dirac_delta(js-1))
        exponential_part = np.exp(1j * ujs*self.b)

        return c_hats * simpsons_part * exponential_part

    def calculate_option_prices(self, distribution: Distribution, alpha: float):
        sequence = self.get_sequence(distribution, alpha)
        plt.plot(np.real(sequence))
        plt.title("Real sequence part")
        plt.show()
        plt.plot(np.imag(sequence))
        plt.title("Imag sequence part")
        plt.show()
        print(sequence)
        fourier_sequence = fft(sequence)
        print("fourier")
        print(fourier_sequence)

        kappa_values = np.linspace(-self.b, self.b, self.N, endpoint=False)

        plt.plot(kappa_values)
        plt.title("kappa values")
        plt.show()

        prices = self.initial_asset_value * np.exp(-kappa_values*alpha) * fourier_sequence / np.pi  # Cancelled forward price and e term to get initial value

        return prices


distribution = MeixnerDistribution(0, 0.01704519, -0.008522595000009334, 0.22649881)

option_pricing_fw = CharacteristicOpPricing(0, 100, 365, 0.25, 4096)
prices = option_pricing_fw.calculate_option_prices(distribution, 0.25)

option_pricing_fw.calculate_c_hat(distribution, 0.25, np.linspace(-100, 100, 1000))

print(prices)

plt.plot(np.real(prices))
plt.show()
plt.plot(np.imag(prices))
plt.show()