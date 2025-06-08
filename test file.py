# from matplotlib import pyplot as plt
# import numpy as np
#
#
# def levy_measure(x):
#     i = 1 if x > 0 else 0
#     a=1.7
#     gp=3
#     gm=1
#
#     return (i * gp / (abs(x) ** (a + 1)) +
#             (1 - i) * gm / (abs(x) ** (a + 1)))
#
#
# x = np.linspace(-1, 1, 200)
#
# v = np.vectorize(levy_measure)
# y = v(x)
#
# print(y)

# plt.plot(x, np.log(y))
# plt.show()


import numpy as np
pn = 1
print("i = 2")
print(pn)

for i in range(2, 10):
    print(f"i = {i+1}")
    pn = pn*(1-((np.pi/2) * (i-1)/(i+1)))
    print(pn)
