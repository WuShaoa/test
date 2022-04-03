import math
import numpy as np
def fib(n):
    return (((1 + math.sqrt(5)) / 2)**n - ((1 - math.sqrt(5)) / 2)**n) / math.sqrt(5)

# for i in range(1,100):
#     print(fib(i))
