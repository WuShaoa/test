import numpy as np
# huawei 面试题1 
# 上游系统u's，共同调用核心服务，最大cnt个调用: sum(u's)<=cnt，不然给出limit，使得u_sorted[j:]=limit，求最大的limit
# 提示：limit有一个可变上界，由sum(u[0:i], (len-i)*limit) <= cnt决定，控制i，limit的最大值为最大上界

# 示例代码如下：
u = input("input nums: ")
u = [int(n) for n in u.split()]
cnt = int(input("count: "))

print(u, cnt)
length = len(u)

np.array(u).sort()

max = 0
for i in range(length):
    candidate = (cnt - np.sum(u[:i])) / (length - i)
    max = candidate if max < candidate else max

print(max)