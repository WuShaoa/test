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

# huawei 面试题2
# 输入：节点数、边数、连接关系、屏蔽节点数，屏蔽节点 输出：由根通向最短叶节点的路径
tree = {}
old_tree = {}
edges = []
node_num = int(input())
edge_num = int(input())
for i in range(edge_num):
    edges.append(input().split())
block_num = int(input())
edges.sort(key=lambda x: int(x[0]))
for edge in edges:
    flag = True
    if edge[0] == '0': 
        tree["->".join(edge)] = 1
        flag = False
    else:    
        for key in list(tree.keys()):
            if key[-1] == edge[0]:
                cnt = tree.pop(key)            
                tree[key + "->" + edge[1]] = cnt + 1
                old_tree[key] = cnt
                flag = False
                break
    if flag:
        for key in list(old_tree.keys()):
            if key[-1] == edge[0]:           
                tree[key + "->" + edge[1]] = old_tree[key] + 1
                break
for i in range(block_num):
    blocked_node = input()
    for key in list(tree.keys()):
        if key.find(blocked_node) != -1:
            tree.pop(key)
min, min_way = -1, "NULL"
for key in tree.keys():
    if min == -1 or min > tree[key]: min, min_way = tree[key], key
print(min_way)
