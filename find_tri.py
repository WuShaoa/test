'''
携程在线机试题：

游游拿到了一个字符矩阵，她想知道有多少个三角形满足以下条件:
三角形的三个顶点分别是y、o、u字符。三角形为直角三角形，且两个直角边一个为水平、另个为垂直。

输入描述
第一行输入两个正整数n,m，用空格隔开，代表矩阵的行数和列数。接下来的n行，每行输入一个长度为n的字符串，代表游游拿到的矩阵。1<n,m<1000
'''

import numpy as np

def find_straight_tri(lines, alp_set):
    count = 0
    for line in lines:
        # print("line", line)
        for i, a in enumerate(line):
            if a in alp_set:
                # print('alp_set', alp_set)
                new_alp_set = alp_set.copy()
                new_alp_set.remove(a)
                index = find_straight_tri_in_row(line, new_alp_set, i)
                for j, b in index: # choosed a line-row           
                    nnew_alp_set = new_alp_set.copy()
                    nnew_alp_set.remove(b)
                    # print('new_alp_set', new_alp_set)
                    for c in lines[:,i]:
                        # print('c', c)
                        if c in nnew_alp_set:
                            # print("cached!")
                            count += 1
                    for c in lines[:,j]:
                        # print('c', c)
                        if c in nnew_alp_set:
                            # print("cached!")
                            count += 1
    return count

def find_straight_tri_in_row(line, alp_set, i):
    index = []
    for j, a in enumerate(line):
        if j > i:
            if a in alp_set:
                index.append([j, a])
    # print('index', index)
    return index        
                
if __name__ == '__main__':
    size = input()
    n, m = [int(i) for i in size.split(' ')]
    rows = []
    for i in range(n):
        rows.append(list(input()))
    alp_set = {'y','o','u'}
    
    print(find_straight_tri(np.array(rows), alp_set))



