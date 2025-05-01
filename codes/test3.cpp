/************ 
 *  美团第五场第二题 计算树的相似节点数
 *  定义树由1为根节点，相似节点为有相同子节点数量的节点
 *  计算相似节点对的个数
 * */ 

#include <iostream>
#include <vector>
#include <unordered_map>
#include <numeric>
#include <algorithm>
using namespace std;

constexpr long long fact(long long n){
    long long x = 1;
    for(long long i = 1; i <= n; ++i){
        x *= i;
    }
    return x; 
}

constexpr long long comb(long long n, long long m){
    if (n < m) return 0;
    return (fact(n)/(fact(n-m) * fact(m)));
}

void clean_bd(int k, vector<vector<int>>& tree){
    for(int i = 0; i < tree.size(); ++i){
         if(tree[k][i] == 1){
                tree[i][k] = 0;
                clean_bd(i, tree);
            }
        }
}

int main() {
    int T;
    cin >> T;
    vector<long long> results;
    unordered_map<int, long long> dict;

    for(auto i = 0; i < T; ++i) {
        long long result = 0;
        int n, u, v;
        cin >> n;
        vector<vector<int>> tree(n, vector<int>(n, 0));
        

        for(auto i = 0; i < n-1; ++i) {
            cin >> u >> v;
            
            tree[u-1][v-1] = 1;
            tree[v-1][u-1] = 1; 
        }
        //from 1 as root, clean all back directed egdes
        clean_bd(0, tree);

        for(auto node : tree){
            dict[accumulate(node.begin(), node.end(), 0)]++;
        }

        for(auto p : dict){
            result += comb(p.second, 2);
        }

        results.push_back(result);
        dict.clear();
    }

    for(auto r : results){
        cout << r <<endl;
    }

    return 0;
}
// 64 位输出请用 printf("%lld")