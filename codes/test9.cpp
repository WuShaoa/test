/*

opt. min ||S|| where S is a multi_value set
init. S = arr, ||S|| = n
after opration loop:
    op1. s_ /subset S 
    ||s_|| = m
    max(s_) - min(s_) <= k
    do S = S - {min(s_)} 
*/

//第二题 
#include <iostream>
#include <vector>
#include <set>
#include <algorithm>

using namespace std;

int main() {
    int n, m, k;
    cin >> n >> m >> k;
    vector<int> arr(n);
    for(int i = 0; i < n; i++){
        cin >> arr[i];
    }
    multiset<int> s;
    for(int i = 0; i < m; i++){
        s.insert(arr[i]);
    }

    int ans = s.size();
    for(int i = m; i < n; i++){
        s.insert(arr[i]);
        s.erase(s.find(arr[i - m]));
        if(*s.rbegin() - *s.begin() <= k){
            ans = min(ans, (int)s.size());
        }
    }

    cout << ans << endl;
    return 0;
}
