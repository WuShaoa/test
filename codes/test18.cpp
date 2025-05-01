/* count(number of a : a_k > a_i) where k < i */

#include <iostream>
#include <vector>
#include <algorithm>
#include <map>

using namespace std;

int main()
{
    int n;
    cin >> n;
    vector<int> a(n);
    for(int i = 0; i < n; ++i){
        cin >> a[i];
    }
    map<int, int> m;
    for(int i = 0; i < n; ++i){
        m[a[i]] = i;
    }
    int count = 0;
    for(int i = 0; i < n; ++i){
        for(int j = i+1; j < n; ++j){
            if(m[a[j]] < m[a[i]]){
                ++count;
            }
        }
    }
    cout << count << endl;
    return 0;
}