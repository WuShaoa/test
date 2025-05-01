/*
a_i in a[N]
operation (a_i, a_i+1) -> a_i, a_i+1 = -a_i, -a_i+1 
after several operation
such that opt: max sum a_i

简单一维二元元胞自动机
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>

using namespace std;

int main(){ // not correct
    int N;
    cin >> N;
    vector<int> a(N);
    for(auto i = 0; i < N; ++i){
        cin >> a[i];
    }
    int sum = accumulate(a.begin(), a.end(), 0);
    if(N == 1){
        cout << a[0] << endl;
    }else if(N == 2){
        cout << max(a[0], a[1]) << endl;
    }else{
        int maxsum = 0;
        for(auto i = 0; i < N; ++i){
            for(auto j = i+1; j < N; ++j){
                vector<int> tempa = a;
                for(auto k = i; k <= j; ++k){
                    tempa[k] = -tempa[k];
                }
                maxsum = max(maxsum, accumulate(tempa.begin(), tempa.end(), 0));
            }
        }
        cout << maxsum << endl;
    }
    return 0;
}