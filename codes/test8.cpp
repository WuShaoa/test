#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main(){
    int n;
    cin >> n;

    vector<int> nums(n);
    vector<int> dp(n, -1);
    vector<int> prev(n, -1);
    vector<int> max_seq;

    for(int i = 0; i < n; i++){
        cin >> nums[i];
    }
    
    for(int i = 0; i < n; i++){
        dp[i] = 1;
        for(int j = 0; j < n; j++){
            if(nums[j] < nums[i]){
                dp[i] = max(dp[i], dp[j] + 1);
                if(dp[i] == dp[j] + 1){
                    prev[i] = j;
                }
            }
        }
    }

    int index = max_element(dp.begin(), dp.end()) - dp.begin();
    while(index != -1){
        max_seq.push_back(nums[index]);
        index = prev[index];
    }

    reverse(max_seq.begin(), max_seq.end());
    for(int i = 0; i < max_seq.size(); i++){
        cout << max_seq[i] << " ";
    }

    return 0;
}