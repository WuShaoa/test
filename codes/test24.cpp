#include <bits/stdc++.h>
using namespace std;

class Solution {
public:
    void printdp(vector<vector<int>> dp){
        for(auto cv : dp){
            for(auto c : cv){
                cout << c << " ";
            }
            cout << endl;
        }
    }
    
    string longestPalindrome(string s) {
        int n = s.length();
        vector<vector<int>> dp(n, vector<int>(n, 0));
        for(auto i = 0; i < n; ++i) dp[i][i] = 1;
        for(auto l = 1; l <= n; ++l){
            for(auto i = 0; i < n - l; ++i){
                auto j = i + l;
                if(s[i] == s[j]) {
                    dp[i][j] = ((i+1) > (j-1)) ? 1 : dp[i+1][j-1];  
                }
            }
        }
        int maxl = 0;
        int maxi = 1;
        int i = 0;
        for(auto cv : dp){
            int tempacc = 0;
            for(auto it = cv.rbegin(); it != cv.rend(); ++it){
                if(*it == 1){
                    tempacc = (cv.rend() - it) - i;    
                    break;
                }
            }

            if(tempacc > maxl){
                maxl = tempacc;
                maxi = i;
            }
            ++i;
        }
        
        printdp(dp);

        return s.substr(maxi, maxl);
    }
};

int main(){
    Solution s;
    auto ans = s.longestPalindrome("abba");
    cout << ans << endl;
}