#include <vector>
#include <iostream>
#include <string>

using namespace std;

/* ldistance of string x(i),y(j)
                 / max(i,j) if min(i,j) = 0
ldiatance(i,j) = |     / ldistance(i-1, j) + 1 (delete at the end of x or pend at the end of y)
                 | min | ldistance(i-1, j-1) + 1(x(i)!=y(j)) (substitute one of the x or y) else
                 \     \ ldistance(i, j-1) + 1 (delete at the end of y or pend at the end of x)
*/

int ldistance(const string& s1, const string& s2){
    vector<vector<int>> dp(s1.length()+1, vector<int>(s2.length()+1, 0));
    for (int i = 0; i <= s1.length(); i++){
        for (int j = 0; j <= s2.length(); j++){
            if(min(i, j) == 0) dp[i][j] = max(j,j);
            else {
                int ind = s1[i-1] == s2[j-1] ? 0 : 1; 
                dp[i][j] = min(dp[i-1][j] + 1, min(dp[i][j-1] + 1, dp[i-1][j-1] + ind));
            }
        }
    }
    return dp[s1.length()][s2.length()];
}

int main(){
    string s1, s2;
    cin >> s1 >> s2;
    cout << ldistance(s1, s2) << endl;
}