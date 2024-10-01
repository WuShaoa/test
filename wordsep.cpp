/*s 和多个单词组成的WordDict
判断是否可以由WordDict中的一个或多个单词构成*/
#include <iostream>
#include <string>
#include <vector>
#include <unordered_set>
using namespace std;

bool wordBreak(string s, vector<string>& wordDict) {
    unordered_set<string> dict(wordDict.begin(), wordDict.end());
    vector<bool> dp(s.size() + 1, false);
    dp[0] = true;
    for (int i = 1; i <= s.size(); i++) {
        for (int j = 0; j < i; j++) {
            if (dp[j] && dict.count(s.substr(j, i - j))) { // j之前的字符串可以由wordDict中的单词组成，且j到i之间的字符串是wordDict中的单词
                dp[i] = true; // i之前的字符串可以由wordDict中的单词组成
                break;
            }
        }
    }
    return dp[s.size()];
}

int main() {
    string s = "threedogs";
    vector<string> wordDict = {"three", "dog", "s"};
    cout << wordBreak(s, wordDict) << endl;
    return 0;
}
