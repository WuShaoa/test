#include <unordered_map>
#include <iostream>
#include <string>

using namespace std;

class Solution {
public:
    int lengthOfLongestSubstring(string s) {
        unordered_map<char, int> dict;
        int max_len = 0;
        int tail = 0;
        for(int head = 0; head < s.length(); ++head){
            if(dict[s[head]] > 0) {
                for(int k = head - 1; k >= tail; --k){
                    if(s[k] == s[head]){
                        tail = k + 1;
                        break;
                    }
                }
                max_len = max(max_len, head-tail);
            } else {
                dict[s[head]]++;
                max_len = max(max_len, head-tail+1);
            }   
        }

        return max_len;
    }
};
int main(){
    Solution s;

    cout << s.lengthOfLongestSubstring("bbbbbb") << endl;
    cout << s.lengthOfLongestSubstring("abcabcaaa") << endl;

    return 0;
}