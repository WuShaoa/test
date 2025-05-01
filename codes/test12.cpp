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
            if(dict.find(s[head]) != dict.end() && dict[s[head]] >= tail) {
                tail = dict[s[head]] + 1;
            }
            dict[s[head]] = head;
            max_len = max(max_len, head-tail+1);
        }

        return max_len;
    }
};

int main(){
    Solution s;

    cout << s.lengthOfLongestSubstring("tmmzuxt") << endl;

    return 0;
}