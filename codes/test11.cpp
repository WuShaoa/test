#include <unordered_map>
#include <iostream>

using namespace std;

class Solution {
public:
    void print_map(const std::unordered_map<char, int>& map)
    {
        for (auto iter : map)
        {
            cout << iter.first << " " << iter.second << endl;
        }
    }
    inline bool is_same(const std::unordered_map<char, int>& map1, const std::unordered_map<char, int>& map2)
    {
        for (auto iter1 : map1)
        {
            auto iter2 = map2.find(iter1.first);
            if (iter2 == map2.end())
            {
                if(iter1.second == 0) continue; // not communicative
                return false;
            }
            else
            {
                if (iter1.second != iter2->second)
                {
                    return false;
                }
            }
        }
    
        return true;
    }

    bool checkInclusion(string s1, string s2) {
        unordered_map<char, int> guard, dict;
        for(auto c : s1){
            guard[c]++;
        }

        int j = 0;
        for (int i = 0; i < s2.length(); ++i){
            dict[s2[i]]++;

            if(i >= s1.length()-1){
                cout << "i: " << i << " j: " << j << endl;
                cout << "dict: " << endl;
                print_map(dict);
                cout << "--------" << endl;
                cout << "guard: " << endl;
                print_map(guard);
                cout << "----------------" << endl;
                if(is_same(dict, guard)){
                    return true;
                }
                dict[s2[j]]--;
                ++j;
            }
        }

        return false;
    }
};

int main(){
    Solution s;
    cout << s.checkInclusion("adc", "dcda") << endl;
    // cout << s.checkInclusion("ab", "eidboaoo") << endl;
    return 0;
}