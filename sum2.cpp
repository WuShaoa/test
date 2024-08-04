#include <iostream>
#include <algorithm>
#include <unordered_map>

using namespace std;


class Solution {
public:
    vector<int> twoSum(vector<int>& nums, int target) {
        unordered_map<int, int> htable;
        for(auto i = 0; i < nums.size(); i++){
            auto it = htable.find(target - nums[i]);
            if (it != htable.end())   return {it->second, i};
            htable[nums[i]] = i;
        }
        return {};    
    }
};