#include <iostream>
#include <vector>
#include <set>
#include <algorithm>
#include <map>
using namespace std;

class Solution {
public:
    /**
     * Note: 类名、方法名、参数名已经指定，请勿修改
     *
     * 
     * 返回的是一个list，可能是空，可能是包括两个index元素的list。
     * @param nums int整型 vector 
     * @param target int整型  
     * @return int整型vector
     */
    vector<int> two_sum(vector<int>& nums, int target) {
        // write code here
        map<int, int> dict;
        vector<int> ans;
        for(int i = 0; i < nums.size(); ++i){
            if(dict.count(nums[i]) != 0){
                ans.push_back(dict[nums[i]]);
                ans.push_back(i);
                break;
            } else {
                dict[target - nums[i]] = i;
            }
        }

        return ans;
    }
};
