#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<vector<int>> fourSum(vector<int>& nums, int target) {
        // 双指针法
        int n = nums.size();
        vector<vector<int>> result;
        sort(nums.begin(), nums.end());
        for (int i = 0; i < n - 3; i++) {
            if (i > 0 && nums[i] == nums[i - 1]) continue; //去重
            if (nums[i] > max(0, target)) break; //special case: (分类讨论) 如果target为负数，nums[i] > 0，直接break; 如果target为正数，nums[i] > target，直接break
            for (int j = i + 1; j < n - 2; j++) {
                long x = nums[i] + nums[j];
                if (x + nums[j + 1] + nums[j + 2] > target) break; //special case
                if (x + nums[n - 2] + nums[n - 1] < target) continue; //special case
                if (j > i + 1 && nums[j] == nums[j - 1]) continue; //去重
                int left = j + 1, right = n - 1;
                while (left < right) {
                    long s = x + nums[left] + nums[right];
                    if (s < target) left++;
                    else if (s > target) right--;
                    else {
                        result.push_back({nums[i], nums[j], nums[left], nums[right]});
                        for (++left; left < right && nums[left] == nums[left - 1]; left++); //去重
                        for (--right; right > left && nums[right] == nums[right + 1]; right--); //去重
                    }
                }
            }
        }
        return result;
    }
};