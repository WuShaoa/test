#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    vector<vector<int>> fourSum(vector<int> nums, int target) {
        auto n = nums.size();   
        if(n < 4) return {};

        sort(nums.begin(), nums.end());
        
        unordered_map<long, int> hmap;
        vector<vector<int>> ret = {};
        for(int i = 0; i < n-3; ++i){
            if(i > 0 && nums[i] == nums[i-1]) continue;
            for(int j = i+1; j < n-2; ++j){
                auto psum = (long)nums[i] + (long)nums[j]; 
                if(psum + (long)nums[j+1] + (long)nums[j+2] > (long)target) break;
                if(psum + (long)nums[n-2] + (long)nums[n-1] < (long)target) continue;
                if(j > i+1 && nums[j] == nums[j-1]) continue;
                hmap.clear();
				pair<int,int> lastpair;
                bool firsttime = true;
                for(int k = j+1; k < n; ++k){
                    //if(k > j+2 && nums[k] == nums[k-1]) continue;
                    //if(nums[i] + nums[k] + nums[j] + nums[hmap[nums[k]]] > target) break; //hmap means a past k
                    if(hmap.find((long)nums[k]) != hmap.end()){
                        if(!firsttime && lastpair.first == nums[k] && lastpair.second == nums[hmap[nums[k]]]) continue;
                        ret.push_back({nums[i], nums[j], nums[k], nums[hmap[(long)nums[k]]]});
						lastpair.first = nums[k];
                        lastpair.second = nums[hmap[(long)nums[k]]];
                        if(firsttime) firsttime = false; //初始化的pair不会被考虑
					}
                    else{
                        hmap[(long)target - (long)nums[k] - (long)nums[i] - (long)nums[j]] = k;
                    }
                }
            }
        }
        return ret;
    }
};