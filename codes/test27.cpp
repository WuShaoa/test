#include <iostream>
#include <algorithm>
#include <vector>
#include <map>

using namespace std;

struct TreeNode {
    int val;
    TreeNode *left;
    TreeNode *right;
    TreeNode() : val(0), left(nullptr), right(nullptr) {}
    TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
    TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
};


class Solution {
    map<int, int> indInPost;
public:
    int findIndex(vector<int>& vec, int val){
        for(int i = 0; i < vec.size(); ++i){
            if(vec[i] == val){
                return i;
            }
        }
        return -1;
    }

    void printVec(vector<int>& vec){
        for(int val : vec){
            cout << val << " ";
        }
        cout << endl;
    }
    TreeNode* buildTreeSub(vector<int>& inorder, vector<int>& postorder) { 

        cout << "aaa " << inorder.size() << " " << postorder.size() << endl;
        if (inorder.size() == 0){
            return nullptr;
        }
        
        int head_val =  postorder[postorder.size() - 1];
        TreeNode* root = new TreeNode(head_val);

        if(inorder.size() == 1){
            return root;
        }

        int inorder_index = findIndex(inorder, head_val);

        cout << "inorder_index: " << inorder_index << endl;

        int left_post_index = 0;
        printVec(inorder);
        printVec(postorder);
        vector<int> left_inorder(inorder.begin(), inorder.begin() + inorder_index);
        vector<int> right_inorder(inorder.begin() + inorder_index + 1, inorder.end());
        
        // for(int val : left_inorder){
        //     if(indInPost[val] > left_post_index){
        //         left_post_index = indInPost[val];
        //     }
        // }
        // cout << "left_post_index: " << left_post_index << endl;
        
        // vector<int> left_postorder(postorder.begin(), postorder.begin() + left_post_index);
        // vector<int> right_postorder(postorder.begin() + left_post_index, postorder.end() - 1);
        vector<int> left_postorder(postorder.begin(), postorder.begin() + left_inorder.size());
        vector<int> right_postorder(postorder.begin() + left_inorder.size(), postorder.end() - 1);
        printVec(left_inorder);
        printVec(left_postorder);
        printVec(right_inorder);
        printVec(right_postorder);
        root->left = buildTreeSub(left_inorder, left_postorder);
        root->right = buildTreeSub(right_inorder, right_postorder);

        return root;
    }
    TreeNode* buildTree(vector<int>& inorder, vector<int>& postorder) { 
        for (int i = 0; i < inorder.size(); ++i){
            indInPost[postorder[i]] = i;
        }
        return buildTreeSub(inorder, postorder);
    }
};

int main() {
    Solution s;
    vector<int> inorder = {9,3,15,20,7};
    vector<int> postorder = {9,15,7,20,3};
    TreeNode* root = s.buildTree(inorder, postorder);
    return 0;
}