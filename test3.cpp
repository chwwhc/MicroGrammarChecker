/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 * };
 */
class Solution {
public:
    unordered_map<int, int> tbl;
    int ans = 0;

    int pathSum(TreeNode* root, int targetSum) {
        if(!root) return ans;
        tbl[0] = 1;
        dfs(root, 0, targetSum);
        return ans;
    }
    void dfs(TreeNode* node, int curSum, int targetSum){
        if(!node) return;
        curSum += node->val;
        ans += tbl[curSum - targetSum];
        tbl[curSum] += 1;
        dfs(node->left, curSum, targetSum);
        dfs(node->right, curSum, targetSum);
        tbl[curSum] -= 1;
    }
};