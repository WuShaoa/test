#include <vector>
#include <iostream>

using namespace std;

class ListNode {
public:
    int val;
    ListNode* next;

    ListNode(int val) {
        this->val = val;
        this->next = nullptr;
    }
    ListNode(int val, ListNode* next) {
        this->val = val;
        this->next = next;
    }
};

class Solution {
public:
    ListNode* removeNthFromEnd(ListNode* head, int n) {
        vector<ListNode*> v(n+1, nullptr);
        auto curr = head;
        int count = 0;
        while(curr != nullptr){
            auto temp = curr;
            v[count % (n+1)] = temp;
            count++;
            curr = curr->next;
        }
        auto index = (count) % (n+1); //(count - (n + 1)) === count % (n+1); count-1 : 倒数第一个节点，(count - (n + 1)) 倒数第 n+1 个节点
        auto next = (count+1) % (n+1);
        
        if(count == n){ // corner case: 没有倒数第 n+1 个节点
            return head->next; //删除头节点
        } else {
            v[index]->next = v[next]->next;
        }

        return head;
    }
};



void print(ListNode* head){
    ListNode* curr = head;
    while(curr != nullptr){
        cout << curr->val << " ";
        curr = curr->next;
    }
    cout << endl;
}

int main(){
    Solution s;
    ListNode* head = new ListNode(1);
    head->next = new ListNode(2);

    auto curr = s.removeNthFromEnd(head, 2);
    print(curr);

    return 0;
}