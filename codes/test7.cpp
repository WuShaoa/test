#include <iostream>
#include <vector>

using namespace std;

class Node {
    public:
    int val;
    Node* next;

    Node(int val) {
        this->val = val;
        this->next = nullptr;
    }
    Node(int val, Node* next) {
        this->val = val;
        this->next = next;
    }
};

Node* reverse(Node* head) {
    Node* prev = nullptr;
    Node* curr = head;
    Node* next = nullptr;

    while(curr != nullptr){
        next = curr->next;
        curr->next = prev;
        prev = curr;
        curr = next;
    }

    return prev;
}

void print(Node* head){
    Node* curr = head;
    while(curr != nullptr){
        cout << curr->val << " ";
        curr = curr->next;
    }
    cout << endl;
}

int main(){
    Node* head = new Node(1);
    head->next = new Node(2);
    head->next->next = new Node(3);
    head->next->next->next = new Node(4);
    head->next->next->next->next = new Node(5);

    Node* curr = head;
    curr = reverse(curr); // 蠢
    print(curr);

    // delete
    while(curr != nullptr){
        Node* temp = curr;
        curr = curr->next;
        delete temp;
    }

    return 0;
}