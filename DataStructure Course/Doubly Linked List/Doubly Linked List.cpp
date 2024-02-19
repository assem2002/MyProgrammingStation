#include<iostream>
#include <vector>
#include <bits/stdc++.h>
using namespace std;
struct Node{
    int data{};
    Node* next{};
    Node* prev{};
    Node(int value):data(value){}
    void set(Node* prev_,Node* next_){
        this->prev = prev_;
        this->next = next_;
    }
};

class linkedList{ //This should be Doubly Linked list.
private:
    Node* head{};
    Node* tail {};
    int length=0;
    void link (Node* first,Node* second){
        first->next = second;
        second->prev = first;
    }
    Node* deleteNode(Node* theNode){
        Node* prev = theNode->prev;
        if(theNode->prev) theNode->prev->next = theNode->next;
        if(theNode->next) theNode->next->prev = theNode->prev;
        if (!theNode->next && !theNode->prev ) head = tail = nullptr; //handles the case where there's only one node
        length--;
        debug_remove_node(theNode);
        delete theNode;
        return prev;

    }
    //Start debuging
    vector<Node*> debug_data;
    void debug_add_node(Node* node) {
        debug_data.push_back(node);
    }
    void debug_remove_node(Node* node) {
        auto it = std::find(debug_data.begin(), debug_data.end(), node);
        if (it == debug_data.end())
            cout << "Node does not exist\n";
        else
            debug_data.erase(it);
    }
public:
    void debug_print_address() {
        for (Node* cur = head; cur; cur = cur->next)
            cout << cur << "," << cur->data << "\t";
        cout << "\n";
    }

    void debug_print_node(Node* node, bool is_seperate = false) {
        if (is_seperate)
            cout << "Sep: ";
        if (node == nullptr) {
            cout << "nullptr\n";
            return;
        }
        cout << node->data << " ";
        if (node->next == nullptr)
            cout << "X ";
        else
            cout << node->next->data << " ";

        if (node == head)
            cout << "head\n";
        else if (node == tail)
            cout << "tail\n";
        else
            cout << "\n";
    }
    void debug_print_list(string msg = "") {
        if (msg != "")
            cout << msg << "\n";
        for (int i = 0; i < (int) debug_data.size(); ++i)
            debug_print_node(debug_data[i]);
        cout << "************\n"<<flush;
    }

    string debug_to_string() {
        if (length == 0)
            return "";
        ostringstream oss;
        for (Node* cur = head; cur; cur = cur->next) {
            oss << cur->data;
            if (cur->next)
                oss << " ";
        }
        return oss.str();
    }

    void debug_verify_data_integrity() {
        if (length == 0) {
            assert(head == nullptr);
            assert(tail == nullptr);
        } else {
            assert(head != nullptr);
            assert(tail != nullptr);
            if (length == 1)
                assert(head == tail);
            else
                assert(head != tail);
            assert(!tail->next);
        }
        int len = 0;
        for (Node* cur = head; cur; cur = cur->next, len++)
            assert(len < 10000);	// Consider infinite cycle?
        assert(length == len);
        assert(length == (int)debug_data.size());
    }
    //end debuging
    void insertEnd(int value) {
        length++;
        Node* newNode = new Node(value);
        debug_add_node(newNode);
        if(!head){
            head = tail = newNode;
        }
        else{
            link(tail,newNode);
            tail = newNode;
        }
        debug_verify_data_integrity();
    }
    void print(){
        for(Node* currentNode = head; currentNode;currentNode=currentNode->next){
            std::cout<<currentNode->data<<" ";
        }
        std::cout<<std::endl;
    }
    void insertFront(int value){
        length++;
        Node* newNode = new Node(value);
        debug_add_node(newNode);
        if (!head){
            head = tail =newNode;
        }
        else{
            link(newNode,head);
            head = newNode;
        }
        debug_verify_data_integrity();
    }
    void insertSorted(int value){
        if (!head ||(head->data)>=value) return insertFront(value);
        for(Node* currentNode = head;currentNode;currentNode=currentNode->next){
            if (currentNode->data >= value){
                length++;
                Node* newNode = new Node(value);
                debug_add_node(newNode);
                currentNode->prev->next = newNode;
                newNode->prev = currentNode->prev;
                link(newNode,currentNode);
                debug_verify_data_integrity();
                return;
            }
        }
        insertEnd(value);
        debug_verify_data_integrity();
    }
    void deleteFront(){
        if (!head) return;
        Node* tobeDeleted = head;
        head = head->next;
        deleteNode(tobeDeleted);
        debug_verify_data_integrity();
    }
    void deleteBack(){
        if(!head) return;
        Node* tobeDeleted = tail;
        tail = tail->prev;
        deleteNode(tobeDeleted);
        debug_verify_data_integrity();
    }
    void deleteByKey(int key){
        for (Node* currentNode= head;currentNode;currentNode=currentNode->next){
            if (currentNode->data == key){
                if (currentNode==head) deleteFront();
                else if (currentNode == tail) deleteBack();
                else deleteNode(currentNode);
                debug_verify_data_integrity();
                return;
            }
        }
    }
    void deleteAllByKey(int key){
        int initialLength = length;
        for(Node* currentNode = head; initialLength;initialLength--){
            Node* nextNode = currentNode->next;
            if (currentNode->data ==key){
                if(currentNode == head) deleteFront();
                else if (currentNode == tail) deleteBack();
                else deleteNode(currentNode);
            }
            currentNode = nextNode;
        }
    }
    void deleteEvenPositions(){
        Node* nextNode = nullptr;
        for(Node* currentNode =head->next ; currentNode ; currentNode = nextNode){
            if(currentNode ==tail){
                nextNode = nullptr;
                deleteBack();
            }
            else {
                nextNode = currentNode ->next->next;
                deleteNode(currentNode);
            }
        }
    }
    void deleteOddPositions(){
        if(length ==1) return deleteBack();
        Node* nextNode = nullptr;
        for(Node* currentNode = head; currentNode; currentNode = nextNode->next){
            nextNode= currentNode->next;
            if (currentNode == head) deleteFront();
            else if (currentNode == tail) return deleteBack();
            else deleteNode(currentNode);
        }
    }
    bool palindrome(){
        Node* fromLeft =head;
        Node* fromRight =tail;
        for ( int moves =0 ; length/2>moves; ++moves,fromLeft = fromLeft->next,fromRight =fromRight->prev)
            if(fromLeft->data != fromRight->data) return false;
        return true;
    }
    int findMiddle1(){
        Node* fromLeft =head;
        Node* fromRight =tail;
        if (head == nullptr) return -1;
        for ( ;;fromLeft = fromLeft->next,fromRight =fromRight->prev){
            if(fromLeft == fromRight) return fromLeft->data;
            if(fromLeft->next == fromRight) return fromRight->data;
        }
        debug_verify_data_integrity();
    }
    int findMiddle2(Node* currentNode = nullptr,int depth =0){
        int res=-1;
        if(currentNode == nullptr){
            if (!depth) res = findMiddle2(head,depth+1);
            else
                return depth-1;
        }
        else res = findMiddle2(currentNode->next,depth+1);

        if( (res/2)+1 ==depth){
//            std::cout<<"here the res "<<res;
            std::cout<<currentNode->data<<'\n';
            return res+1;
        }
        return res;
    }
    void swapper1(int k){
        if ((k >length)|| length == 1 || (length%2== 1 && (length/2)+1 == k)) return;
        bool swapTailHead = (k==length)||(k==1);
        if (k>(length/2)) k = length-k+1;
        if (length == 2){
            head = tail;
            tail = head->prev;
            return;
        }

        Node* first = head;
        Node* second = tail;
        k--;
        while (k--){
            first = first->next;
            second = second->prev;
        }
        if (swapTailHead){
            second->next = first->next;
            first->prev = second->prev;
            first->next->prev = second;
            second->prev->next =first;
            first->next = second->prev =nullptr;
            tail =first;
            head = second;
            debug_verify_data_integrity();
            return;
        }
        first->prev->next =second;
        first->next->prev  = second;
        second->prev->next = first;
        second->next->prev = first;
        Node* temp1=first->prev;
        Node* temp2 = first->next;
        first->prev = second->prev;
        first->next = second->next;
        second->prev = temp1;
        second->next =temp2;
        debug_verify_data_integrity();

    }
    void swapper2(int k){
        Node* first=head;
        Node* second=tail;
        if (head == nullptr) return;
        if (head ==tail) return;
        for (int move = 0;move!=k-1;move++){
            first = first->next;
            second= second->prev;
        }
        if (first->prev ==second) std::swap(first,second);
        if (first ==head || second== head){
            first =head;
            second = tail;
            second->next = first->next;
            first->prev = second->prev;
            first->next->prev = second;
            second->prev->next =first;
            first->next = second->prev =nullptr;
            tail =first;
            head = second;
            debug_verify_data_integrity();
            return;

        }
        else if (first == second) return;
        else if (first->next == second){
            first->next = second->next;
            second->prev = first->prev;
            first->prev->next = second;
            first->prev =second;
            second->next = first;
        }
        else{
            first->prev->next = second;
            first->next->prev  = second;
            second->prev->next = first;
            second->next->prev = first;
            Node* temp1 = first->prev;
            Node* temp2 = first->next;
            first->prev = second->prev;
            first->next = second->next;
            second->prev = temp1;
            second->next =temp2;

            debug_verify_data_integrity();
        }

    }
    void reverse(){
        Node* prevNode = nullptr;
        for(Node* currentNode =tail; currentNode;currentNode =prevNode){
            prevNode = currentNode->prev;
            std::swap(currentNode->next,currentNode->prev);
        }
        std::swap(head,tail);
        debug_verify_data_integrity();
    }
    void mergeTwoSortedLists (linkedList &secondList){
        Node* secondListCurrentNode = secondList.head;
        Node* secondListCurrentNext= nullptr;
        Node* firstListCurrentNode =this->head;
        if (!this->head){
            head = secondList.head;
            tail = secondList.tail;
            return;
        }
        for(;secondListCurrentNode && firstListCurrentNode;){
            secondListCurrentNext =secondListCurrentNode->next;
            if(firstListCurrentNode->data >= secondListCurrentNode->data){
                ++length;
                debug_add_node(secondListCurrentNode);
                if(firstListCurrentNode ==this->head){ //special condition if a node gets to be the new head.
                    firstListCurrentNode->prev =secondListCurrentNode;
                    secondListCurrentNode->next =firstListCurrentNode;
                    secondListCurrentNode->prev = nullptr;
                    head =secondListCurrentNode;
                }
                else{ //the normal node insertion
                    firstListCurrentNode->prev->next = secondListCurrentNode;
                    secondListCurrentNode->prev = firstListCurrentNode->prev;
                    firstListCurrentNode->prev = secondListCurrentNode;
                    secondListCurrentNode->next = firstListCurrentNode;
                }
                secondListCurrentNode = secondListCurrentNext;
            }
            else firstListCurrentNode =firstListCurrentNode->next; // if we don't find a match we move to the next node.
        }
        firstListCurrentNode =this->tail;
        for(;secondListCurrentNode;secondListCurrentNode = secondListCurrentNode->next,firstListCurrentNode =firstListCurrentNode->next){
            ++length;
            debug_add_node(secondListCurrentNode);
            firstListCurrentNode->next =secondListCurrentNode;
            secondListCurrentNode->prev = firstListCurrentNode;
        }
        tail =firstListCurrentNode;
        debug_verify_data_integrity();
    }
};


int main(){
    linkedList myLinkedList;
//    myLinkedList.insertEnd(10);
//    myLinkedList.insertEnd(20);
//    myLinkedList.insertEnd(30);
//    myLinkedList.insertEnd(40);
//    myLinkedList.insertEnd(50);
    linkedList myOtherLinkedList;
    myOtherLinkedList.insertEnd(9);
//    myOtherLinkedList.insertEnd(17);
//    myOtherLinkedList.insertEnd(22);
//    myOtherLinkedList.insertEnd(24);
//    myOtherLinkedList.insertEnd(35);
//    myOtherLinkedList.insertEnd(70);
//    myOtherLinkedList.insertEnd(80);
//    myLinkedList.insertEnd(6);
//    myLinkedList.insertEnd(7);
//    myLinkedList.insertEnd(8);
//    myLinkedList.insertFront(10);
//    myLinkedList.insertFront(5);
    myLinkedList.print();
//    myLinkedList.swapper2(1);
//    myLinkedList.deleteBack();
    myLinkedList.mergeTwoSortedLists(myOtherLinkedList);
    myLinkedList.print();


}