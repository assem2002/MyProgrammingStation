#include <iostream>
#include<vector>
#include <stack>
#include <queue>

struct Node{
    int data;
    Node* next = nullptr;
    Node(int data):data(data){}
};
class linkedList{
private:
    Node* head = nullptr;
    Node* tail = nullptr;
    int length = 0;
    void deleteNode(Node* toBeDeleted){
        length--;
        delete toBeDeleted;
    }
    //This function delete the current node.
    void deleteNodeHelper(Node* prev ,Node* current){
        prev->next = current ->next;
        deleteNode(current);
    }
    //This function insert the node after the current node.(inserting infront or inback isn't provided)
    void insertHelper(Node* current,int value){
        Node* newNode = new Node(value);
        Node* nextOfCurrent = current->next;
        current->next = newNode;
        newNode->next = nextOfCurrent;
        length++;
    }
public:
    int getLength(){
        return length;
    }
    void insert_end(int value){
        Node* currentNode = new Node(value);
        if (head == nullptr){
            head = tail =  currentNode;
        }
        else{
            tail->next = currentNode;
            tail = currentNode;
        }
        length++;
    }
    void print(){
        for(Node* cur = head ; cur ; cur=cur->next)
            std::cout<<cur->data<<" ";
        std::cout<<"\n";
    }
    Node* get_nt(int idx) const{ // This function is 0-based indexing.
        if (idx >= length || idx < 0) return nullptr;
        Node* tempHead = head;
        while(idx--) tempHead = tempHead->next;
        return tempHead;
    }
    int search(int value){
        int cnt=0;
        for (Node* cur = head ; cur ;cur = cur->next){
            if (cur->data == value) return cnt;
            cnt++;
        }
        return -1;
    }
    int improvedSearch(int value){
        int idx =search(value);
        if (!idx || idx==-1) return idx;
        Node* first = get_nt(idx-1);
        //Swap operation
        int firstData = first->data;
        first->data = first->next->data;
        first->next->data = firstData;
        return idx;
    }
    ~linkedList(){
        while(head){
            Node* temp = head->next;
            delete head;
            head = temp;
        }
    }
    void insertFront(int value){
        if (length == 0) return insert_end(value);

        Node* newNode = new Node(value);
        newNode->next = head;
        head = newNode;
        length++;
    }
    int deleteFront(){
        if (!length) return -1;
        Node* toBeDeleted = head;
        head = head->next;
        if (!head) tail = nullptr;
        deleteNode(toBeDeleted);
        return 1;
    }
    int deleteBack(){
        if (length == 1) {
            return  deleteFront();
        }
        if (!length) return -1;

        Node* toBeDeleted = tail;
        tail = get_nt(length-2);
        tail->next = nullptr;
        deleteNode(toBeDeleted);
        return 1;
    }
    int deleteNth(int idx){ //idx here is 1-based indexing.
        if (idx> length) return -1;
        if (idx == length) return deleteBack();
        if(idx == 1) return deleteFront();
        Node* previousNode =  get_nt(idx-2);
        Node* toBeDeleted  = previousNode->next;
        previousNode->next=  previousNode->next->next;
        deleteNode(toBeDeleted);
        return 1;


    }
    Node* get_nth_fromBack(int idx){
        return get_nt(length-idx);
    }

    bool isSame(const linkedList &secondList){
        if (this->length != secondList.length) return 0;
        Node* first = this->head;
        Node* second = secondList.get_nt(0);
        for(int i = 0  ;i < length ; i++,first=first->next , second = second->next){
            if (first->data != second->data) return 0;
        }
        return 1;
    }
    bool deleteValue(int value){
        //I used a lazy approach :) .
        int toBeDeletedIdx = search(value);
        if(toBeDeletedIdx!=-1){
            deleteNth(toBeDeletedIdx+1);
            return true;
        }
        return false;
    }
    void swapPairs(){
        for (Node* tempHead = head; head && head->next ; head = head->next->next ){
            std::swap(head->data,head->next->data);
        }
    }
    void reverse(){
        if (length == 1) return;
        if (length ==2 ){
            tail->next = head ;
            head->next = nullptr;
            std::swap(head,tail);
            return;
        }
        //function is based on a window of three nodes
        Node* tailTemp = tail;
        tail = head;
        Node* prev = head;
        Node* next = head->next;
        Node* nextOfNext= head->next->next;
        while(next){
            next->next = prev;
            prev=next;
            next = nextOfNext;
            if (next) nextOfNext = next->next;
        }
        head = prev;
        tail->next = nullptr;

    }
    void deleteEvens(){
        if(length <=1) {
            deleteFront();
            return;
        }
        Node* temp = new Node(0);//Fake Node
        temp->next = head;
        head = head->next;
        for(Node* current =temp; (current && current->next);){
            deleteNodeHelper(current,current->next);
            current =current->next;
        }
        delete temp;
    }

    void insert_sorted(int value){
        if (length==0  || (length == 1 && value>= head->data)) return insert_end(value);
        if (length == 1 && value< head->data) return insertFront(value);
        Node fake = Node(-1);
        Node* fakeNode  = &fake;
        fakeNode->next =head;
        for (;fakeNode->next;fakeNode = fakeNode->next){
            if (fakeNode->next->data >= value){
                if (fakeNode->next ==head) return insertFront(value);
                return insertHelper(fakeNode,value);
            }
        }
        return insert_end(value);
    }
    void swapHeadTail(){
        if (length == 1)return;
        Node* beforeTail = get_nt(length-2);
        beforeTail->next = head;
        tail->next = head->next;
        head->next= nullptr;
        std::swap(head,tail);
    }
    void leftRotate(int steps){
        steps%=length;
        while(steps--){
            Node* currentNode = head;
            head = head->next;
            currentNode->next = nullptr;
            tail->next = currentNode;
            tail = currentNode;
        }

    }
    void removeDuplicates(){
        for(Node* currentNode = head;currentNode ; currentNode = currentNode->next){
            for(Node* searcherNode = currentNode->next ; searcherNode;){
                if(currentNode->data == searcherNode->data) {
                    deleteNodeHelper(currentNode, searcherNode);
                    searcherNode = currentNode->next;
                    continue;
                }
                searcherNode = searcherNode->next;
            }
        }
    }
    int removeLastOccurance(int key){
        Node* beforeTobeDeleted = nullptr;
        Node fakeNode = Node(-1);
        fakeNode.next = head;
        for(Node* currentNode =&fakeNode ; currentNode!=tail;currentNode = currentNode->next){
            if(currentNode->next->data == key) beforeTobeDeleted = currentNode;
        }
        if(beforeTobeDeleted->next == head) return deleteFront();
        if(beforeTobeDeleted->next == tail) return deleteBack();
        deleteNodeHelper(beforeTobeDeleted,beforeTobeDeleted->next);
        return 1 ;
    }
    void pushToBack(int key){
        Node fakeNode = Node(-1);
        fakeNode.next = head;
        int move=length;
        for(Node* currentNode = &fakeNode ; move ; --move){
            if (currentNode->next->data == key){
                if(currentNode->next == head){ //moving from the head part has a special deal.
                    leftRotate(1);
                    currentNode->next = head;
                }
                else{
                    tail->next = currentNode->next;
                    tail=currentNode->next;
                    currentNode->next =currentNode->next->next;
                    tail->next = nullptr;
                }

            }
            else
                currentNode = currentNode->next;
        }
    }
    int max(Node* currentNode = nullptr){
        if(length == 0) return 0;//handles empty list.
        if(currentNode == nullptr) return  max(head); //handles the initial value
        if(currentNode == tail) return currentNode->data; //base case
        return  std::max(max(currentNode->next),currentNode->data);
    }
    void arrangeOodEven(){
        if (length<=2) return;
        int step =1;
        bool tailIsOod = false;
        Node* addAfter = head;
        Node* fetcher =addAfter;
        while(true){
            for(int i = 0 ;i<step ; i++){
                fetcher= fetcher->next;
                if(!fetcher ||!(fetcher->next)) return; //finishes the code.
            }
            if(fetcher->next == tail) tailIsOod= true;
            Node* current = fetcher->next;
            fetcher->next = fetcher->next->next;
            current->next = addAfter->next;
            addAfter->next = current;
            addAfter = addAfter->next;
            if (tailIsOod) tail = fetcher;
            fetcher =addAfter;
            step++;

        }
    }
    void insertAlternating(const linkedList &secondList){
        Node* currentFirst =this->head;
        Node* currentSecond = secondList.head;
        Node* tempNode{};
        Node* tempNode2 {};
        int minlength = std::min(this->length , secondList.length);
        while(minlength--){
            tempNode = currentFirst->next;
            currentFirst->next = currentSecond;
            tempNode2 = currentSecond->next;
            if(!tempNode){
                this->tail =secondList.tail;
                break;
            }
            currentSecond->next =tempNode;
            currentFirst = tempNode;
            currentSecond = tempNode2;
        }


    }
    void addNumbers(const linkedList &secondList){
        bool carry= false;
        Node* currentFirst = head;
        Node* currentSecond = secondList.head;
        int minLength = std::min(this->length,secondList.length);
        while(minLength--){ //loops on the list of mine first and add numbers
            int res = currentFirst->data + currentSecond->data +carry;
            currentFirst->data = res%10;
            carry = (res>9);
            currentFirst=currentFirst->next;
            currentSecond= currentSecond->next;
        }
        if(this->length < secondList.length){//connects my list with the rest of the other one if this one is shorter.
            tail->next = currentSecond;
            currentFirst = currentSecond;
        }
        while(currentFirst){ // loops on the rest of the list whether it's the original one or the connected.
            int res = currentFirst->data + carry;
            currentFirst->data = res%10;
            carry = (res>9);
            if(!(currentFirst->next)) tail = currentFirst;
            currentFirst = currentFirst->next;
        }
        if(carry) insert_end(1); //if there's still a carry it adds a new whole node to the list.
    }
    void removeAllRepeated(){
        if (!length) return;
        int foundValue = head->data;
        bool deleteBoss = false;
        Node* foundNode = head;
        Node* prevToFound= nullptr;
        for(Node* currentNode = head->next ; currentNode ;currentNode = currentNode->next ){
            if(currentNode->data ==foundValue){
                deleteBoss = true;
                foundNode->next = currentNode->next;
                Node* toBeDeleted = currentNode;
                deleteNode(toBeDeleted);
            }
            else{
                if(deleteBoss){
                    if(foundNode == head){
                        head = head->next;
                        deleteNode(foundNode);
                        foundNode = head;
                        foundValue = foundNode->data;
                        deleteBoss=false;
                    }
                    else{
                        prevToFound->next = currentNode;
                        deleteNode(foundNode);
                        deleteBoss = false;
                        foundNode = currentNode;
                        foundValue = foundNode->data;

                    }
                }
                else{
                    prevToFound = foundNode;
                    foundNode = currentNode;
                    foundValue = foundNode->data;

                }
            }
        }
        if(deleteBoss){
            prevToFound->next = nullptr;
            tail =prevToFound;
            deleteNode(foundNode);
        }
        else
            tail = foundNode;

    }
    void reverseChain(int k ){ // if you don't get this solution so fuck it, it's hard to place comment on it.
        if (!length || k >length) return;
        int majorLoops = length/k;
        Node* startFrom = head;
        bool changeHead=true;
        Node* prevNode = nullptr;
        Node* oldConnector = nullptr;
        Node* currentConnector = nullptr;
        while(majorLoops--){
            currentConnector = startFrom;
            for(int i  = 0 ; i<k;i++ ){
                Node* currentNext = startFrom->next;
                startFrom->next = prevNode;
                prevNode = startFrom;
                startFrom =currentNext;
            }
            if(changeHead){
                changeHead = false;
                head = prevNode;
            }
            if(!oldConnector){
                oldConnector = currentConnector;
            }
            else{
                oldConnector->next = prevNode;
                oldConnector = currentConnector;
            }
        }
        currentConnector->next = startFrom;
        if(tail->next) {
            tail = currentConnector;
        }

    }

};


////////////////////////////////////////////////////////////////////////////////


class queueCircular{
private:
    int size;
    int indexFront=0;
    int indexEnd=0;
    int* arr;
//    int length=0;
    int getmod(int index,bool isFrontIndex){
        if (isFrontIndex) return  ((index%size)+size)%size;
        return ((index%size)+size)%size;
    }
public:
    queueCircular(int size):size(size){
        arr =  new int[size];
    }
    bool empty(){
        return (indexFront ==indexEnd);
    }
    bool full(){
        return ((getmod(indexEnd,false)== getmod(indexFront, true)) &&(!empty()));
    }
    int getRear(){
        if(empty()) return -1;
        return arr[getmod(indexEnd,false)-1];
    }

    bool enqueue_rear(int value){
        if(full()) return false;
        arr[getmod(indexEnd++,false)] =value;
        return true;
    }
    int dequeue_front(){
        if(empty()) return -1;
        return  arr[getmod(indexFront++,true)];

    }
    bool enqueue_front(int value){
        if(full())return false;
        arr[getmod(--indexFront, true)] =value;
        return true;
    }
    int dequeue_rear(){
        if(empty()) return -1;
        return getmod(--indexEnd,false);
    }
    ~queueCircular(){
        delete[] arr;
    }
    void display(){
        if(empty()) return;
        for(int tempFront = indexFront;tempFront<indexEnd ; ++tempFront){
            std::cout<<arr[((tempFront)%size+size)%(size)]<<" ";
        }
        std::cout<<'\n';
    }



};


class queueLinkedList{
private:
    linkedList list;
public:
    void enqueue(int value){
        list.insert_end(value);
    }
    int dequeue(){
        if(!list.getLength()) return -1;
        int temp  = list.get_nt(0)->data;
        list.deleteFront();
        return temp;


    }
};

class queueBasedStack{
private:
    queueCircular stk;
public:
    queueBasedStack(int size): stk(size){
    }
    bool push(int value){
        return  stk.enqueue_rear(value);
    }
    int top(){
        return stk.getRear();
    }
    int pop(){
        return stk.dequeue_rear();
    }
};

class priorityQueue{
private:
    queueCircular queue1;
    queueCircular queue2;
    queueCircular queue3;
public:
    priorityQueue(int size): queue1(size), queue2(size), queue3(size){
    }
    bool enqueue(int value,int priority){
        if(priority ==1) return queue1.enqueue_rear(value);
        else if(priority ==2) return queue2.enqueue_rear(value);
        return queue3.enqueue_rear(value);
    }
    int dequeue(){
        if(!queue3.empty())return queue3.dequeue_front();
        else if(!queue2.empty())return queue2.dequeue_front();
        return queue1.dequeue_front();
    }
    void display(){
        std::cout<<"priority #3:";queue3.display();
        std::cout<<"priority #2:";queue2.display();
        std::cout<<"priority #1:";queue1.display();

    }
    bool isempty(){
        return queue1.empty()&&queue2.empty()&&queue3.empty();
    }

};


class getlast{
private:
    int k;
    std::queue<int> q;
    int ret=0;
public:
    getlast(int k):k(k){}
    int next(int value){
        ret+=value;
        q.push(value);
        if(q.size()==k+1){ret-=q.front();q.pop();}
        return ret;
    }
};
int main() {
    int temp;
    getlast obj(4);
    while(std::cin>>temp){
        std::cout<<obj.next(temp);
    }
    return 0;
}
