#include <iostream>
#include <vector>
#include <algorithm>
#include <valarray>
#include <stack>
#include<assert.h>
#include<queue>
using namespace  std;
struct Node{
    int data;
    Node* left{};
    Node* right{};
    Node(int value): data(value){}
};

class binaryTree{
private:
    void addPathHelper(std::vector<int> &numbers,std::string &path,int current , Node* parentNode){
        if(current>=path.size()) return;
        if(path[current] == 'L') {
            if (!(parentNode->left))
                parentNode->left = new Node(numbers[current + 1]);
            parentNode->left->data = numbers[current + 1];
            addPathHelper(numbers, path, current + 1, parentNode->left);
        }
        else{
            if(!(parentNode->right))
                parentNode->right = new Node(numbers[current+1]);
            parentNode->right->data = numbers[current+1];
            addPathHelper(numbers,path,current+1,parentNode->right);
        }

    }
    void printInOrderHelper(Node* current){
        if(!current) return;
        printInOrderHelper(current->left);
        std::cout<<current->data<<' ';
        printInOrderHelper(current->right);
    }
    void clearTree(Node* parentNode){
        if(!parentNode) return;
        clearTree(parentNode->left);
        clearTree(parentNode->right);
        delete parentNode;
    }
//    int getMaxValueHelper(Node* current){
//        if (!current) return -1;
//        int lefty = getMaxValueHelper(current->left);
//        int righty = getMaxValueHelper(current->right);
//        return std::max(std::max(current->data,righty),lefty);
//    }
    int getHeightHelper(Node* current){
        if(!current) return 0;
        int lefty = 1+getHeightHelper(current->left);
        int righty = 1+ getHeightHelper(current->right);
        return std::max(righty,lefty);
    }
    int getNodeCountHelper(Node* current){
        if(!current) return 0;
        std::cout<<"One more"<<current->data<<'\n';
        return (1+ getNodeCountHelper(current->left) + getNodeCountHelper(current->right));
    }
    bool isExistHelper(Node* current,int searchFor){
        if(!current) return false;
        return (current->data == searchFor) || isExistHelper(current->left,searchFor) || isExistHelper(current->right,searchFor);
    }
    int isPerfectHelper2(Node* current){
        if(!current) return 0;
        int lefty =  isPerfectHelper2(current->left);
        int  righty = isPerfectHelper2(current->right);
        if(righty ==-1 || lefty == -1) return -1;
        if( lefty == righty )return 1+righty;
        else return -1;
    }

public:

    Node* root{};
    void addPath(std::vector<int> &numbers,std::string &path){
        if(!root){
            Node* newNode = new Node(numbers[0]);
            root =newNode;
        }
        root->data = numbers[0];
        addPathHelper(numbers,path,0,root);
    }
    void printInOrder(){
        printInOrderHelper(root);
        std::cout<<'\n';
    }
//    int getMaxValue(){
//        getMaxValueHelper(root);
//    }
    int getHeight(){
        return  getHeightHelper(root)-1;
    }
    int getNodeCount(){
        return getNodeCountHelper(root);
    }
    bool isExist(int searchFor){
        isExistHelper(root,searchFor);
    }
    bool isPerfect1(){
        int nodes = getNodeCountHelper(root);
        int height = getHeightHelper(root); //return height plus 1.
        if(std::pow(2,height)-1 != nodes) return false;
        return true;
    }
    bool isPerfect2(){
        return !(isPerfectHelper2(root) == -1);
    }
    void inorderPrint_Iterative(){
        std::stack<Node*> s;
        s.push(root);
        while((!s.empty())){
            Node* temp = s.top();
            if(!temp){
                s.pop();
                temp = s.top();
                std::cout<<temp->data<<' ';
                s.pop();
                if(temp->right) s.push(temp->right);
            }
            else if(temp->left) {
                s.push(nullptr);
                s.push(temp->left);
            }
            else{
                std::cout<<temp->data<<' ';
                s.pop();
                if(temp->right) s.push(temp->right);
            }
        }
    }
    std::pair<int,int> getDiameterHelper(Node* current){ //return <max diameter , max height>.
        if(!current) return std::make_pair(0,0);
        std::pair<int,int> lefty = getDiameterHelper(current->left);
        std::pair<int,int> righty = getDiameterHelper(current->right);
        return std::make_pair(std::max(std::max(lefty.first,righty.first),lefty.second+righty.second+1),std::max(lefty.second+1,righty.second+1));

    }
    int getDiameter(){
        return getDiameterHelper(root).first -1;
    }
    void printPostOrderHelper(Node* current){
        if (!current) return;
        std::cout<<current->data<<' ';
        printPostOrderHelper(current->left);
        printPostOrderHelper(current->right);
    };
    void printPostOrder(){
        printPostOrderHelper(root);

    }
//    std::string print_inorder_expression_helper(Node* current){
//        if(!current->left &&!current->right) return std::string(1,current->data);
//        return '('+print_inorder_expression_helper(current->left)+current->data+ print_inorder_expression_helper(current->right) + ')';
//
//    }
//    void print_inorder_expression(){
//        std::cout<<print_inorder_expression_helper(root);
//    }
    void addPath(std::string s){
        assert(s.size()%2);
        std::stack<Node*> nodes;
        for(char &e : s){
            Node* newNode = new Node(e);
            if(e == '+' || e == '-' || e == '*' || e == '/' ||e == '^'){
                newNode->right = nodes.top();
                nodes.pop();
                newNode->left = nodes.top();
                nodes.pop();
            }
            nodes.push(newNode);
        }
        root = nodes.top();

    }
    void printBFS(){
        std::queue<Node*> q;
        q.push(root);
        while(!q.empty()){
            Node* temp = q.front();
            q.pop();
            if(temp->left) q.push(temp->left);
            if(temp->right)q.push(temp->right);
            std::cout<<(temp->data)<<' ';
        }
        std::cout<<'\n';

    }
    std::vector<std::string> data;
    void printBfsRecursiveHelper(Node* current, int level){
        if(!current){
          if(data.size()<=level)data.push_back("X");
          else data[level] = data[level]+ "X" ;
          return;
        }
        if(data.size()<=level)  data.push_back(std::to_string(current->data));
        else data[level] =  data[level]+" "+std::to_string(current->data);
        printBfsRecursiveHelper(current->left,level+1);
        printBfsRecursiveHelper(current->right,level+1);
    }
    bool printBfsRecursive(){
        printBfsRecursiveHelper(root , 0);
//        for(std::string level : data) std::cout<<level<<'\n';
//        std::cout<<'\n';
        std::string searchFor = data[data.size()-2];
        for(int i = 0 ; i<searchFor.size();++i){
            if(searchFor[i] == 'X' && i !=searchFor.size()-1) return false;
        }

        data.clear();
        return true;
    }
    //the most asshole function I've ever written before.
    Node* addUsingOrders(std::deque<int> inOrder , std::deque<int> &preOrder){
        if(inOrder.empty()) return nullptr;
        Node* newNode = new Node(preOrder.front());
        preOrder.pop_front();
        std::deque<int> left;
        int siz = inOrder.size();
        for(int i = 0  ; i<siz ;++i ){
            int temp  = inOrder.front();inOrder.pop_front();
            if(temp==newNode->data) break;
            left.push_back(temp);
        }
        newNode->left = addUsingOrders(left , preOrder);
        newNode->right = addUsingOrders(inOrder,preOrder);
        return root = newNode;
    }
    void printPreOrder_complete_helper(Node* current){
        std::cout<<current->data;
        if(current->right){
            std::cout<<"(";
            printPreOrder_complete_helper(current->left);
            std::cout<<") ";
        }
        if(current->left){
            std::cout<<"(";
            printPreOrder_complete_helper(current->right);
            std::cout<<")";
        }

    }
    void printPreOrder_complete(){
        printPreOrder_complete_helper(root);
    }
    Node* createTree_preorder_leafFlag(std::deque<std::pair<int,int>> &preOrder_dequeue){
        std::pair<int,int> current = preOrder_dequeue.front();preOrder_dequeue.pop_front();
        Node* newNode = new Node(current.first);
        if(!current.second){
            newNode->left = createTree_preorder_leafFlag(preOrder_dequeue);
            newNode->right = createTree_preorder_leafFlag(preOrder_dequeue);
        }
        return root = newNode;
    }
    std::string left;
    std::string right;
    void isSymmetric(Node* current,std::string &placeIn){
        if(!current){
            left+="-1 ";
            return;
        }
        placeIn+=std::to_string(current->data)+' ';
        isSymmetric(current->left,placeIn);
        isSymmetric(current->right,placeIn);
    }

    struct whatever{
        bool operator()( const Node* v1, const Node* v2) const {
            return  v1->data > v2->data;
        }
    };

    std::string solve(Node* root1){
        std::priority_queue<Node*,std::vector<Node*>,whatever> p1;
        p1.push(root1);
        std::string res;
        while(!p1.empty()){
            Node* current = p1.top();p1.pop();
            res+=std::to_string(current->data)+" ";
            if(!current->left)res+="-1 ";
            else p1.push(current->left);
            if(!current->right)res+="-1 ";
            else p1.push(current->right);
        }
        return res;
    }

    Node* createBSTHelper(std::deque<int> preorder){
        if(preorder.empty()) return nullptr;
        deque<int> myownLeft;
        Node* newNode  = new Node(preorder.front());
        preorder.pop_front();
        while( !preorder.empty() && preorder.front() < newNode->data ) {
            myownLeft.push_back(preorder.front());
            preorder.pop_front();
        }
        newNode->left = createBSTHelper(myownLeft);
        newNode->right = createBSTHelper(preorder);
        return newNode;
    }
    void createBST(std::deque<int> preorder){
        root = createBSTHelper(preorder);
    }
    void creatFromLevelOrder(deque<int> levelOrder){
        if(levelOrder.empty()) return;
        queue<pair<Node*,pair<int,int>>> q;
        Node* newNode = new Node(levelOrder.front());
        levelOrder.pop_front();
        q.push({newNode,{0,10000}});
        root =newNode;
        while(!q.empty()){
            Node* currentNode =q.front().first;
            int min=q.front().second.first,max =q.front().second.second;
            q.pop();
            if( !levelOrder.empty() && levelOrder.front()>min&& levelOrder.front()<currentNode->data){
                Node* newNodeleft = new Node(levelOrder.front());
                levelOrder.pop_front();
                currentNode->left = newNodeleft;
                q.push({newNodeleft,{min,currentNode->data}});
            }
            if( !levelOrder.empty() && levelOrder.front()>currentNode->data && levelOrder.front()<max){
                Node* newNoderight = new Node(levelOrder.front());
                levelOrder.pop_front();
                currentNode->right = newNoderight;
                q.push({newNoderight,{currentNode->data,max}});
            }
        }
    }
    Node* findMin(Node* current){
        if(current->left){
            return findMin(current->left);
        }
        return current;
    }
    Node* findmax(Node* current){
        if(current->right) return findmax(current->right);
        return current;
    }



    Node* deleteNodeHelper(int target,Node* current) {
        if(target < current->data) current->left = deleteNodeHelper(target,current->left);
        else if (target> current->data)  current->right = deleteNodeHelper(target,current->right);
        else{
            Node* temp =current;
            if(!current->left && !current->right) current = nullptr;
            else if(!current->left && current->right)
                current = current->right;
            else if (!current->right && current->left) current=current->left;
            else{
                Node* minimumNode = findMin(current->right);
                current->data = minimumNode->data;
                current->left  = deleteNodeHelper(minimumNode->data,current->left);
                temp= nullptr;
            }

            if (temp) delete temp;
        }
            return current;



    }
    void deleteNode(int target){
        deleteNodeHelper(target,root);
    }
    ~binaryTree(){
        std::cout<<"This other bitch has worked again !";
        clearTree(root);
    }
    void print_level_order_traversal(){
        priority_queue<Node*> first;
        priority_queue<Node*> second;
        priority_queue<Node*>* current = &first;
        priority_queue<Node*>* spare = &second;
        first.push(root);

        while(!first.empty() || !second.empty()){
            Node* cur = current->top();
            current->pop();
            cout<<cur->data<<' ';
            if(cur->left) spare->push(cur->left);
            if(cur->right) spare->push(cur->right);
            if(current->empty()){
                swap(current,spare);
                cout<<'\n';
            }


        }

    }
};


int main() {
    std::vector<int> v{25,3,5};
    std::string p  ="LL";
    std::vector<int> v1{25,7,6};
    std::string p1  ="RR";
    std::vector<int> v2{25,7,30};
    std::string p2  ="RL";
//    std::vector<int> v2{1,3,6};
//    std::string p2 ="RL";
//    std::vector<int> v3{2,13,99};
//    std::string p3  ="RL";
//    std::vector<int> v{1,2};
//    std::string p  ="L";
//    std::vector<int> v1{1,2,5};
//    std::string p1  ="LR";
//    std::vector<int> v2{1,3};
//    std::string p2 ="R";
//    std::vector<int> v3{2,13,99};
//    std::string p3  ="RL";
//    std::deque<int> a{1,2,4,7,8,5,9,3,6,10};
//    std::deque<int> b {7,4,8,2,5,9,1,3,10,6};


    binaryTree obj;
    obj.addPath(v,p);
    obj.addPath(v1,p1);
    obj.addPath(v2,p2);
//    obj.addUsingOrders(b,a);
//    std::deque<std::pair<int,int>> temp {{1,0},{2,0},{4,0},{6,1},{7,1},
//                                         {5,0},{8,1},{9,1},{3,0},{10,0},{12,1},
//                                         {13,1},{11,0},{14,1},{15,1}};
//    obj.createTree_preorder_leafFlag(temp);
//    obj.printPreOrder_complete();
//    obj.addPath(v,p);
//    obj.addPath(v1,p1);
//    obj.addPath(v2,p2);
//    obj.addPath(v3,p3);
//    std::cout<<obj.root->data<<'\n';
//    std::cout<<obj.root->left->data<<'\n';
//    std::cout<<obj.root->left->left->data<<'\n';
//    std::cout<<obj.root->left->left->right->data<<'\n';
//    obj.inorderPrint_Iterative();
//    std::cout<<'\n';
//    std::cout<<obj.getDiameter();
//    obj.printInOrder();
//    std::cout<<obj.getMaxValue()<<'\n';
//    std::cout<<obj.getHeight()<<'\n';
//    std::cout<<obj.getNodeCount()<<"\n";
//    std::cout<<obj.isPerfect2()<<'\n';
//    std::cout<<obj.printBfsRecursive();
//    deque<int> a {50,20,60,15,45,70,35,73};
//    obj.creatFromLevelOrder(a);
//    obj.printInOrder();
//    obj.deleteNode(20);
//    obj.printInOrder();
    obj.print_level_order_traversal();
    return 0;
}
