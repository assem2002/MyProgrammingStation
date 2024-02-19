#include <iostream>
#include <bits/stdc++.h>
using namespace std;

class node{
private:
public:
    int data{};
    int height{};
    node* left{};
    node* right{};
    int ch_height(node* thenode){
        if(!thenode)
            return -1; //for me, i don't know how it would work.
        return thenode->height;
    }
    node(int value):data(value){}
    int update_height(){
        return height = 1 + max(ch_height(left), ch_height(right));
    }
    int balanceFactor(){
        return ch_height(left) - ch_height(right);
    }

};


class avlTree{
private:
    node* root{};
    node* right_rotation(node* q){
        node* p = q->left;
        q->left = p->right;
        p->right = q;
        q->update_height();
        p->update_height();
        return p;
    }
    node* left_rotate(node* p){
        node* q = p->right;
        p->right = q->left;
        q->left =p;
        q->update_height();
        p->update_height();
        return q;
    }
    node* balance(node* node){
        if(node->balanceFactor() == 2){
            if(node->left->balanceFactor() == -1)
                node->left = left_rotate(node->left);
            node = right_rotation(node);
        }
        else if(node->balanceFactor() == -2){
            if(node->right->balanceFactor() == 1)
                node->right = right_rotation(node->right);
            node = left_rotate(node);
        }
        return  node;
    }
    node* insert_node(int value,node* current){
        if(value< current->data){
            if(!current->left)
                current->left = new node(value);
            else
                current->left = insert_node(value,current->left);
        }
        else if(value> current->data){
            if(!current->right)
                current->right = new node(value);
            else current->right = insert_node(value,current->right);
        }
        current->update_height();
        return balance(current);
    }


public:
    void level_order_traversal() {
        if (!root)
            return;

        cout << "******************\n";
        queue<node*> nodes_queue;
        nodes_queue.push(root);

        int level = 0;
        while (!nodes_queue.empty()) {
            int sz = nodes_queue.size();

            cout << "Level " << level << ": ";
            while (sz--) {
                node* cur = nodes_queue.front();
                nodes_queue.pop();

                cout << cur->data << " ";
                if (cur->left)
                    nodes_queue.push(cur->left);
                if (cur->right)
                    nodes_queue.push(cur->right);
            }
            level++;
            cout << "\n";
        }
    }
    avlTree(){}
    void insert(int value){
        if(!root)
            root =new node(value);
        root = insert_node(value,root);
    }
    node* getmin(node* n){
        if(!n->left) return n;
        return  getmin(n->left);
    }

    node* delete_node_helper(node* current, int value){
        if(!current) return nullptr;
        if(value < current->data)
            current->left = delete_node_helper(current->left,value);
        else if (value > current->data)
            current->right = delete_node_helper(current->right,value);
        else {
            if(!current->left && current->right){
                node* keepThis = current->right;
                delete current;
                return keepThis;
            }
            else if (current->left && !current->right){
                node* keepThis = current->left;
                delete current;
                return keepThis;
            }
            else{
                node* deletedNode = getmin(current->right);
                current->data = deletedNode->data;
                current->right = delete_node_helper(current->right,deletedNode->data);
            }
        }
        if(current){
            current->update_height();
            balance(current);
        }
        return current;
    }
    node* delete_node(int value){
        root = delete_node_helper(root,value);
    }
    pair<bool,int> lower_bound_helper(int value,node* current,bool found=false,int min=-1){
        if(!current)
            return {found,min};
        if(value<current->data){
            found=true;
            min= current->data;
            lower_bound_helper(value,current->left,found,min);
        }
        else if(value> current->data)
            lower_bound_helper(value,current->right,found,min);
        else
            return {true,current->data};
    }

    pair<bool,int> lower_bound(int value){
        lower_bound_helper(value,root);
    }

    pair<bool,int> upper_bound_helper(int value,node* current,bool found=false,int min=-1){
        if(!current)
            return {found,min};
        if(value<current->data){
            found=true;
            min= current->data;
            upper_bound_helper(value,current->left,found,min);
        }
        else if(value> current->data)
            upper_bound_helper(value,current->right,found,min);
        else{
            if(current->right) return {true,current->right->data};
            return {found,min};
        }

    }

    pair<bool,int> upper_bound(int value){
        upper_bound_helper(value,root);
    }

    int count_inversions_helper(int value,node* current,int counter=0){
        if(! current)return counter;
        if(value<current->data)
            return count_inversions_helper(value,current->left,counter++);
        else if(value> current->data)
            return count_inversions_helper(value,current->right,counter);
        else
            return counter;
    }
    int count_inversions(int value){
        return  count_inversions_helper(value,root);
    }

    int countminNodes(int h,int k){
        vector<int> mem;
        for(int i=0;i<=h;++i){
            int res=1;
            if(i-1 >=0) res+=mem[i-1];
            if((i-1-k)>=0) res+=mem[i-1-k];
            mem.push_back(res);
        }
        return  mem[h];
    }
    int countminNodesrec(int h,int k){
        if(h<0)return  0;
        return 1+ countminNodesrec(h-1-k,k)+ countminNodesrec(h-1,k);
    }



};


void f1() {
    avlTree tree;
    for (int i = 0; i <= 5; ++i) {	// degenerate
        tree.insert(i);
        tree.level_order_traversal();
    }
}

class pr{
    avlTree a;
public:

};


int main() {
//    f1();
    avlTree t;
//    cout<<t.countminNodes(4,1);
//    cout<<t.countminNodes(1,1);
//    cout<<t.countminNodesrec(4,1);
    cout<<("abcd"<"abcd");
    std::cout << "Hello, World!" << std::endl;
    return 0;
}
