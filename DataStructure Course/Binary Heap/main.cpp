#include <iostream>
#include <assert.h>
#include <bits/stdc++.h>
using namespace std;
class minheap{
private:
    int size=0;
    int cnt=0;
    int* arr;
    int parent(int i){
        return (i-1)/2;
    }
    int leftChild(int i ){
        int res = i*2+1;
        if(res>=cnt) return -1;
        return res;
    }
    int rightChild(int i){
        int res = 2*(i+1);
        if(res>=cnt) return -1;
        return res;
    }
    void heapify(int ind){
        if(ind ==0) return;
        int parentIndex = parent(ind);
        if(arr[ind]<arr[parentIndex]){
            std::swap(arr[ind],arr[parentIndex]);
            heapify(parentIndex);
        }
    }
    void heapify_down(int index){
        int leftChildIndex = leftChild(index);
        int rightChildIndex = rightChild(index);
        if(leftChildIndex == -1) return; // for sure there's no right child.
        if(rightChildIndex!=-1 && arr[rightChildIndex]<=arr[leftChildIndex]){
            leftChildIndex=rightChildIndex;
        }
        if(arr[index]>=arr[leftChildIndex]){
            swap(arr[index],arr[leftChildIndex]);
            heapify_down(leftChildIndex);
        }
    }
public:
    minheap(int size):size(size){
        arr = new int [size];
    }
    bool empty(){
        if(cnt) return false;
        return true;
    }
    int top(){
        assert(!empty());
        return arr[0];
    }
    bool full(){
        return (cnt == size);

    }
    bool push(int x){
        if(full())return false;
        arr[cnt] =x;
        heapify(cnt);
        cnt++;
        return true;
    }
    int pop(){
        int toReturn =top();
        arr[0] = arr[--cnt];
        heapify_down(0);
        return toReturn;
    }
    void print_less_than(int val,int indFirst=0){
        int current=pop();
        while(current<=val){
            if(current==val) {
                push(current);
                return;
            }
            cout<<current<<" ";
            current=pop();
        }

    }
    bool is_heap(int* p,int n){
        for(int i=0 ; i<n;++i){
            int parent = p[i];
            int myright = rightChild(parent);
            int myleft = leftChild(parent);
            if((parent>myright) || parent>myleft) return false;
        }
        return true;
    }
    void heapsort(int* p ,int sz){
        int* arrtemp = arr;
        int tempsize = size;
        int tempcnt = cnt;
        arr = p;
        size = sz;
        cnt =sz;
        for(int i=sz-1;i>=0;--i){
            heapify(i);
        }
        for(int i=sz-1;i>=0;--i){
            int ret = pop();
            arr[cnt]=ret;
        }
        arr =arrtemp;
        size =tempsize;
        cnt =tempcnt;
    }
};

class maxheap{
private:
public:
    minheap myMinHeap;
    maxheap(int size): myMinHeap(size){}

    bool push(int k){
        return myMinHeap.push(-k);
    }
    int pop(){
        return myMinHeap.pop()*-1;
    }
    int top(){
        return myMinHeap.top()*-1;
    }
    int ksmallest=0;
    int kthsmallest(int pushed){
        ksmallest= pushed;
    }

    int next(int num){
        int min;
        if(myMinHeap.empty()) min=-1;
        else min=top();
        if(ksmallest<=0){
            if(num>min) return min;
            else{
                pop();
                push(num);
                return top();
            }
        }
        else{
            push(num);
            min =-1;
        }
        ksmallest--;
        return -1;
    }
};

class priorityQ{
private:
    int sz =1000;
    maxheap obj;
    vector<queue<int>> v;
public:
    priorityQ(int a):obj(sz),v(sz){}
    bool enqueue(int value,int prio){
        obj.push(prio);
        v[prio].push(value);
    }
    int dequeue(){
        int queueindex = obj.pop();
        int ret = v[queueindex].front();
        v[queueindex].pop();
        return ret;
    }


};



int main() {
    int arr[14]{9,8,7,6,5,4,10,8,3,3,5,5,15};
    maxheap obj(100);
////    obj.kthsmallest(4);
//////     obj(100);
//    for(int i=0;i<13;++i){
//        obj.push(arr[i]);
////        cout<<obj.next(arr[i])<<"\n";
//    }

    for(int i =0;i< 13;++i){
        cout<<obj.pop()<<" ";
    }

    std::cout << "Hello, World!" << std::endl;
//    obj.print_less_than(9);
    return 0;
}
