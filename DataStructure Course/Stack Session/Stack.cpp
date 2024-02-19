#include <iostream>
#include <bits/stdc++.h>
class sstack{
private:
    int actualSize;
    int* arr;
    void expand(){
        int* tempPointer = new int[actualSize*2];
        for(int i= 0; i<actualSize;++i) tempPointer[i] =arr[i];
        delete[] arr;
        actualSize*=2;
        arr = tempPointer;
    }
public:
    int fakeSize=-1; //represents index of the last element.
    bool empty(){
        return (fakeSize == -1);
    }
    sstack(int actualSize):actualSize(actualSize){
        arr = new int[actualSize];
    }
    void push(int value){
        if (++fakeSize == actualSize) expand();
        arr[fakeSize] = value;
    }
    int& top() {
//        if (empty()) return -1;
        return arr[fakeSize];
    }
    int pop(){
        if (empty()) return -1;
        return arr[fakeSize--];

    }
    int insertAtBottom(int value,int indexMove =0,bool isNew =1,int stoper = -1){
        if (fakeSize-indexMove == stoper){
            fakeSize+=isNew;
            return arr[stoper+1] = value;
        }
        arr[fakeSize-indexMove+1] = arr[fakeSize-indexMove];
        return insertAtBottom(value,indexMove+1,isNew,stoper);
    }
    void reverse(int stoper =-1){
        if (stoper == fakeSize-1) return;
        insertAtBottom(top(),1,0,stoper);
        reverse(stoper+1);
    }
    void print(){
        for(int i=0 ; i<= fakeSize ;i++){
            std::cout<<arr[i]<<' ';
        }
        std::cout<<'\n';
    }
    ~sstack(){
        delete[]arr;
    }
};

sstack mm(10);

int factorial(){
    if (mm.top()==2) return 1;
    mm.push(mm.top()-1);
    return mm.top()*factorial()*mm.pop();
}

class Solution {
public:
    int scoreOfParentheses(std::string s) {
        int total=0;
        sstack myS(s.size());
        for (int i = 0 ; i< s.size(); i++){
            if (i>0 && s[i] =='(' && s[i-1] =='('){
                myS.top()+=1;
                myS.push(1);
            }
            else if (s[i] == '('){
                myS.push(1);
            }
        }
        int mainTotal=0;
        bool lastisOne=false;
        while (!myS.empty()){
            if (myS.pop() ==1) {
                if (lastisOne){
                    mainTotal+=total;
                    total =0;
                }
                else {
                    lastisOne =true;
                }
                total+=1;
            }
//            else if(mainTotal != 0) {
//                total = 2*total +mainTotal(mainTotal+total);
//                mainTotal =0;
//                lastisOne =false;
//            }
            else{
                lastisOne =false;
                total*=2;
            }
        }
        return total +mainTotal;

    }
};


int main(){
//    mm.push(5);
//    std::cout<<factorial();
//    sstack myStack(10);
//    myStack.push(4);
//    myStack.push(3);
//    myStack.push(2);
//    myStack.push(1);
//    myStack.pop();
//    myStack.pop();
//    std::cout<<myStack.empty();
//    myStack.insertAtBottom(10);
//    myStack.reverse();
//    myStack.print();
    Solution obj;
    std::cout<<obj.scoreOfParentheses("(())()");
//    std::vector<int>a{-2,-1,1,2};
//    auto x = obj.asteroidCollision(a);
//    for (int dd :x){
//        std::cout<<dd<<' ';
//    }



}