#include <iostream>
#include <cassert>

class Vector{
private:
    int size = 0;
    int *arr = nullptr;
    int capacity = 10;
    void enlarge(){
        capacity*=2;
        int *arrTemp = new int[capacity];
        for(int i = 0 ; i < size ; i++){
            arrTemp[i] = arr[i];
        }
        std::swap(arr,arrTemp);
        delete[] arrTemp;
    }
public:
    Vector(int size):size(size){
        if (size<0) size = 1;
        arr = new int[capacity];
    }
    ~Vector(){
        delete[] arr;
    }
    void push_back(int value){
        if (size==capacity) enlarge();
        arr[size++] = value;
    }
    void insert(int idx , int value){
        push_back(value); //instead if checking the capacity validation myself
        for(int i =size-1 ; i >idx;i--)
            arr[i] =arr[i-1];
        arr[idx] =value;
    }
    void rotate_right(){
        int lastValue = arr[size-1];
        for (int i = size-1 ;i>0;i--){
            arr[i] = arr[i-1];
        }
        arr[0]= lastValue;
    }
    void rotate_right(int times){
        times%=size;
        int tempArray[times];
        int index = 0;
        for(int i = size-times ;  i <size ; i++)tempArray[index++] =arr[i];
        for(int i=size-times-1 ;i>=0;i--) arr[i+times] = arr[i];
        for(int i= 0 ;i<times;i++)arr[i] =tempArray[i];
        //you could've just used the rotate_right method.
    }

    void rotate_left(){
        int firstValue = arr[0];
        for (int i = 0 ;i <size-1 ; i++){
            arr[i] =arr[i+1];
        }
        arr[size-1] = firstValue;
    }
    int pop(int idx){
        int deleted =arr[idx];
        for(int i= idx ; i <size-1 ; i++)
            arr[i] = arr[i+1];
        size--;
        return deleted;
    }
    void set(int idx,int value){
        assert(idx>=0 && idx<size);
        *(arr + idx)=value;
    }
    int get(int idx){
        assert(idx>=0 && idx<size);
        return *(arr + idx);
    }
    void print(){
        for (int i =0 ; i<size ; i++){
            std::cout << arr[i] << " ";
        }
        std::cout<<"\n";
    }
    int find(int value){
        for(int i = 0 ; i<size ;i++){
            if (value == arr[i]) return i;
        }
        return -1;
    }
    int find_transposition(int value){
        if (size == 1 && arr[0] ==value) return 0;
        for(int i = 0;i <size ;i++){
            if (arr[i] == value){
                std::swap(arr[i],arr[i-1]);
                return i;
            }
        }
        return -1;
    }

};


int main() {
    Vector myVector(10);
    for(int i = 0 ; i<10 ; i++){
        myVector.set(i,i);
    }

    myVector.insert(2,1555);
    myVector.print();
    std::cout<<myVector.find_transposition(5);
    myVector.print();



    std::cout<<myVector.get(9);
    std::cout<<myVector.find(9);

    return 0;
}
