#include<iostream>
struct Node{
    int data;
    int idx;
    Node* next{};
    Node* prev{};
    Node(int data,int idx):data(data),idx(idx){}
};

class matrix1D{
private:
    Node* head{} ;
    Node* tail{} ;
    int size=0;
    int actualLength=0;
    void link(Node* first,Node* second){
        second->prev = first;
        second ->next = first->next;
        first->next = second;
        if (second->next) second->next->prev = second; //this condition handles an insertion after the tail...but still you have to update the tail pointer.
    }
public:
    matrix1D(int size):size(size){}
    void set_value(int value,int idx){
        ++actualLength;
        Node* newNode = new Node(value,idx);
        if (head == nullptr) head = tail = newNode;
        else{
            for (Node* currentNode = head ; currentNode; currentNode= currentNode->next){
                if (currentNode->idx>idx) {
                    if(currentNode == head){
                        newNode->prev = head->prev;
                        head->prev = newNode;
                        newNode->next = head;
                        head = newNode;
                        return;
                    }
                    link(currentNode->prev,newNode);
                    return;
                }
                else if(currentNode->idx == idx){
                    currentNode->data = value;
                    delete newNode;
                    return;
                }
            }
            link(tail,newNode);
            tail = newNode;
        }
    }
    void print(){
        Node* waitNode = head;
        for(int i =0; i<size; ++i){
            if (!waitNode || waitNode->idx != i) std::cout<<0<<" ";
            else{
                std::cout<<waitNode->data<<" ";
                waitNode = waitNode->next;
            }
        }
        std::cout<<'\n';
    }
    void add(matrix1D &other){
        Node* first = this->head;
        Node* second = other.head;
        Node* secondNext = nullptr;
        if (!first){
            this->head = other.head;
            this->tail = other.tail;
            return;
        }
        for(;first && second;){
            if (first->idx>second->idx) {
                ++actualLength;
                secondNext = second->next;
                if(first == this->head){
                    second->prev = head->prev;
                    head->prev = second;
                    second->next = head;
                    head = second;
                }
                else link(first->prev,second);
                second =secondNext;

            }
            else if(first->idx == second->idx){
                first->data+=second->data;
                first = first->next;
                second = second->next;
            }
            else first = first->next;
        }
        for(;second;second = secondNext){
            secondNext = second->next;
            link(tail,second);
            tail = second;
        }

    }
    ~matrix1D(){
        while(head){
            Node* temp = head->next;
            delete head;
            head = temp;
        }
    }

};
struct Node2D{
    matrix1D* row;
    int rowIndex;
    Node2D* next{};
    Node2D* prev{};
    Node2D(matrix1D* row,int rowIndex):row(row),rowIndex(rowIndex){}
};
class matrix2D{
private:
    Node2D* head{};
    Node2D* tail{};
    int rowSize=0;
    int columnSize =0;
    int actualRowsLength=0;
    void link(Node2D* first,Node2D* second){
        second->prev = first;
        second ->next = first->next;
        first->next = second;
        if (second->next) second->next->prev = second; //this condition handles an insertion after the tail...but still you have to update the tail pointer.
    }

    Node2D* createNewRow(int rowIndex){
        ++actualRowsLength;

        matrix1D* newRow = new matrix1D(columnSize);
        Node2D* rowPointer = new Node2D(newRow,rowIndex);

        if (head == nullptr){
            head = tail = rowPointer;
            return rowPointer;
        }
        else{
            for(Node2D* currentRow = head ; currentRow; currentRow=currentRow->next){
                if (currentRow->rowIndex > rowIndex){
                    if(currentRow == head){
                        head->prev = rowPointer;
                        rowPointer->next = head;
                        return rowPointer;
                    }
                    link(currentRow,rowPointer);
                    return rowPointer;
                }
            }
            link(tail,rowPointer);
            tail = rowPointer;
            return rowPointer;
        }

    }

public:
    matrix2D(int rows,int columns) :rowSize(rows),columnSize(columns){}

    void set_value(int rowIndex,int columnIndex ,int value){
        for(Node2D* currentRow = head ;currentRow; currentRow = currentRow->next){
            if (currentRow->rowIndex == rowIndex){
                currentRow->row->set_value(value,columnIndex);
                return;
            }
        }
        Node2D* newRowCreated = createNewRow(rowIndex);
        newRowCreated->row->set_value(value,columnIndex);
    }
    void print(){
        Node2D* waitToPrint =head;
        for (int i=0 ; i<rowSize ; ++i){
            if (!waitToPrint || waitToPrint->rowIndex != i) {
                for(int j=0;j<columnSize;++j) std::cout<<0<<' ';
                std::cout<<'\n';
            }
            else{
                waitToPrint->row->print();
                waitToPrint= waitToPrint->next;
            }
        }
    }
    void add(matrix2D &other){
        if(this->rowSize != other.rowSize || this->columnSize != other.columnSize){
            std::cout<<"invalid operation, matrices don't match in size.";
            return;
        }
        if (!this->head){ // if i got no data in my own 2D matrix.
            head =other.head;
            tail = other.tail;
            return;
        }

        Node2D* first = this->head;
        Node2D* second = other.head;
        Node2D* secondNextHolder;
        for (;first && second;){
            secondNextHolder = second->next;
            if (first->rowIndex > second->rowIndex){
                if (first == this->head){ //if the new row will be inserted in front of the head.
                    head->prev = second;
                    second->next = head;
                    second->prev = nullptr;
                    head = second;
                }
                else{
                    link(first,second);
                }
                second = secondNextHolder;
            }
            else if(first->rowIndex == second->rowIndex){
                first->row->add(*(second->row));
                first = first->next;
                second = second->next;
            }
            else
                first = first->next;

        }
        if(second){ //completes the job if the second matrix still has row not inserted in my matrix.
            tail->next = second;
            second->prev = tail;
            tail = other.tail;
        }

    }
    ~matrix2D(){
        while(head){
            Node2D* temp = head->next;
            delete head;
            head = temp;
        }
    }


};
int main(){
//    matrix1D myMatrix(10);
//    myMatrix.set_value(2,1);
////    myMatrix.set_value(4,3);
////    myMatrix.set_value(7,6);
//    myMatrix.set_value(9,8);
//    myMatrix.set_value(10,9);
//    myMatrix.print();
//    matrix1D myMatrix2(10);
//    myMatrix2.set_value(10,9);
//    myMatrix2.set_value(555,9);
//    myMatrix2.set_value(8,7);
////    myMatrix2.set_value(10,9);
//    myMatrix2.print();
//    myMatrix.add(myMatrix2);
//    myMatrix.print();

    matrix2D my2DMatrix(10,10);
    my2DMatrix.set_value(0,0,1);
    my2DMatrix.set_value(0,0,2);
    my2DMatrix.set_value(1,9,2);
    my2DMatrix.set_value(9,9,55);
    my2DMatrix.print();
    std::cout<<"\n\n";
    matrix2D my2DMatrix2(10,10);
    my2DMatrix2.set_value(0,0,1);
    my2DMatrix2.set_value(0,0,2);
    my2DMatrix2.set_value(1,9,2);
    my2DMatrix2.set_value(9,9,55);
    my2DMatrix2.print();
    std::cout<<"\n\n";
    my2DMatrix.add(my2DMatrix);
    my2DMatrix.print();


    std::cout<<"Done";
    return 0;


}