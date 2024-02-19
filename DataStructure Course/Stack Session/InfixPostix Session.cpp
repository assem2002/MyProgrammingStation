#include <iostream>
#include <cmath>
template<class theType>
class theStack{ //this stack uses the linked list approach.
private:
    struct Node{
        theType data;
        Node* next{};
        Node(theType value):data(value){}
    };
    Node* head{};
public:
    void push(theType value){
        Node* newNode = new Node(value);
        if (!head){
            head = newNode;
        }
        else{
            newNode->next =head;
            head = newNode;
        }
    }
    bool empty(){
        return !head;
    }
    bool pop(){
        if (empty()) return false;
        Node* toDelete = head;
        head = head->next;
        delete toDelete;
        return true;
    }
    theType top(){
        if(empty()){
            return '0';
        }
        return head->data;
    }
    void print(){
        for (Node* currentNode = head;currentNode;currentNode=currentNode->next){
            std::cout<<currentNode->data<<' ';
        }
        std::cout<<'\n';
    }
    ~theStack(){
        while(head){
            Node* temp =head->next;
            delete head;
            head= temp;
        }
    }
};

std::string shuntingYard(std::string expression){
    expression+='#'; // This makes the stack pop everything if it still has opeators in it.
    theStack<char> operatorsStack;
    std::string output;
    int arr[100]{0};
    arr['*'] =arr['/'] =2 ; arr['+'] = arr['-'] =1; arr[')'] = -1;arr['^'] =3;arr['#'] =-2;arr['0']=-1000; //precedence assigning.
    for(char letter : expression){
        if (arr[letter]==1 || arr[letter] ==2 ||arr[letter] == -1 ||arr[letter] == 3 ||arr[letter] == -2){
            if(operatorsStack.empty()) operatorsStack.push(letter);
            else{
                while (arr[operatorsStack.top()]>=arr[letter]){
                    if(arr[operatorsStack.top()] == arr[letter] &&arr[letter] == 3) break; //right to left associativity of ^ operator
                    if(operatorsStack.top() == '('){ //handles the sub problems thing.
                        operatorsStack.pop();
                        break;
                    }
                    output+=operatorsStack.top();
                    operatorsStack.pop();
                }
                operatorsStack.push(letter);
            }
            if (operatorsStack.top() ==')') operatorsStack.pop(); //this operator shouldn't exist in the stack ever.
        }
        else if(letter == '(') operatorsStack.push(letter);
        else output+=letter;
    }
    return output;
}

int evaluatePostfix(std::string postfixExpression){
    theStack<int> myS;
    for (char letter:postfixExpression){
        if(std::isdigit(letter)) myS.push(letter-48);
        else{
            int second,first,res;
            second = myS.top();myS.pop();
            first  = myS.top();myS.pop();
            switch (letter) {
                case '+':
                    res = first+second;
                    break;
                case '-':
                    res = first - second;
                    break;
                case'*':
                    res = first * second;
                    break;
                case '/':
                    res = first / second;
                    break;
                case '^':
                    res =std::pow(first,second);
                    break;
            }
            myS.push(res);
        }
    }
    return myS.top();
}
char getRightOperatorResult(char first,char second){
    if (first == second) return '+';
    return '-';
}
std::string removeParantheses(std::string expression){
    std::string output;
    theStack<char> operatorsStack;
    operatorsStack.push('+');
    for (int letter= 0 ; letter< expression.size();letter++){
        if (std::isdigit(expression[letter])) output+=expression[letter];
        else if (expression[letter] == '(' && letter) operatorsStack.push(getRightOperatorResult(operatorsStack.top(),expression[letter-1]));
        else if(expression[letter] == ')') operatorsStack.pop();
        else output+= getRightOperatorResult(expression[letter],operatorsStack.top());
    }
    return output;
}

int main(){
    std::cout<<removeParantheses("1-(2-3-(4+5))-6-(7-8)")<<'\n';
    std::cout<<(removeParantheses("1-(2-3-(4+5))-6-(7-8)") == "1-2+3+4+5-6-7+8");
    std::cout<<evaluatePostfix("432^^");
}