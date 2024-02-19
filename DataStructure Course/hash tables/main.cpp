#include <iostream>
#include <bits/stdc++.h>
using namespace std;



class hash{
private:
    const int hash_factor = 2123133;
    string s1,s2;
    int number;
public:
    int hash_string (string str,int n){
        long long sum=0;
        const int base =75;
        for(int i =0;i<str.size();++i){
            sum = ((sum*base)%n+(str[i]-'0')%n)%n;
        }
        return  sum;
    }
    long long hashres=0;
    int fold_hash(string str,int n){
        string pushThis;
        for(int i=0;i<str.size();++i){
            if(i%4==0) {
                hashres+=hash_string(pushThis,n);
                hashres%=n;
                pushThis.clear();
            }
            pushThis+=str[i];
        }
        hashres+= hash_string(pushThis,n);
        hashres%=n;
        return (int) hashres;
    }
    int hash_big (){
        hashres=0;
        fold_hash(s1,hash_factor);
        fold_hash(s2,hash_factor);
        fold_hash(to_string(number),hash_factor);
        return hash_factor;
    }
};
class phoneEntry{
public:
    int number;
    string name;
    phoneEntry(string name,int number):name(name),number(number){}
};
class hashTableofPhone{
private:
    int tableSize;
    int load_factor;
    int elemnts_have=0;
    vector<vector<phoneEntry>> table;
    int hash_string(string str){
        long long sum=0;
        const int base = 75;
        for(int i=0;i<str.size();++i){
            sum= ((sum*75)%tableSize +(str[i]-'0')%tableSize)%tableSize;
        }
        return (int)sum;
    }

public:
    hashTableofPhone(int size,int load_factor):tableSize(size),load_factor(load_factor){
        table.resize(tableSize);
    }

    void rehash(){
        if((tableSize/elemnts_have)<load_factor){
            tableSize*=2;
            table.resize(tableSize);

        }
    }
    void insert(phoneEntry obj){
        ++elemnts_have;
        int idx = hash_string(obj.name);
        for(int i =0;i<table[idx].size();++i){
            if(obj.name == table[idx][i].name){
                table[idx][i].number = obj.number;
                return;
            }
        }
        table[idx].push_back(obj);
        rehash();
    }
    void remove(phoneEntry toRemove){

    }



};

class hashprob{
private:
    vector<phoneEntry*> table;
    phoneEntry* deleted = new phoneEntry("",0);
    int tableSize;
public:
    hashprob(int tableSize):tableSize(tableSize){
        table.resize(tableSize);
    }


    int hash_string (string str,int n){
        long long sum=0;
        const int base =75;
        for(int i =0;i<str.size();++i){
            sum = ((sum*base)%n+(str[i]-'0')%n)%n;
        }
        return  sum;
    }

    bool insert(phoneEntry entry){
        int idx = hash_string(entry.name,tableSize);
        for(int i=0;i<tableSize;++i){
            if(!table[idx] || table[idx] == deleted){
                table[idx] = new phoneEntry(entry.name,entry.number);
                return true;
            }
            else if(table[idx]->name == entry.name){
                table[idx]->number = entry.number;
                return true;
            }
            idx = (idx+1)%tableSize;
        }
        return false;
    }
    void rehash(){
        vector<phoneEntry*> oldTable =table;
        table.clear();
        tableSize*=2;
        table.resize(tableSize);
        for(phoneEntry* p:oldTable){
            insert(*p);
        }
        

    }
    bool remove(string name){
        int idx = hash_string(name,tableSize);
        for(int i =0;i<tableSize;++i){
            if(!table[idx])
                return false;
            else if(table[idx]->name == name){
                delete table[idx];
                table[idx] = deleted;
                return true;
            }
            idx = (idx+1)% tableSize;
        }
        return false;
    }
    int get(string  name){
        int idx = hash_string(name,tableSize);
        for(int i=0 ;i<tableSize;++i){
            if(!table[idx])
                return -1;
            else if(table[idx]->name == name){
                return table[idx]->number;
            }
            idx = (idx+1)%tableSize;
        }
        return -1;
    }
};



class problem{
public:
    int find_distinct_sub(string str){
        int len = str.size();
        set<string> mySet;
        for(int i =0;i<len;++i){
            string s;
            for(int j=i;j<len;++j){
                s+=str[j];
                mySet.insert(s);
            }
        }
        return mySet.size();
    }


};
int main() {
    phoneEntry p1("assem",1004543017);
    phoneEntry p2("medhat",1093781554);
    phoneEntry p3("fathy",1234);
    phoneEntry p4("mostafa",15555);
    hashprob h(6);
    h.insert(p1);
    h.insert(p2);
    h.insert(p3);
    h.insert(p4);
    h.remove("assem");
    cout<<h.get("medhat");
    return 0;
}
