#include <iostream>
#include <bits/stdc++.h>
using namespace std;
class trie{
private:
    struct node{
        bool marked=false;
        node* arr[26]{};
        node(){}
    };
    node* root =new node();
public:
    void insert(string value){
        node* current =root;
        for(int i=0;i<=value.size();++i){
            if(i==value.size()) current->marked= true;
            else {
                if (!current->arr[value[i] - 97])
                    current->arr[value[i] - 97] = new node();
                current = current->arr[value[i] - 97];
            }
        }
    }
    int existall_or_prefix(string value){
        node* current =root;
        for(int i=0;i<=value.size();++i){
            if(i== value.size() && current->marked) return 1;
            if(i== value.size() && !current->marked) return 2;
            if(!current->arr[value[i]-97]) return 0;
            current = current->arr[value[i]-97];
        }
        return -1;
    }
    bool exist(string value){
        if (existall_or_prefix(value) ==1) return true;
        return false;
    }
    bool prefixExist(string value){
        if(existall_or_prefix(value) == 0) return false;
        return true;
    }

};

class theTrie{
private:
    bool end =false;
    theTrie* edges[26]{};

public:
    void insertrec(string word,int idx=0){
        if(word.size() == idx){
            end=true;
            return;
        }
        theTrie* &next = edges[word[idx]-'a'];
        if(!next)
            next = new theTrie;
        next->insertrec(word,idx+1);
    }
    bool isExist(string word,int idx=0){
        theTrie* edge = edges[word[idx]-'a'];
        if(word.size() == idx && end) return true;
        if(!edge)
            return false;
        return edge->isExist(word,idx+1);
    }
    string minimum_prefix(string word,int idx=0){
        theTrie* edge = edges[word[idx]-'a'];
        if(idx>word.size()) word;
        if(end) return word.substr(0,idx);
        if(!edge)
            return word;
        return edge->minimum_prefix(word,idx+1);
    }
    // can't be used with the normal insertion, would lead to incorrect answers.
    //just didn't want to write the code again.
    string flipper(string &word){
        string filpped;
        for(int i =word.size()-1 ;i>=0;--i){
            filpped+=word[i];
        }
        return filpped;
    }
    void insertReversed(string word){
        insertrec(flipper(word));
    }
    bool suffix_exist_helper(string word,int idx=0){
        theTrie* edge = edges[word[idx] -'a'];
        if(word.size() == idx) return true;
        if(!edge) return false;
        return edge->suffix_exist_helper(word,idx+1);
    }
    bool suffix_exist (string word){
        return  suffix_exist_helper(flipper(word));
    }

};



//class theTrieEffecient{
//private:
//    bool end =false;
//    map<char,theTrieEffecient*> edges{};
//
//public:
//    void insertrec(string word,int idx=0){
//        if(word.size() == idx){
//            end=true;
//            return;
//        }
//        theTrieEffecient* &next = edges[word[idx]];
//        if(!next)
//            next = new theTrieEffecient;
//        next->insertrec(word,idx+1);
//    }
//    bool isExist(string word,int idx=0){
//        theTrieEffecient* edge = edges[word[idx]];
//        if(word.size() == idx && end) return true;
//        if(!edge)
//            return false;
//        return edge->isExist(word,idx+1);
//    }
//    string minimum_prefix(string word,int idx=0){
//        theTrieEffecient* edge = edges[word[idx]];
//        if(idx>word.size()) word;
//        if(end) return word.substr(0,idx);
//        if(!edge)
//            return word;
//        return edge->minimum_prefix(word,idx+1);
//    }
//    // can't be used with the normal insertion, would lead to incorrect answers.
//    //just didn't want to write the code again.
//    string flipper(string &word){
//        string filpped;
//        for(int i =word.size()-1 ;i>=0;--i){
//            filpped+=word[i];
//        }
//        return filpped;
//    }
//    void insertReversed(string word){
//        insertrec(flipper(word));
//    }
//    bool suffix_exist_helper(string word,int idx=0){
//        theTrieEffecient* edge = edges[word[idx]];
//        if(word.size() == idx) return true;
//        if(!edge) return false;
//        return edge->suffix_exist_helper(word,idx+1);
//    }
//    bool suffix_exist (string word){
//        return  suffix_exist_helper(flipper(word));
//    }
//    void print_all(string passed=""){
//        if (end) cout<<passed<<'\n';
//        for(pair<char,theTrieEffecient*> a:edges){
//            if(a.second) {
//                (a.second)->print_all(passed + a.first);
//            }
//        }
//    }
//    void auto_complete(string word,int idx=0){
//        if(idx == word.size()){
//            return print_all(word);
//        }
//        theTrieEffecient* edge = edges[word[idx]];
//        if (!edge) return;
//        edge->auto_complete(word,idx+1);
//    }
//    bool singleLetter(string word){
//        for(int letter =0 ; letter<word.size();++letter){
//            for(char change='a';change<='z' ;++change){
//                if(change ==word[letter]) continue;
//                if(isExist(word.substr(0,letter)+change+word.substr(letter+1,word.size())))
//                    return true;
//            }
//        }
//        return false;
//    }
//    bool sub_exist(const string &word ,int idx =0){
//        if(idx==word.size()) return true;
//        for(pair<char,theTrieEffecient*> a:edges){
//            if(a.second){
//                if(word[idx] == a.first) return (a.second)->sub_exist(word,idx+1);
//                else if(idx>0) return (a.second)->sub_exist(word,0);
//                return (a.second)->sub_exist(word,idx);
//            }
//            return false;
//        }
//    }
//    int this_begining_exist_as_word(string word,int idx){
//        if(idx ==word.size()) return -1;
//        theTrieEffecient* next =edges[word[idx]];
//        if(end)
//            return idx;//not included.
//        if(next){
//            return next->this_begining_exist_as_word(word,idx+1);
//        }
//        else
//            return -1;
//    }
//};


class theTrieEffecient{
private:
    bool end =false;
    int indexx=-1;
    unordered_map<char,theTrieEffecient*> edges{};

public:
    void insertrec(string word,int dicind,int idx=0){
        if(word.size() == idx){
            end=true;
            indexx =dicind;
            return;
        }
        theTrieEffecient* &next = edges[word[idx]];
        if(!next)
            next = new theTrieEffecient;
        next->insertrec(word,dicind,idx+1);
    }
    static int maxi;
    void job(string &suf,int ind=0){
        if(end){
//            cout<<"worked\n"<<indexx<<'\n';
            if(ind == suf.size()) theTrieEffecient::maxi =max(maxi,indexx);
//            cout<<"worked\n"<<max(maxi,indexx)<<'\n';
        }
        for(pair<char,theTrieEffecient*> a: edges){
            if(a.second){
                cout<<a.first<<suf<<'\n';
                if(a.first == suf[ind]){
//                    cout<<"this one\n";
                    (a.second)->job(suf,ind+1);
                }
                else{
//                    cout<<"this oneeeeeeeee\n";
                    (a.second)->job(suf,0);
                }
            }
        }
    }
    void job0(string &pre,string &suf,int ind=0){
        auto r= pre_exist(pre,suf);
        if (r){
            cout<<"branch to job\n";
            if(suf==pre) return;
            else
                return r->job(suf,0);

        }

    }
    theTrieEffecient* pre_exist(string word,string suf ,int ind=0){
        theTrieEffecient* n = edges[word[ind]];
        if(ind ==word.size()) {
            if(word==suf) theTrieEffecient::maxi=max(maxi,indexx);
            return this;
        }
        if(!n) return nullptr;
        else return n->pre_exist(word,suf,ind+1);
    }
    int mainjob(string &pre,string &suf){
        theTrieEffecient::maxi=-1;
        cout<<"main works"<<'\n';
        job0(pre,suf);
        return 0;
    }


};
int theTrieEffecient::maxi =-1;



//void find_all_substr(const string &str,const vector<string> &queries){
//    theTrieEffecient t;
//    t.insertrec(str);
//    for(const string &s:queries){
//        if(t.sub_exist(s)) cout<<s<<'\n';
//    }
//
//}
//void find_all_substr2(const string &str,const vector<string> &queries){
//    theTrieEffecient t;
//    for(string s:queries) t.insertrec(s);
//    for(int i =0;i<str.size();++i){
//        int ret =t.this_begining_exist_as_word(str,i);
//        if( ret !=-1){
//            cout<<str.substr(i,ret-i)<<'\n';
//        }
//    }
//
//}



class WordFilter {
    theTrieEffecient t;
public:
    WordFilter(vector<string>& words) {
        set<string>s;
        for(int i=words.size()-1;i>=0;--i){
            if(s.count(words[i])) continue;
            s.insert(words[i]);
            t.insertrec(words[i],i);
            cout<<"insertingworks\n";
        }
    }

    int f(string pref, string suff) {
        t.mainjob(pref,suff);
        return theTrieEffecient::maxi;
    }
};
//
//class ospathTrie{
//private:
//    bool end =false;
//    map<string,ospathTrie*> edges{};
//
//public:
//    void insertrec(vector<string> word,int idx=0){
//        if(word.size() == idx){
//            end=true;
//            return;
//        }
//        ospathTrie* &next = edges[word[idx]];
//        if(!next)
//            next = new ospathTrie;
//        next->insertrec(word,idx+1);
//    }
////    bool isExist(vector<string> word,int idx=0){
////        if(word.size() == idx && end) return true;
////        if (word.size() == idx ) return false;
////        ospathTrie* edge = edges[word[idx]];
////        if(!edge)
////            return false;
////        return edge->isExist(word,idx+1);
////    }
//    bool isExist(vector<string> word,int idx=0){
//        if (word.size() == idx ) return true;
//        ospathTrie* edge = edges[word[idx]];
//        if(!edge)
//            return false;
//        return edge->isExist(word,idx+1);
//    }
////    vector<string> minimum_prefix(vector<string> &word,int idx=0){
////        ospathTrie* edge = edges[word[idx]];
////        if(idx>word.size()) word;
////        if(end) return copy(word.begin(),word.end(),word.begin()+idx);
////        if(!edge)
////            return word;
////        return edge->minimum_prefix(word,idx+1);
////    }
//    // can't be used with the normal insertion, would lead to incorrect answers.
//    //just didn't want to write the code again.
//    vector<string> flipper(vector<string> word){
//        vector<string> filpped;
//        for(int i =word.size()-1 ;i>=0;--i){
//            filpped.push_back(word[i]);
//        }
//        return filpped;
//    }
//    void insertReversed(vector<string> word){
//        insertrec(flipper(word));
//    }
//    bool suffix_exist_helper(vector<string> word,int idx=0){
//        ospathTrie* edge = edges[word[idx]];
//        if(word.size() == idx) return true;
//        if(!edge) return false;
//        return edge->suffix_exist_helper(word,idx+1);
//    }
//    bool suffix_exist (vector<string> word){
//        return  suffix_exist_helper(flipper(word));
//    }
//
//};

int main() {
//    theTrieEffecient t;
//    find_all_substr2("heyabcdtwxyw",{"xy","ab","t","yz"});

//    t.insertrec({"assem","medhat"});
//    t.insertrec({"pop","medhat"});

//    t.insertrec("af");
//    t.insertrec("assem");
//    t.insertrec("assemmedhat");
//    t.insertrec("assemmedhatfa");
//    t.insertrec("as");
//    t.insertrec("medhat");
//    t.insertrec("fathy");
//    t.print_all();
//    t.auto_complete("as");
//    cout<<t.isExist({"popp","medhat"});
//    cout<<t.isExist({"pop","medhat"});
//    cout<<t.isExist({"pop","medhat","assem"});
//    cout<<t.minimum_prefix("afbcd");
//    t.insertReversed("problem");
//    cout<<t.suffix_exist("pm");
//    cout<<t.singleLetter("assemfedhat");
    vector<string> a {"abbba","abba"};
    WordFilter w(a) ;

    cout<<w.f("ab","ba");
    cout<<w.f("i","i");



    return 0;
}
