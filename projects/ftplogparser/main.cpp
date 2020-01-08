#include <iostream>
#include <fstream>
#include <ios>
#include <map>
#include <set>
#include <string>
#include <locale>
#include <vector>

using namespace std;

set<string> Checked;
map<string,long long> Downloaded;
map<long long, string> SortedDownloads;
vector<string> Words;

string GetG6FileName(ifstream& f)
{
  string s="",s1;
  f>>s;
  do
  {
    f>>s1;
    s+=s1;
  }
  while(s1!="from" && !f.eof());
  return s;    
}

string& ToUpper(string& s)
{
  for(int i=0; i<s.size(); i++)
    s[i]=toupper(s[i]);
  return s;
}

void ParseG6File(string& filename)
{
  ifstream f;
  f.open(filename.c_str(), ios_base::in);
  string s;
  while(f>>s>>s>>s)
  {
    string ip;
    f>>ip;
    ip.erase(ip.size()-1, 1);
    f>>s>>s;
    string file=GetG6FileName(f);
    long long fbegin,fend=0;
    while(!(f>>fbegin))
    {
      f.clear();
      do{f>>s;}while(s!="from");
    }
    f>>s;
    f>>fend;
    f>>s>>s>>s>>s>>s>>s>>s;
    if(s=="timed")
      f>>s;
    
    string FileUpper = ToUpper(file);
    if(Words.size() == 0)
      Downloaded[ip]+=fend-fbegin;
    else
    for(int i = 0; i < Words.size(); i++)
      if(FileUpper.find(Words[i]) != -1)
      {
        Downloaded[ip]+=fend-fbegin;
        break;
      }
  }
  f.close();
}

int main(int argc, char** argv)
{
  if(argc == 1)
  {
    cout << "ArX FTP Log Parser" << endl << endl;
    cout << "USAGE:" << endl; 
    cout << "FTPLogParser file1 [fileN...] [options]" << endl;
    cout << "Possible options:" << endl;
    cout << "-f=WORD   take into account only files with one of the WORDs as a substring" << endl;
    return 0;
  }

  for(int i = 1; i < argc; i++)
    if(argv[i][0] == '-' && strlen(argv[i]) > 3 && argv[i][1] == 'f' && argv[i][2] == '=')
      Words.push_back(ToUpper(string(&argv[i][3])));

  ifstream in;
  in.open("data.txt");
  if(in.is_open())
  {
    Checked.clear();
    int n;
    in>>n;
    for(int i=0; i<n; i++)
    {
      string s;
      in>>s;
      Checked.insert(s);
    }
    in>>n;
    for(int i=0; i<n; i++)
    {
      string s;
      long long size;
      in>>s>>size;
      Downloaded[s]=size;
    }
  }
  in.close();
  for(int i=1; i<argc; i++)
  { 
    if(argv[i][0] == '-' && strlen(argv[i]) > 3 && argv[i][1] == 'f' && argv[i][2] == '=')
      continue;
    string arg=argv[i];
    if(Checked.find(arg)==Checked.end())
      ParseG6File(arg);
    Checked.insert(arg);
  }
  ofstream out;
  out.open("data.txt");
  if(out.is_open())
  {
    out<<Checked.size()<<endl;
    for(set<string>::iterator i=Checked.begin(); i!=Checked.end(); i++)
      out<<*i<<endl;
      out<<Downloaded.size()<<endl;
    for(map<string,long long>::iterator i=Downloaded.begin(); i!=Downloaded.end(); i++)
    {
      out<<i->first<<' '<<i->second<<endl;
      SortedDownloads[(*i).second]=(*i).first;
    }
    out<<endl;
    for(map<long long,string>::reverse_iterator i=SortedDownloads.rbegin(); i!=SortedDownloads.rend(); i++)
    {
      if(i->first > 1024*1024*1024)
        out<<i->second<<' '<<i->first*1.0/(1024*1024*1024)<<" Gb"<<endl;
      else if(i->first > 1024*1024)
        out<<i->second<<' '<<i->first*1.0/(1024*1024)<<" Mb"<<endl;
      else if(i->first > 1024)
        out<<i->second<<' '<<i->first*1.0/(1024)<<" Kb"<<endl;
      else 
        out<<i->second<<' '<<i->first<<" b"<<endl;
    }
  }
  out.close();
  return 0;
}

