#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <sstream>
#include "pipecomm.h"


#define SZ(filename) ("7z.exe t -y \"" + string(filename) + "\"")
#define SZOK         ("\nEverything is Ok")

#define string std::basic_string<TCHAR>

using namespace std;

string strencode(string s)
{
  string result;
  for(string::iterator i = s.begin(); i != s.end(); i++)
  {
    if(*i == ' ')
      result += "%_";
    else if(*i == '\t')
      result += "%t";
    else if(*i == '%')
      result += "%%";
    else
      result += *i;
  }
  return result;
}

string strdecode(string s)
{
  string result;
  for(string::iterator i = s.begin(); i != s.end(); i++)
  {
    if(*i == '%')
    {
      i++;
      if(*i == '_')
        result += " ";
      else if(*i == 't')
        result += "\t";
      else if(*i == '%')
        result += "%";
    }
    else
      result += *i;
  }
  return result;
}


typedef struct DBElement {
  string Name;
  unsigned long long Size;
  unsigned long long LastWriteTime;
  bool operator< (const DBElement& e) const 
  {
    if(Name < e.Name)
      return true;
    else if(Name > e.Name)
      return false;
    else if(Size < e.Size)
      return true;
    else if(Size > e.Size)
      return false;
    else if(LastWriteTime < e.LastWriteTime)
      return true;
    else
      return false;
  }
} DBElement;

set<DBElement> Checked;
bool Recursive = false;
ostream* out = &cout;
string CurrentDir;

void TraverseDirTree();

void CheckFile(LPWIN32_FIND_DATA FD)
{
  cout << "Processing " << CurrentDir << FD->cFileName << endl;
  if((FD->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0)
  {
    if(strcmp(FD->cFileName, ".") != 0 && strcmp(FD->cFileName, "..") != 0 && Recursive)
    {
      string OldDir = CurrentDir;
      CurrentDir += FD->cFileName;
      TraverseDirTree();
      CurrentDir = OldDir;
    }
  }
  else
  {
    DBElement e;
    e.Name = CurrentDir + FD->cFileName;
    e.Size = ((unsigned long long)FD->nFileSizeHigh << 32) + FD->nFileSizeLow;
    e.LastWriteTime = *((unsigned long long *)&FD->ftLastWriteTime);

    string ext = "";
    unsigned int pos = e.Name.rfind(".");
    if(pos != string::npos)
      ext = e.Name.substr(pos);

    if(ext != ".zip" && ext != ".rar" && ext != ".7z")
      return;

    if(Checked.find(e) == Checked.end())
    {
      PipeComm p7z(SZ(e.Name).c_str());
      string SZOutput;
      char buf[1025];
      while(true)
      {
        int num = p7z.Read(buf, 1024);
        if(num == -1 || (num <= 0 && !p7z.Alive()))
          break;
        else if(num == 0)
          Sleep(100);
        else
        {
          buf[num] = 0;
          SZOutput += buf;
        }
      }
      if(SZOutput.find(SZOK) != string::npos)
        Checked.insert(e);
      else
        *out << e.Name << endl;
    }
  }
  return;
}

void TraverseDirTree()
{
  if(CurrentDir[CurrentDir.length()] != '\\')
    CurrentDir += "\\";
  string Mask = CurrentDir + "*.*";

  WIN32_FIND_DATA FD;
  HANDLE hSearch;

  if((hSearch = FindFirstFile(Mask.c_str(), &FD)) == INVALID_HANDLE_VALUE)
    return;
  CheckFile(&FD);
  while(FindNextFile(hSearch, &FD) != 0)
    CheckFile(&FD);
  FindClose(hSearch);
  return;
}

int main(int argc, char** argv)
{
  if(argc < 2)
  {
    cout << "arxaic - ArX Archive Integrity Checker" << endl << endl;
    cout << "USAGE:" << endl; 
    cout << "arxaic dir [options]" << endl;
    cout << "Possible options:" << endl;
    cout << "-r       Recursive (also check archives in subdirectories)" << endl;
    cout << "-o=FILE  Output the list of broken archives into FILE instead of stdout" << endl;
  }
  else
  {
    CurrentDir = argv[1];
    for(int i = 2; i < argc; i++)
      if(argv[i][0] == '-' && strlen(argv[i]) > 1)
      {
        if(argv[i][1] == 'r')
          Recursive = true;
        else if(argv[i][1] == 'o')
        {
          if(strlen(argv[i]) > 3)
          {
            ofstream* newout = new ofstream;
            newout->open(argv[i] + 3);
            if(!newout->is_open())
              delete newout;
            else
              out = newout;
          }
        }
      }
  }

  ifstream dbin;
  dbin.open("checked.db");
  if(dbin.is_open())
    while(true)
    {
      DBElement e;
      dbin >> e.Name >> e.Size >> e.LastWriteTime;
      e.Name = strdecode(e.Name);
      if(dbin.eof())
        break;
      Checked.insert(e);
    }
  dbin.close();

  TraverseDirTree();

  ofstream dbout;
  dbout.open("checked.db");
  if(dbout.is_open())
    for(set<DBElement>::iterator i = Checked.begin(); i != Checked.end(); i++)
      dbout << strencode(i->Name) << ' ' << i->Size << ' ' << i->LastWriteTime << endl;
  dbout.close();

  return 0;
}