#ifndef __PIPECOMM_H__
#define __PIPECOMM_H__
#include <Windows.h>

class PipeComm
{
private:
	bool Created;
	HANDLE mrPipe, mrPipeD;
	HANDLE mwPipe, mwPipeD;
	HANDLE crPipe;
	HANDLE cwPipe;
	PROCESS_INFORMATION pInfo;
public:
	PipeComm(const char* CommandLine);
	~PipeComm();
	int Write(const char* buf, int count);
	int Read(char* buf, int count);
	bool Alive();
	HANDLE ReadHandle() {return this->mrPipe;};
	HANDLE WriteHandle() {return this->mwPipe;};
};

#endif