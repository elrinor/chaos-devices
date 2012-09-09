#include "pipecomm.h"
#include <string.h>

PipeComm::PipeComm(const char* pCommandLine)
{
  char* CommandLine = new char[strlen(pCommandLine) + 1];
  strcpy(CommandLine, pCommandLine);
	this->Created = false;

	SECURITY_ATTRIBUTES sa;
	memset(&sa, 0, sizeof(sa));
	sa.bInheritHandle = TRUE;
	CreatePipe(&this->mrPipeD, &this->cwPipe, &sa, 0);
	CreatePipe(&this->crPipe, &this->mwPipeD, &sa, 0);
	DuplicateHandle(GetCurrentProcess(), this->mwPipeD, GetCurrentProcess(), &this->mwPipe, 0, FALSE, DUPLICATE_SAME_ACCESS);
	DuplicateHandle(GetCurrentProcess(), this->mrPipeD, GetCurrentProcess(), &this->mrPipe, 0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(this->mwPipeD);
	CloseHandle(this->mrPipeD);

	STARTUPINFO si;
	memset(&si, 0, sizeof(si));
	si.cb = sizeof(si);
	si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
	si.wShowWindow = SW_HIDE;
	si.hStdInput = this->crPipe;
	si.hStdOutput = this->cwPipe;
	si.hStdError = this->cwPipe;

	if(CreateProcess(NULL, CommandLine, NULL, NULL, TRUE, /*CREATE_NO_WINDOW*/CREATE_NEW_CONSOLE, NULL, NULL, &si, &this->pInfo))
		this->Created = true;

	CloseHandle(this->crPipe);
	CloseHandle(this->cwPipe);
  delete CommandLine;
}

PipeComm::~PipeComm()
{
	if(this->Created)
	{
		TerminateProcess(this->pInfo.hProcess, 0);
		CloseHandle(this->pInfo.hProcess);
		CloseHandle(this->pInfo.hThread);
		CloseHandle(this->mrPipe);
		CloseHandle(this->mwPipe);
	}
}

int PipeComm::Write(const char* buf, int count) 
{
	int written = 0;
	DWORD w;
	do {
		if(!WriteFile(this->mwPipe, buf, count - written, &w, NULL))
			return -1;
		written += w;
	} while (written < count);
	return 0;
}

int PipeComm::Read(char* buf, int count)
{
	DWORD read = 0, avail = 0;
	PeekNamedPipe(this->mrPipe, buf, count, &read, &avail,NULL);
	if(read > 0)
	{
		if(!ReadFile(this->mrPipe, buf, count, &read, NULL))
			return -1;
	}
	else
		return 0;
	return read;
}

bool PipeComm::Alive()
{
	DWORD ExitCode;
	GetExitCodeProcess(this->pInfo.hProcess, &ExitCode);
	if(ExitCode == STILL_ACTIVE)
		return true;
	else
		return false;
}