// Compiler.cpp : Defines the entry point for the console application.
//


#include <iostream>
#include <windows.h>

#include "stdafx.h"
#include "Compiler.h"
#include "CompilerException.h"

using namespace std;

int _tmain(int argc, _TCHAR* argv[]) {
	
	HANDLE out_handle = GetStdHandle(STD_OUTPUT_HANDLE);
	COORD crd = {120, 1000};
	SetConsoleScreenBufferSize (out_handle, crd);

	try {
		Compiler compiler;
		
		compiler.showTokens();	// show list of tokens
		compiler.showTree();		// show syntactic tree
		compiler.showLogs();		// show semantic errors	
		getchar();
	}

	catch( CompilerException e ) {
		e.printMessage();
		getchar();
	}

	return 0;
}

