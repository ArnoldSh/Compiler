// Compiler.h

#ifndef __COMPILER_H
#define __COMPILER_H

#include "SemanticAnalyzer.h"

class Compiler {
private:
	SemanticAnalyzer semanticAnalyzer;
public:
	Compiler() {}
	~Compiler() {}

	void showTokens() {
		semanticAnalyzer.showTokens();
	}

	void showTree() {
		semanticAnalyzer.showTree();
	}

	void showLogs() {
		semanticAnalyzer.showLogs();
	}
};

#endif //__COMPILER_H