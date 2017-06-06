// CompilerException.h

#ifndef __COMPILER_EXCEPTION_H
#define __COMPILER_EXCEPTION_H

#include <string>
#include <iostream>

class CompilerException {
	std::string message;
public:
	CompilerException(const std::string, int, int);
	CompilerException(const std::string);

	void printMessage();
};

#endif //__COMPILER_EXCEPTION_H