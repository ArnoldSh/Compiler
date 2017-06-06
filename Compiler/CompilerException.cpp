// CompilerException.cpp

#include "CompilerException.h"

CompilerException :: CompilerException(const std::string _message, int _line, int _pos) {
		message = _message;
		message += " line ";
		message += std::to_string(_line);
		message += ", pos ";
		message += std::to_string(_pos);		
}

CompilerException :: CompilerException(const std::string _message) {
		message = _message;		
}

void CompilerException :: printMessage() {
	std::cerr << message;	
}
