// Translator.h

#ifndef __TRANSLATOR_H
#define __TRANSLATOR_H

#include "SyntaxAnalyzer.h"

using namespace token_types;

struct RedefinedVariables {
	VariableNode* first;
	VariableNode* second;
	
	RedefinedVariables(VariableNode&, VariableNode&);
};

struct RedefinedFunctions {
	FunctionPrototypeNode* first;
	FunctionPrototypeNode* second;

	RedefinedFunctions(FunctionPrototypeNode&, FunctionPrototypeNode&); 
};

struct ErrorMessage {
	string	errorMessage;
	int		errorLine;
	ErrorMessage(string, int);
};

class SemanticAnalyzer {
private:
	void	setVariableDeclarationsLinks(VariableCallNode*, Node&);
	void	setFunctionDeclarationsLinks(FunctionCallNode*);
	void	checkVariableRedefinitions();
	void	checkFunctionRedefenitions();

	vector<RedefinedVariables>*		redefinedVariables;		
	vector<RedefinedFunctions>*		redefinedFunctions;
	vector<VariableCallNode*>*		undefinedVariables;
	vector<FunctionCallNode*>*		undefinedFunctions;

	vector<ErrorMessage>			errorLogs;

	SyntaxAnalyzer	parser;

	void checkErrors();
	bool checkAssignments(BinaryOperationNode*);
	bool checkUnary(UnaryOperationNode*);
public:
	SemanticAnalyzer();
	~SemanticAnalyzer();

	void showTokens();
	void showTree();
	void showLogs();
};

#endif // __TRANSLATOR_H