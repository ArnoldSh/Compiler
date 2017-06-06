// Translator.cpp

#include "SemanticAnalyzer.h"
#include "CompilerException.h"

SemanticAnalyzer::SemanticAnalyzer() {
	redefinedVariables				= new vector<RedefinedVariables>;
	redefinedFunctions				= new vector<RedefinedFunctions>;
	undefinedVariables				= new vector<VariableCallNode*>;
	undefinedFunctions				= new vector<FunctionCallNode*>;

	for(unsigned int i = 0; i < parser.idCallTable->size(); i++)
		setVariableDeclarationsLinks(parser.idCallTable->at(i), *parser.idCallTable->at(i));

	for(unsigned int i = 0; i < parser.functionCallTable->size(); i++)
		setFunctionDeclarationsLinks(parser.functionCallTable->at(i));

	checkVariableRedefinitions();
	checkFunctionRedefenitions();

	checkErrors();
}

SemanticAnalyzer::~SemanticAnalyzer() {
	delete redefinedVariables;		redefinedVariables = NULL;
	delete redefinedFunctions;		redefinedFunctions = NULL;
}

ErrorMessage :: ErrorMessage(string _errorMessage, int _errorLine) {
	errorMessage	= _errorMessage;
	errorLine		= _errorLine;
}

void SemanticAnalyzer :: showLogs() {
	for(unsigned int i = 0; i < errorLogs.size(); i++) {
		cerr	<< errorLogs.at(i).errorMessage << " (line " 
				<< errorLogs.at(i).errorLine << ")" << endl;
	}
}

void SemanticAnalyzer :: checkErrors() {

	int errorsNumber = 0;

	for(unsigned int i = 0; i < parser.idCallTable->size(); i++) {
		if(parser.idCallTable->at(i)->declaration	== NULL &&
		parser.idCallTable->at(i)->parameter	== NULL  ) {
			undefinedVariables->push_back(parser.idCallTable->at(i));
			string errorMessage = "Semantic error: undefined variable '" 
					+ parser.idCallTable->at(i)->tokenId.data
					+ "'" ;
			errorLogs.push_back(ErrorMessage(errorMessage, 
					parser.idCallTable->at(i)->tokenId.line));
			errorsNumber++;
		}
	}

	for(unsigned int i = 0; i < parser.functionCallTable->size(); i++) {
		if(parser.functionCallTable->at(i)->definition	== NULL &&
		parser.functionCallTable->at(i)->prototype	== NULL	) {
			undefinedFunctions->push_back(parser.functionCallTable->at(i));
			string errorMessage = "Semantic error: undefined function '" 
					+ parser.functionCallTable->at(i)->tokenId.data
					+ "'";
			errorLogs.push_back(ErrorMessage(errorMessage, 
					parser.functionCallTable->at(i)->tokenId.line));
			errorsNumber++;
		}
	}

	for(unsigned int i = 0; i < redefinedVariables->size(); i++) {
		string errorMessage = "Semantic error: redefined variable '" 
				+ redefinedVariables->at(i).first->tokenId.data
				+ "'";
		errorLogs.push_back(ErrorMessage(errorMessage, 
				redefinedVariables->at(i).second->tokenId.line));
		errorsNumber++;
	}

	for(unsigned int i = 0; i < redefinedFunctions->size(); i++) {
		string errorMessage = "Semantic error: redefined function '" 
				+ redefinedFunctions->at(i).first->tokenId.data
				+ "'";
		errorLogs.push_back(ErrorMessage(errorMessage, 
				redefinedFunctions->at(i).second->tokenId.line));
		errorsNumber++;
	}

	for(unsigned int i = 0; i < parser.expressionsTable->size(); i++) {
		if(parser.expressionsTable->at(i)->nodeType == Node :: BIN_OP) {
			BinaryOperationNode* dummyBO =
					dynamic_cast<BinaryOperationNode*>(parser.expressionsTable->at(i));
			if(checkAssignments(dummyBO) == false)
				errorsNumber++;
		}
		else if(parser.expressionsTable->at(i)->nodeType == Node :: UNARY_OP) {
			UnaryOperationNode* dummyUO = 
					dynamic_cast<UnaryOperationNode*>(parser.expressionsTable->at(i));
			if(checkUnary(dummyUO) == false)
				errorsNumber++;
		}
	}

}

bool SemanticAnalyzer :: checkUnary(UnaryOperationNode* _node) {
	if(_node->operation.operationType == INC || _node->operation.operationType == DCR) {
		if(_node->operand->nodeType != Node :: USING_VAR && 
				_node->operand->nodeType != Node :: USING_ARRAY) {
			// error: operand of increment\decrement must be a modifiable

			string errorMessage = 
					"Semantic error: operand of increment\\decrement must be a l-value";
			errorLogs.push_back(ErrorMessage(errorMessage, _node->operation.line));

			return false;
		}
	}
	if(_node->operand->nodeType == Node :: UNARY_OP) {
		UnaryOperationNode* dummyUO = 
				dynamic_cast<UnaryOperationNode*>(_node->operand);
		return checkUnary(dummyUO);
	}
	return true;
}

bool SemanticAnalyzer :: checkAssignments(BinaryOperationNode* _node) {
	if(_node->operation.operationType == ASSIGN				|| 
		_node->operation.operationType == ADD_ASSIGN		|| 
		_node->operation.operationType == SUB_ASSIGN		|| 
		_node->operation.operationType == MUL_ASSIGN		||
		_node->operation.operationType == DIV_ASSIGN		|| 
		_node->operation.operationType == MOD_ASSIGN		||
		_node->operation.operationType == LEFT_BS_ASSIGN	|| 
		_node->operation.operationType == RIGHT_BS_ASSIGN	||
		_node->operation.operationType == BIT_NOT_ASSIGN	|| 
		_node->operation.operationType == BIT_XOR_ASSIGN	||
		_node->operation.operationType == BIT_AND_ASSIGN	|| 
		_node->operation.operationType == BIT_OR_ASSIGN		) {

		if(_node->leftOperand->nodeType != Node :: USING_VAR && 
				_node->leftOperand->nodeType != Node :: USING_ARRAY) {
			//error: left operand must be a modifiable

			string errorMessage = 
					"Semantic error: left operand of assignment-expression must be a l-value";
			errorLogs.push_back(ErrorMessage(errorMessage, _node->operation.line));

			return false;
		}

	} // check l-type of left operand

	if(_node->rightOperand->nodeType == Node :: BIN_OP) {
		BinaryOperationNode* dummyBO =
				dynamic_cast<BinaryOperationNode*>(_node->rightOperand);
		return checkAssignments(dummyBO);		
	} // check all right operands on assignments
	return true;
}

void SemanticAnalyzer :: showTokens() {
	parser.showTokens();
}

void SemanticAnalyzer :: showTree() {
	parser.showTree();
}

RedefinedVariables :: RedefinedVariables(VariableNode& _first, VariableNode& _second) {
	first	= &_first;
	second	= &_second;
}

RedefinedFunctions :: RedefinedFunctions(FunctionPrototypeNode& _first, 
										 FunctionPrototypeNode& _second) {
	first	= &_first;
	second	= &_second;
}

void SemanticAnalyzer :: checkVariableRedefinitions() {
	VariableNode* first;
	VariableNode* second;
	for(unsigned int i = 0; i < parser.idDeclareTable->size(); i++) {
		second = parser.idDeclareTable->at(i);
		for(unsigned int j = i + 1; j < parser.idDeclareTable->size(); j++) {
			first = parser.idDeclareTable->at(j);	
			if(second == first) continue;
			if(second->parent->parent == first->parent->parent 
					&& second->tokenId.data == first->tokenId.data)
				redefinedVariables->push_back(RedefinedVariables(*second, *first));
		}
	}
}

void SemanticAnalyzer :: checkFunctionRedefenitions() {
	FunctionPrototypeNode* first;
	FunctionPrototypeNode* second;
	for(unsigned int i = 0; i < parser.functionDeclareTable->size(); i++) {
		second = parser.functionDeclareTable->at(i);
		for(unsigned int j = i + 1; j < parser.functionDeclareTable->size(); j++) {
			first = parser.functionDeclareTable->at(j);
			if(second == first) continue;
			if(first->tokenId.data == second->tokenId.data) {
				if(first->parametersList->size() == second->parametersList->size()) {
					int n = 0;
					for(unsigned int k = 0; k < first->parametersList->size(); k++) {
						if(first->parametersList->at(k)->tokenType.keywordType ==
								second->parametersList->at(k)->tokenType.keywordType)
							n++;

					}
					if(n == first->parametersList->size())
						redefinedFunctions->push_back(RedefinedFunctions(*second, *first));
				}
			}
		}
	}
}

void SemanticAnalyzer :: setFunctionDeclarationsLinks(FunctionCallNode* _func) {
	int parametersNumber = 0;
	for(unsigned int i = 0; i < parser.syntacticTree->declarations->size(); i++) {
		if(parser.syntacticTree->declarations->at(i)->nodeType == Node :: DEF_FUNC) {
			FunctionDefinitionNode* dummy = 
					dynamic_cast<FunctionDefinitionNode*>(parser.syntacticTree->declarations->at(i));
			if(dummy->tokenId.data == _func->tokenId.data) {
				if(dummy->parametersList->size() == _func->parametersList->size()) {
					_func->definition = dummy; 
					i = parser.syntacticTree->declarations->size();
				}
			}
		}
	}
	for(unsigned int i = 0; i < parser.syntacticTree->declarations->size(); i++) {
		if(parser.syntacticTree->declarations->at(i)->nodeType == Node :: DECLARE_FUNC) {
			FunctionPrototypeNode* dummy = 
					dynamic_cast<FunctionPrototypeNode*>(parser.syntacticTree->declarations->at(i));
			if(dummy->tokenId.data == _func->tokenId.data) {
				if(dummy->parametersList->size() == _func->parametersList->size()) {
					_func->prototype = dummy; 
					i = parser.syntacticTree->declarations->size();
				}
			}
		}
	}
}

void SemanticAnalyzer :: setVariableDeclarationsLinks(VariableCallNode* _var, Node& _node) {
	Node*	dummy = &_node;

	if(dummy->parent == NULL) return;

	if(dummy->parent->nodeType == Node :: COMPOUND_ST) {
		CompoundStatementNode* dummyCS = 
				dynamic_cast<CompoundStatementNode*>(dummy->parent);
		unsigned int t = 0;
		while(true)  {
			if(dummyCS->statementsList->at(t) == dummy)
				break;
			t++;
			if(t-1 == dummyCS->statementsList->size())
				break;	
		}
		for(unsigned int j = 0; j <= t; j++) {
			if(dummyCS->statementsList->at(j)->nodeType == Node :: DECLARE_ST) {
				DeclarationStatementNode* dummyDS = 
						dynamic_cast<DeclarationStatementNode*>(dummyCS->statementsList->at(j));
				for(unsigned int k = 0; k < dummyDS->variablesList->size(); k++) {
					if(dummyDS->variablesList->at(k)->tokenId.data == _var->tokenId.data) {
						_var->declaration = dummyDS;
						return;
					}
				}
			}
		}
	}
	
	if(dummy->nodeType == Node :: DEF_FUNC) {
		FunctionDefinitionNode* dummyFD = 
				dynamic_cast<FunctionDefinitionNode*>(dummy);
		for(unsigned int j = 0; j < dummyFD->parametersList->size(); j++) {
			if(dummyFD->parametersList->at(j)->tokenId.data == _var->tokenId.data) {
				_var->parameter = dummyFD->parametersList->at(j);
				return;
			}
		}
	}

	if(dummy->nodeType == Node :: FOR_ST) {
		ForStatementNode* dummyFS = 
				dynamic_cast<ForStatementNode*>(dummy);
		if(dummyFS->forInitialization->nodeType == Node :: DECLARE_ST) {
			DeclarationStatementNode* dummyDS = 
					dynamic_cast<DeclarationStatementNode*>(dummyFS->forInitialization);
			for(unsigned int j = 0; j < dummyDS->variablesList->size(); j++) {
				if(dummyDS->variablesList->at(j)->tokenId.data == _var->tokenId.data) {
					_var->declaration = dummyDS;
					return;
				}
			}
		}
	}

	if(dummy->nodeType == Node :: SENTENCE) {
		SentenceNode* dummyS = dynamic_cast<SentenceNode*>(dummy);
		for(unsigned int n = 0; n < dummyS->declarations->size(); n++) {
			if(dummyS->declarations->at(n)->nodeType == Node :: DECLARE_ST) {
				DeclarationStatementNode* dummyDS = 
						dynamic_cast<DeclarationStatementNode*>(dummyS->declarations->at(n));
				for(unsigned int m = 0; m < dummyDS->variablesList->size(); m++) {
					if(dummyDS->variablesList->at(m)->tokenId.data == _var->tokenId.data) {
						_var->declaration = dummyDS;
						return;
					}
				}
			}
		}
	}

	dummy = dummy->parent;
	setVariableDeclarationsLinks(_var, *dummy);
	return;
}
