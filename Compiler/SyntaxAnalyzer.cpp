// Parser.cpp

#include "SyntaxAnalyzer.h"
#include "CompilerException.h"

SyntaxAnalyzer :: SyntaxAnalyzer() {
	idCallTable				= new vector<VariableCallNode*>;
	functionCallTable		= new vector<FunctionCallNode*>;
	idDeclareTable			= new vector<VariableNode*>;
	functionDeclareTable	= new vector<FunctionPrototypeNode*>;
	functionDefineTable		= new vector<FunctionDefinitionNode*>;
	expressionsTable		= new vector<ExpressionNode*>;
	errorsTable				= new vector<SyntaxError>;
	syntacticTree			= checkSentence();
	checkErrors();
}

SyntaxAnalyzer :: ~SyntaxAnalyzer() {
	syntacticTree->free();
	delete syntacticTree;			syntacticTree		 = NULL;
	delete idCallTable;				idCallTable			 = NULL;
	delete functionCallTable;		functionCallTable	 = NULL;
	delete idDeclareTable;			idDeclareTable		 = NULL;
	delete functionDeclareTable;	functionDeclareTable = NULL;
	delete functionDefineTable;		functionDefineTable	 = NULL;
	delete expressionsTable;		expressionsTable	 = NULL;
	delete errorsTable;				errorsTable			 = NULL;
}

void SyntaxAnalyzer :: checkErrors() {
	if(errorsTable->size() != 0) {
		for(unsigned int i = 0; i < errorsTable->size(); i++) {
			cerr	<< "Syntactic error in line: " 
					<< errorsTable->at(i).errorLine << endl;
			if(errorsTable->at(i).errorNode->nodeType == Node :: SENTENCE_ER) {
				SentenceErrorNode* dummySE = 
						dynamic_cast<SentenceErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummySE->errorMessage.c_str() << endl;
			}
			else if(errorsTable->at(i).errorNode->nodeType == Node :: DECLARE_ST_ER) {
				StatementDeclarationErrorNode* dummySDE = 
						dynamic_cast<StatementDeclarationErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummySDE->errorMessage.c_str() << endl;
			}
			else if(errorsTable->at(i).errorNode->nodeType == Node :: FUNC_DEF_ER) {
				FunctionDefinitionErrorNode* dummyFDE = 
						dynamic_cast<FunctionDefinitionErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummyFDE->errorMessage.c_str() << endl;
			}
			else if(errorsTable->at(i).errorNode->nodeType == Node :: FUNC_DECLARE_ER) {
				FunctionPrototypeErrorNode* dummyFPE = 
						dynamic_cast<FunctionPrototypeErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummyFPE->errorMessage.c_str() << endl;
			}
			else if(errorsTable->at(i).errorNode->nodeType == Node :: EXPRESSION_ER) {
				ExpressionErrorNode* dummyEE = 
						dynamic_cast<ExpressionErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummyEE->errorMessage.c_str() << endl;
			}
			else if(errorsTable->at(i).errorNode->nodeType == Node :: STATEMENT_ER) {
				StatementErrorNode* dummyStE = 
						dynamic_cast<StatementErrorNode*>(errorsTable->at(i).errorNode);
				cerr << dummyStE->errorMessage.c_str() << endl;
			}
		}
		throw CompilerException("Syntactic tree was no built successfully because of error nodes.");
	}
}

SyntaxError :: SyntaxError(Node& _errorNode, int _errorLine) {
	errorNode = &_errorNode;
	errorLine = _errorLine;
}

void SyntaxAnalyzer :: showTokens() {
	lexer.showTokens();
}

void SyntaxAnalyzer :: showTree() {
	syntacticTree->showTree(0);
}

void SentenceNode :: free() {
	for(unsigned int i = 0; i < declarations->size(); i++) {
		declarations->at(i)->free();
		delete declarations->at(i);
		declarations->at(i) = NULL;
	}
	delete declarations;
	declarations = NULL;
}

void DeclarationStatementNode :: free() {
	for(unsigned int i = 0; i < variablesList->size(); i++) {
		variablesList->at(i)->free();
		delete variablesList->at(i);
		variablesList->at(i) = NULL;
	}
	delete variablesList;
	variablesList = NULL;
}

void VariableNode :: free() {
	if(initExpression != NULL) {
		initExpression->free();
		delete initExpression;
		initExpression = NULL;
	}
	if(arraySize != NULL) {
		arraySize->free();
		delete arraySize;
		arraySize = NULL;
	}
}

void BinaryOperationNode :: free() {
	leftOperand->free();
	rightOperand->free();
	delete leftOperand;
	leftOperand = NULL;
	delete rightOperand;
	rightOperand = NULL;
}

void UnaryOperationNode :: free() {
	operand->free();
	delete operand;
	operand = NULL;
}

void CompoundStatementNode :: free() {
	for(unsigned int i = 0; i < statementsList->size(); i++) {
		statementsList->at(i)->free();
		delete statementsList->at(i);
		statementsList->at(i) = NULL;
	}
	delete statementsList;
	statementsList = NULL;
}

void DoStatementNode :: free() {
	doBody->free();
	doCondition->free();
	delete doBody;
	doBody = NULL;
	delete doCondition;
	doCondition = NULL;
}

void WhileStatementNode :: free() {
	whileBody->free();
	whileCondition->free();
	delete whileBody;
	whileBody = NULL;
	delete whileCondition;
	whileCondition = NULL;
}

void ForStatementNode :: free() {
	if(forInitialization != NULL) {
		forInitialization->free();
		delete forInitialization;
		forInitialization = NULL;
	}

	if(forCondition != NULL) {
		forCondition->free();
		delete forCondition;
		forCondition = NULL;
	}

	if(forIteration != NULL) {
		forIteration->free();
		delete forIteration;
		forIteration = NULL;
	}

	if(forBody != NULL) {
		forBody->free();
		delete forBody;
		forBody = NULL;
	}
}

void IfStatementNode :: free() {
	ifBody->free();
	ifCondition->free();
	delete ifBody;
	ifBody = NULL;
	delete ifCondition;
	ifCondition = NULL;
}

void IfElseStatementNode :: free() {
	ifBody->free();
	ifCondition->free();
	elseBody->free();
	delete ifBody;
	ifBody = NULL;
	delete ifCondition;
	ifCondition = NULL;
	delete elseBody;
	elseBody = NULL;
}

void ReturnStatementNode :: free() {
	if(returnExpression != NULL) {
		returnExpression->free();
		delete returnExpression;
		returnExpression = NULL;
	}
}

void ExpressionStatementNode :: free() {
	if(expressionNode != NULL) {
		expressionNode->free();
		delete expressionNode;
		expressionNode = NULL;
	}
}

void ParameterNode:: free() {
	if(initExpression != NULL) {
		initExpression->free();
		delete initExpression;
		initExpression = NULL;
	}
}

void FunctionPrototypeNode :: free() {
	for(unsigned int i = 0; i < parametersList->size(); i++) {
		parametersList->at(i)->free();	
		delete parametersList->at(i);
		parametersList->at(i) = NULL;
	}
	delete parametersList;
	parametersList = NULL;
}

void FunctionDefinitionNode :: free() {
	for(unsigned int i = 0; i < parametersList->size(); i++) {
		parametersList->at(i)->free();	
		delete parametersList->at(i);
		parametersList->at(i) = NULL;
	}
	functionBody->free();
	delete functionBody;
	functionBody = NULL;
}

void FunctionCallNode :: free() {
	for(unsigned int i = 0; i < parametersList->size(); i++) {
		parametersList->at(i)->free();
		delete parametersList->at(i);
		parametersList->at(i) = NULL;
	}
	delete parametersList;
	parametersList = NULL;
}

void ArrayElementCallNode :: free() {
	indexExpression->free();
	delete indexExpression;
	indexExpression = NULL;
}

void BinaryOperationNode :: setParentLink() {
	if(leftOperand != NULL)
		leftOperand->parent	= this;
	if(rightOperand != NULL)
		rightOperand->parent = this;
}

void UnaryOperationNode :: setParentLink() {
	if(operand != NULL)
		operand->parent	= this;
}

void FunctionCallNode :: setParentLink() {
	for(unsigned int i = 0; i < parametersList->size(); i++)
		if(parametersList->at(i) != NULL)
			parametersList->at(i)->parent = this;
}

void VariableNode :: setParentLink() {
	if(initExpression != NULL)
		initExpression->parent = this;
	if(arraySize != NULL)
		arraySize->parent = this;
}

void CompoundStatementNode :: setParentLink() {
	for(unsigned int i = 0; i < statementsList->size(); i++)
		if(statementsList->at(i) != NULL)
			statementsList->at(i)->parent = this;
}

void DoStatementNode :: setParentLink() {
	if(doBody != NULL)
		doBody->parent = this;
	if(doCondition != NULL)
		doCondition->parent = this;
}

void WhileStatementNode :: setParentLink() {
	if(whileBody != NULL)
		whileBody->parent = this;
	if(whileCondition != NULL)
		whileCondition->parent = this;
}

void ForStatementNode :: setParentLink() {
	if(forInitialization != NULL)
		forInitialization->parent = this;
	if(forCondition != NULL)
		forCondition->parent = this;
	if(forIteration != NULL)
		forIteration->parent = this;
	if(forBody != NULL)
		forBody->parent = this;
}

void IfStatementNode :: setParentLink() {
	if(ifCondition != NULL)
		ifCondition->parent = this;
	if(ifBody != NULL)
		ifBody->parent = this;
}

void IfElseStatementNode :: setParentLink() {
	if(ifCondition != NULL)
		ifCondition->parent = this;
	if(ifBody != NULL)
		ifBody->parent = this;
	if(elseBody != NULL)
		elseBody->parent = this;
}

void ReturnStatementNode :: setParentLink() {
	if(returnExpression != NULL)
		returnExpression->parent = this;
}

void ExpressionStatementNode :: setParentLink() {
	if(expressionNode != NULL)
		expressionNode->parent = this;
}

void DeclarationStatementNode :: setParentLink() {
	for(unsigned int i = 0; i < variablesList->size(); i++)
		if(variablesList->at(i) != NULL)
			variablesList->at(i)->parent = this;
}

void ParameterNode :: setParentLink() {
	if(initExpression != NULL)
		initExpression->parent = this;
}

void FunctionPrototypeNode :: setParentLink() {
	for(unsigned int i = 0; i < parametersList->size(); i++)
		if(parametersList->at(i) != NULL)
			parametersList->at(i)->parent = this;
}

void FunctionDefinitionNode :: setParentLink() {
	if(functionBody != NULL)
		functionBody->parent = this;
	for(unsigned int i = 0; i < parametersList->size(); i++)
		if(parametersList->at(i) != NULL)
			parametersList->at(i)->parent = this;
}

void ArrayElementCallNode :: setParentLink() {
	if(indexExpression != NULL)
		indexExpression->parent = this;
}

void SentenceNode :: setParentLink() {
	for(unsigned int i = 0; i < declarations->size(); i++)
		if(declarations->at(i) != NULL)
			declarations->at(i)->parent = this;
}

Node :: Node() {
	parent = NULL, nodeType = ABSTRACT; 
}

void Node :: makeIndent(int& _indent) {
	for(int i = 0; i < _indent; ++i)
		cerr << "....";
	_indent++;
}

ParameterNode :: ParameterNode(Token& _tokenType, Token& _tokenId, 
		ExpressionNode& _initExpression) {
	tokenType		= _tokenType;
	tokenId			= _tokenId;
	initExpression	= &_initExpression;
	nodeType		= Node :: PARAMETER;
	setParentLink();
}

ParameterNode :: ParameterNode(Token& _tokenType, Token& _tokenId) {
	tokenType		= _tokenType;
	tokenId			= _tokenId;
	initExpression	= NULL;
	nodeType		= Node :: PARAMETER;
}

ParameterNode :: ParameterNode(Token& _tokenType) {
	tokenType		= _tokenType;
	tokenId			= Token();
	initExpression	= NULL;
	nodeType		= Node :: PARAMETER;
}

void ParameterNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " ParameterNode [tokenType: "
			<< tokenType.data << "] [tokenId: " 
			<< tokenId.data << "]" << endl << endl;
	if(initExpression != NULL) {
		for(int i = 0; i < _indent; ++i)
			cerr << "....";
		cerr << " DEFAULT VALUE" << endl << endl;
		initExpression->showTree(_indent+1);
	}
}

SentenceNode :: SentenceNode(vector<Node*>& _declarations) {
	declarations	= &_declarations;
	nodeType		= Node :: SENTENCE;
	parent			= new Node;
	setParentLink();
}

void SentenceNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " SentenceNode" << endl << endl;
	for(unsigned int i = 0; i < declarations->size(); ++i)
		declarations->at(i)->showTree(_indent);
}

FunctionPrototypeNode :: FunctionPrototypeNode(Token& _tokenType, Token& _tokenId) {
	tokenType		= _tokenType;
	tokenId			= _tokenId;
	parametersList  = new vector<ParameterNode*>;
	parametersList->clear();
	nodeType		= Node :: DECLARE_FUNC; 
}

FunctionPrototypeNode :: FunctionPrototypeNode(Token& _tokenType, Token& _tokenId, 
		vector<ParameterNode* >& _parametersList) {
	tokenType		= _tokenType;
	tokenId			= _tokenId;
	parametersList  = &_parametersList;
	nodeType		= Node :: DECLARE_FUNC; 
	setParentLink();
}

void FunctionPrototypeNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " FunctionPrototypeNode [tokenType: "
			<< tokenType.data << "] [tokenId: " 
			<< tokenId.data <<  "]" << endl << endl;
	if(parametersList != NULL)
		for(unsigned int i = 0; i < parametersList->size(); ++i)
			parametersList->at(i)->showTree(_indent);
}

FunctionDefinitionNode :: FunctionDefinitionNode(Token& _tokenType, Token& _tokenId, 
		vector<ParameterNode* >& _parametersList, CompoundStatementNode& _functionBody) {
	tokenType		= _tokenType;
	tokenId			= _tokenId;
	parametersList  = &_parametersList;
	functionBody	= &_functionBody;
	nodeType		= Node :: DEF_FUNC;
	setParentLink();
}

void FunctionDefinitionNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " FunctionDefinitionNode" << endl << endl;
	functionBody->showTree(_indent);
}

SentenceErrorNode :: SentenceErrorNode(string _errorMessage, ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: SENTENCE_ER;
}

StatementDeclarationErrorNode :: StatementDeclarationErrorNode(string _errorMessage, 
		ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: DECLARE_ST_ER;
}

FunctionDefinitionErrorNode :: FunctionDefinitionErrorNode(string _errorMessage, 
		ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: FUNC_DEF_ER; 
}

FunctionPrototypeErrorNode :: FunctionPrototypeErrorNode(string _errorMessage, 
		ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: FUNC_DECLARE_ER;
}

ExpressionErrorNode :: ExpressionErrorNode(string _errorMessage, ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: EXPRESSION_ER;
}

StatementErrorNode :: StatementErrorNode(string _errorMessage, ErrorType _errorType) {
	errorMessage	= _errorMessage;
	errorType		= _errorType;
	nodeType		= Node :: STATEMENT_ER;
}

CompoundStatementNode :: CompoundStatementNode(Token& _tokenStatement, 
		vector<StatementNode* >& _statementsList) {
	statementsList	= &_statementsList;
	tokenStatement	= _tokenStatement;
	nodeType		= Node :: COMPOUND_ST;
	setParentLink();
}

void CompoundStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " CompoundStatementNode" << endl << endl;
	for(unsigned int i = 0; i < statementsList->size(); ++i)
		statementsList->at(i)->showTree(_indent);
}

ContinueStatementNode :: ContinueStatementNode(Token& _tokenStatement) {
	tokenStatement	= _tokenStatement;
	nodeType		= Node :: CONTINUE_ST;
}

void ContinueStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " ContinueStatementNode" << endl << endl;
}

BreakStatementNode :: BreakStatementNode(Token& _tokenStatement) {
	tokenStatement	= _tokenStatement;
	nodeType		= Node :: BREAK_ST;
}

void BreakStatementNode::showTree(int _indent) {
	makeIndent(_indent);
	cerr << " BreakStatementNode" << endl << endl;
}

DoStatementNode :: DoStatementNode(Token& _tokenStatement, StatementNode& _doBody,
		ExpressionNode& _doCondition) {
	tokenStatement	= _tokenStatement;
	doBody			= &_doBody;
	doCondition		= &_doCondition;
	nodeType		= Node :: DO_ST;
	setParentLink();
}

void DoStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " DoStatementNode" << endl << endl;
	doBody->showTree(_indent);
	doCondition->showTree(_indent);
}

WhileStatementNode :: WhileStatementNode(Token& _tokenStatement, StatementNode& _whileBody,
		ExpressionNode& _whileCondition) {
	tokenStatement	= _tokenStatement;
	whileBody		= &_whileBody;
	whileCondition	= &_whileCondition;
	nodeType		= Node :: WHILE_ST;
	setParentLink();
}

void WhileStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " WhileStatementNode" << endl << endl;
	whileCondition->showTree(_indent);
	whileBody->showTree(_indent);
}

ForStatementNode :: ForStatementNode(Token& _tokenStatement, StatementNode& _forInitialization,
		StatementNode& _forCondition, ExpressionNode& _forIteration, 
		StatementNode&  _forBody) {
	tokenStatement		= _tokenStatement;
	forInitialization	= &_forInitialization;
	forCondition		= &_forCondition;
	forIteration		= &_forIteration;
	forBody				= &_forBody;
	nodeType			= Node :: FOR_ST;
	setParentLink();
}

void ForStatementNode::showTree(int _indent) {
	makeIndent(_indent);
	cerr << " ForStatementNode" << endl << endl;
	if(forInitialization != NULL)	forInitialization->showTree(_indent);
	if(forCondition != NULL)		forCondition->showTree(_indent);
	if(forIteration != NULL)		forIteration->showTree(_indent);
	else {
		for(int i = 0; i < _indent; ++i)
			cerr << "....";
		cerr << " EMPTY EXPRESSION" << endl << endl;
	}
	forBody->showTree(_indent);
}

IfStatementNode :: IfStatementNode(Token& _tokenStatement, ExpressionNode& _ifCondition,
		StatementNode& _ifBody) {
	tokenStatement		= _tokenStatement;
	ifCondition			= &_ifCondition;
	ifBody				= &_ifBody;
	nodeType			= Node :: IF_ST;
	setParentLink();
}

void IfStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " IfStatementNode" << endl << endl;
	ifCondition->showTree(_indent);
	ifBody->showTree(_indent);
}

IfElseStatementNode :: IfElseStatementNode(Token& _tokenStatement, ExpressionNode& _ifCondition,
		StatementNode& _ifBody, StatementNode& _elseBody) {
	tokenStatement	= _tokenStatement;
	ifCondition		= &_ifCondition;
	ifBody			= &_ifBody;
	elseBody		= &_elseBody;
	nodeType		= Node :: IF_ELSE_ST;
	setParentLink();
}

void IfElseStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " IfElseStatementNode" << endl << endl;
	ifCondition->showTree(_indent);
	ifBody->showTree(_indent);
	elseBody->showTree(_indent);
}

ReturnStatementNode :: ReturnStatementNode(Token& _tokenStatement, 
		ExpressionNode& _returnExpression) {
	tokenStatement		= _tokenStatement;
	returnExpression	= &_returnExpression;
	nodeType			= Node :: RETURN_ST;
	setParentLink();
}

ReturnStatementNode :: ReturnStatementNode(Token& _tokenStatement) {
	tokenStatement		= _tokenStatement;
	returnExpression	= NULL;
	nodeType			= Node :: RETURN_ST;
}

void ReturnStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr << " ReturnStatementNode" << endl << endl;
	if(returnExpression != NULL)	returnExpression->showTree(_indent);
}

ExpressionStatementNode :: ExpressionStatementNode(Token& _tokenStatement) {
	expressionNode	= NULL;
	tokenStatement	= _tokenStatement;
	nodeType		= Node :: EXPRESSION_ST;
}

ExpressionStatementNode :: ExpressionStatementNode(ExpressionNode& _expressionNode) {
	expressionNode	= &_expressionNode;
	tokenStatement	= Token();
	nodeType		= Node :: EXPRESSION_ST;
	setParentLink();
}

void ExpressionStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	if(expressionNode != NULL) {
		cerr << " ExpressionStatementNode" << endl << endl;
		expressionNode->showTree(_indent);
	}
	else
		cerr << " EMPTY OPERATOR" << endl << endl;
}

VariableNode :: VariableNode(Token& _tokenId, ExpressionNode& _initExpression, 
		ExpressionNode& _arraySize) {
	tokenId			= _tokenId;
	initExpression	= &_initExpression;
	arraySize		= &_arraySize;
	nodeType		= Node :: DECLARE_VAR;
	setParentLink();
}

void VariableNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " VariableNode [tokenId: "
			<< tokenId.data << "]" << endl << endl;
	if(arraySize != NULL) {
		for(int i = 0; i < _indent; ++i)
			cerr << "....";
		cerr << " ARRAY SIZE EXPRESSION" << endl << endl;
		arraySize->showTree(_indent+1);
	}
	if(initExpression != NULL)	{
		for(int i = 0; i < _indent; ++i)
			cerr << "....";
		cerr << " INITIALIZATION EXPRESSION" << endl << endl;
		initExpression->showTree(_indent+1);
	}
}

DeclarationStatementNode :: DeclarationStatementNode(Token& _tokenStatement, Token& _tokenType, 
		vector<VariableNode* >& _variablesList) {
	tokenStatement	= _tokenStatement;
	tokenType		= _tokenType;
	variablesList	= &_variablesList;
	nodeType		= Node :: DECLARE_ST;
	setParentLink();
}

void DeclarationStatementNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " DeclarationStatementNode[tokenType: "
			<< tokenType.data << "]" << endl << endl;
	for(unsigned int i = 0; i < variablesList->size(); ++i)
		variablesList->at(i)->showTree(_indent);
}

BinaryOperationNode :: BinaryOperationNode(Token& _token, ExpressionNode& _leftOperand, 
		ExpressionNode& _rightOperand) {
	operation		= _token;
	leftOperand		= &_leftOperand;
	rightOperand	= &_rightOperand;
	nodeType		= Node :: BIN_OP;
	setParentLink();
}

void BinaryOperationNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " BinaryOperationNode [operation: "
			<< operation.data << "]" << endl << endl;
	leftOperand->showTree(_indent);
	rightOperand->showTree(_indent);
}

UnaryOperationNode :: UnaryOperationNode(Token& _operation, ExpressionNode& _operand, 
		Order _order) {
	operation	= _operation;
	operand		= &_operand;
	order		= _order;
	nodeType	= Node :: UNARY_OP;
	setParentLink();
}

void UnaryOperationNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " UnaryOperationNode [operation: "
			<< operation.data << "]" << endl << endl;
	operand->showTree(_indent);
}

FunctionCallNode :: FunctionCallNode(Token& _tokenId, vector<ExpressionNode*>& _parametersList) {
	tokenId			= _tokenId;
	parametersList	= &_parametersList;
	nodeType		= Node :: USING_FUNC;
	definition		= NULL;
	prototype		= NULL;
	setParentLink();
}

FunctionCallNode :: FunctionCallNode(Token& _tokenId) {
	tokenId			= _tokenId;
	parametersList	= NULL;
	nodeType		= Node :: USING_FUNC;
	definition		= NULL;
	prototype		= NULL;
}

void FunctionCallNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " FunctionCallNode [tokenId: "
			<< tokenId.data << "]" << endl << endl;
	if(parametersList != NULL)
		for(unsigned int i = 0; i < parametersList->size(); ++i)
			parametersList->at(i)->showTree(_indent);
}

VariableCallNode :: VariableCallNode(Token& _tokenId) {
	tokenId		= _tokenId;
	nodeType	= Node :: USING_VAR;
	declaration = NULL;
	parameter	= NULL;
}

void VariableCallNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " VariableCallNode [tokenId: "
			<< tokenId.data << "]" << endl << endl;
}

ArrayElementCallNode :: ArrayElementCallNode(Token& _tokenId, ExpressionNode& _indexExpression) {
	tokenId			= _tokenId;
	indexExpression = &_indexExpression;
	nodeType		= Node :: USING_ARRAY;
	declaration		= NULL;
	parameter		= NULL;
	setParentLink();
}

void ArrayElementCallNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " ArrayElementCallNode [tokenId: "
			<< tokenId.data << "]" << endl << endl;
	for(int i = 0; i < _indent; ++i)
		cerr << "....";
	cerr << " ARRAY INDEX EXPRESSION" << endl << endl;
	indexExpression->showTree(_indent+1);
}

NumberNode :: NumberNode(Token& _tokenNumber) {
	tokenNumber = _tokenNumber;
	nodeType	= Node :: USING_NUMBER;
}

void NumberNode :: showTree(int _indent) {
	makeIndent(_indent);
	cerr	<< " NumberNode [tokenNumber: "
			<< tokenNumber.data << "]" << endl << endl;
}

// -------------------- Syntactic Tree functions -----------------------------

ExpressionNode* SyntaxAnalyzer :: checkPrimaryExpression() {
	Token						IdToken;
	Token						numberToken;
	ExpressionNode*				expressionNode = NULL;
	vector<ExpressionNode*>*	actualParameters = new vector<ExpressionNode*>;
	if(match(KWRD_TRUE) == 1 || match(KWRD_FALSE) == 1) {
		numberToken = *(lexer.iterator - 1);
		return new NumberNode(numberToken);
	}
	if(match(OPEN_BRACKET) == 1) {
		expressionNode = checkExpression();
		if(match(CLOSE_BRACKET) == 1)
			return expressionNode;
	}
	if(match(LEX_NUMBER) == 1) {
		numberToken = *(lexer.iterator - 1);
		return new NumberNode(numberToken);
	}
	if(match(LEX_ID) == 1) {
		IdToken = *(lexer.iterator - 1);
		if(match(OPEN_INDEX) == 1) {
			expressionNode = checkExpression();
			expressionsTable->push_back(expressionNode);
			if(match(CLOSE_INDEX) == 0) {
				ExpressionErrorNode* node = 
					new ExpressionErrorNode("Expression error: bad array index expression.\n",
						ExpressionErrorNode :: BAD_PRIMARY_EXPR);
				errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
				return node;
			}
			ArrayElementCallNode* node = new ArrayElementCallNode(IdToken, *expressionNode);
			idCallTable->push_back(node);
			return node;
		}
		if(match(OPEN_BRACKET) == 0) {
			VariableCallNode* node = new VariableCallNode(IdToken);
			idCallTable->push_back(node);
			return node;
		}
		else {
			while(match(CLOSE_BRACKET) == 0) {
				ExpressionNode* exprNode = new ExpressionNode;
				exprNode = checkAssignmentOperation();
				actualParameters->push_back(exprNode);
				expressionsTable->push_back(exprNode);
				if(match(COMMA) == 1) continue;
				if(match(CLOSE_BRACKET) == 1) break;
			}
		}
			FunctionCallNode* node = new FunctionCallNode(IdToken, *actualParameters);
			functionCallTable->push_back(node);
			return node;
	}
	ExpressionErrorNode* node = 
		new ExpressionErrorNode("Expression error: bad primary expression.\n",
			ExpressionErrorNode :: BAD_PRIMARY_EXPR); 
	errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
	return node;
}

ExpressionNode* SyntaxAnalyzer :: checkPostfixOperation() {
	Token				operation;		
	ExpressionNode*		operand;
	operand = checkPrimaryExpression();
	if(match(INC) == 1 || match(DCR) == 1) {
		operation = *(lexer.iterator - 1);
		return new UnaryOperationNode(operation, *operand, 
			UnaryOperationNode :: POST_ORDER);
	}
	return operand;
}

ExpressionNode* SyntaxAnalyzer :: checkPrefixOperation() {
	Token				operation;		
	ExpressionNode*		operand;
	if(match(INC) == 1 || match(DCR) == 1) {
		operation = *(lexer.iterator - 1);
		operand = checkPostfixOperation();
		return new UnaryOperationNode(operation, *operand, 
			UnaryOperationNode :: PRE_ORDER);
	}
	return checkPostfixOperation(); 
}

ExpressionNode* SyntaxAnalyzer :: checkUnaryOperation() {
	Token				operation;		
	ExpressionNode*		operand;
	if(match(PLUS) == 1 || match(MINUS) == 1 || 
			match(BOOL_NOT) == 1 || match(BIT_NOT) == 1) {
		operation = *(lexer.iterator - 1);
		operand = checkPrefixOperation();
		return new UnaryOperationNode(operation, *operand, 
			UnaryOperationNode :: PRE_ORDER);
	}
	return checkPrefixOperation();
}

ExpressionNode* SyntaxAnalyzer :: checkMulOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkUnaryOperation();
	ExpressionNode*		rightOperand;

	if(match(MUL) == 1 || match(DIV) == 1 || match(MOD) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkMulOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkAddOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkMulOperation();
	ExpressionNode*		rightOperand;

	if(match(ADD) == 1 || match(SUB) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkAddOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkShiftOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkAddOperation();
	ExpressionNode*		rightOperand;

	if(match(LEFT_BITSHIFT) == 1 || match(RIGHT_BITSHIFT) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkShiftOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkRelationOperation() { 
	Token				operation;
	ExpressionNode*		leftOperand = checkShiftOperation();
	ExpressionNode*		rightOperand;

	if(match(GREATER) == 1 || match(LESS) == 1 ||
			match(GREATER_OR_EQUALS) == 1 || match(LESS_OR_EQUALS) == 1) {

		operation = *(lexer.iterator - 1);
		rightOperand = checkRelationOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkEqualityOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkRelationOperation();
	ExpressionNode*		rightOperand;

	if(match(EQUALS) == 1 || match(NOT_EQUALS) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkEqualityOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkBitwiseAndOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkEqualityOperation();
	ExpressionNode*		rightOperand;

	if(match(BIT_AND) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkBitwiseAndOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkBitwiseXorOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkBitwiseAndOperation();
	ExpressionNode*		rightOperand;

	if(match(BIT_XOR) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkBitwiseXorOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkBitwiseOrOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkBitwiseXorOperation();
	ExpressionNode*		rightOperand;

	if(match(BIT_OR) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkBitwiseOrOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkBooleanAndOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkBitwiseOrOperation();
	ExpressionNode*		rightOperand;

	if(match(BOOL_AND) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkBooleanAndOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkBooleanOrOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkBooleanAndOperation();
	ExpressionNode*		rightOperand;

	if(match(BOOL_OR) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkBooleanOrOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkAssignmentOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkBooleanOrOperation();
	ExpressionNode*		rightOperand;

	if(match(ASSIGN) == 1 || match(ADD_ASSIGN) == 1 || 
			match(SUB_ASSIGN) == 1 || match(MUL_ASSIGN) == 1 ||
			match(DIV_ASSIGN) == 1 || match(MOD_ASSIGN) == 1 ||
			match(LEFT_BS_ASSIGN) == 1 || match(RIGHT_BS_ASSIGN) == 1 ||
			match(BIT_OR_ASSIGN)  == 1 || match(BIT_AND_ASSIGN)  == 1 ||
			match(BIT_NOT_ASSIGN) == 1 || match(BIT_XOR_ASSIGN)  == 1) {

		operation = *(lexer.iterator - 1);
		rightOperand = checkAssignmentOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkSequenceOperation() {
	Token				operation;
	ExpressionNode*		leftOperand = checkAssignmentOperation();
	ExpressionNode*		rightOperand;

	if(match(COMMA) == 1) {
		operation = *(lexer.iterator - 1);
		rightOperand = checkSequenceOperation();
		return new BinaryOperationNode(operation, *leftOperand, *rightOperand);
	}
	return leftOperand;
}

ExpressionNode* SyntaxAnalyzer :: checkExpression() {
	ExpressionNode* node = new ExpressionNode;
	node = checkSequenceOperation(); 
	return node;
}

DeclarationStatementNode* SyntaxAnalyzer :: checkVariableDeclaration() {
	Token						tokenType;
	Token						tokenId;
	vector<VariableNode* >*		variables = new vector<VariableNode*>;
	
	lexer.prevToken();
	lexer.prevToken(); // rollback to typename

	if(match(KWRD_VOID) == 1) {
		StatementDeclarationErrorNode* node = 
			new StatementDeclarationErrorNode("Variable declaration error: incorrect typename (maybe void?).\n",
				StatementDeclarationErrorNode :: BAD_TYPENAME);
		errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
		return node;
	}
	tokenType = *(lexer.iterator);
	lexer.nextToken();
	int declarationLine;
	do {
		if(match(LEX_ID) == 1) {
			tokenId = *(lexer.iterator - 1);
			declarationLine = lexer.getCurrentToken().line;
			ExpressionNode* arraySize	= NULL;
			ExpressionNode* dummy		= NULL;
			VariableNode*	node		= NULL;
			if(match(OPEN_INDEX) == 1) {
				arraySize = checkExpression();
				expressionsTable->push_back(arraySize);
				if(match(CLOSE_INDEX) == 0) {
					StatementDeclarationErrorNode* node = 
						new StatementDeclarationErrorNode("Variable declaration error: incorrect array size expression.\n",
							StatementDeclarationErrorNode :: BAD_EXPRESSION);
					errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
					return node;
				}
			}
			if(match(SEMICOLON) == 1) {
				node = new VariableNode(tokenId, *dummy, *arraySize);
				idDeclareTable->push_back(node);
				variables->push_back(node);
				break;	
			}
			if(match(COMMA) == 1) {
				node = new VariableNode(tokenId, *dummy, *arraySize);
				variables->push_back(node);
				idDeclareTable->push_back(node);
				continue;
			}
			if(match(ASSIGN) == 1) {
				ExpressionNode* exprNode = new ExpressionNode;
				lexer.prevToken();
				lexer.prevToken();
				exprNode = checkAssignmentOperation();
				expressionsTable->push_back(exprNode);
				node = new VariableNode(tokenId, *exprNode, *arraySize);
				idDeclareTable->push_back(node);
				variables->push_back(node);
				if(match(COMMA) == 1) continue;
				else if(match(SEMICOLON) == 1) break;
				else {
					StatementDeclarationErrorNode* node = 
						new StatementDeclarationErrorNode("Variable declaration error: missing ';'.\n",
							StatementDeclarationErrorNode :: BAD_SEMICOLON);
					errorsTable->push_back(SyntaxError(*node, declarationLine));
					return node;
				}
			}
		}
		else {
			StatementDeclarationErrorNode* node = 
				new StatementDeclarationErrorNode("Variable declaration error: missing or incorrect ID.\n",
					StatementDeclarationErrorNode :: BAD_ID);
			errorsTable->push_back(SyntaxError(*node, declarationLine));
			return node;
		}
	} while(match(SEMICOLON) == 0);
	return new DeclarationStatementNode(tokenType, tokenType, *variables);
}

FunctionPrototypeNode* SyntaxAnalyzer :: checkFunctionPrototype() {
	Token						tokenType;
	Token						tokenId;
	Token						tokenParameterId;
	Token						tokenParameterType;
	vector<ParameterNode* >*	parametersList	=	new vector<ParameterNode* >;

	lexer.prevToken();
	lexer.prevToken(); // rollback to typename

	if(	match(KWRD_CHAR) 	|| match(KWRD_INT)	||
		match(KWRD_FLOAT)	|| match(KWRD_VOID)	||	match(KWRD_BOOL))	{;}

	if(match(LEX_ID)) {;}

	tokenType	= *(lexer.iterator - 2);
	tokenId		= *(lexer.iterator - 1);

	match(OPEN_BRACKET);

	while(match(CLOSE_BRACKET) == 0) {
		if(match(KWRD_INT) == 1 || match(KWRD_FLOAT) == 1 ||
				match(KWRD_CHAR) == 1 || match(KWRD_BOOL) == 1)
			tokenParameterType = *(lexer.iterator - 1);
		else {
			FunctionPrototypeErrorNode* node = new 
					FunctionPrototypeErrorNode("Function declaration error: missing or incorrect typename.\n", 
					FunctionPrototypeErrorNode :: BAD_TYPENAME);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
		if(match(LEX_ID) == 1) {
			tokenParameterId = *(lexer.iterator - 1);
			if(match(ASSIGN) == 1) {
				ExpressionNode* exprNode = new ExpressionNode;
				lexer.prevToken();
				lexer.prevToken();
				exprNode = checkAssignmentOperation();
				expressionsTable->push_back(exprNode);
				parametersList->push_back(new ParameterNode(tokenParameterType, tokenParameterId, 
						*exprNode));
			}
			else 
				parametersList->push_back(new ParameterNode(tokenParameterType, tokenParameterId));	
		}
		else 
			parametersList->push_back(new ParameterNode(tokenParameterType));
		if(match(COMMA) == 1 && match(CLOSE_BRACKET) == 1) {
			FunctionPrototypeErrorNode* node = new 
				FunctionPrototypeErrorNode("Function declaration error: missing or incorrect typename.\n", 
				FunctionPrototypeErrorNode :: BAD_TYPENAME);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
	}
	if(match(SEMICOLON) == 1) { 
		if(parametersList->empty() == 1) {
			FunctionPrototypeNode* node = new FunctionPrototypeNode(tokenType, tokenId);
			functionDeclareTable->push_back(node);
			return node;
		}
		else {
			FunctionPrototypeNode* node = new FunctionPrototypeNode(tokenType, tokenId, *parametersList);
			functionDeclareTable->push_back(node);
			return node;
		}
	} else { 
		// return NULL if it is definition
		delete parametersList;
		return NULL;
	}
}

FunctionDefinitionNode* SyntaxAnalyzer :: checkFunctionDefinition() {
	Token						tokenType;
	Token						tokenId;
	Token						tokenParameterId;
	Token						tokenParameterType;
	vector<ParameterNode* >*	parametersList	=	new vector<ParameterNode* >;
	CompoundStatementNode*		functionBody;
	FunctionDefinitionNode*		functionDefinition;

	lexer.prevToken();
	lexer.prevToken(); // rollback to typename

	if(	match(KWRD_CHAR) 	|| match(KWRD_INT)	||
		match(KWRD_FLOAT)	|| match(KWRD_VOID)	||	match(KWRD_BOOL))	{;}

	if(match(LEX_ID)) {;}

	tokenType	= *(lexer.iterator - 2);
	tokenId		= *(lexer.iterator - 1);

	match(OPEN_BRACKET);

	while(match(CLOSE_BRACKET) == 0) {
		if(match(KWRD_INT) == 1 || match(KWRD_FLOAT) == 1 ||
				match(KWRD_CHAR) == 1 || match(KWRD_BOOL) == 1)
			tokenParameterType = *(lexer.iterator - 1);
		else {
			FunctionDefinitionErrorNode* node = new FunctionDefinitionErrorNode("Function definition error: missing or incorrect typename.\n", 
					FunctionDefinitionErrorNode :: BAD_TYPENAME);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
		if(match(LEX_ID) == 1) {
			tokenParameterId = *(lexer.iterator - 1);
			if(match(ASSIGN) == 1) {
				ExpressionNode* exprNode = new ExpressionNode;
				lexer.prevToken();
				lexer.prevToken();
				exprNode = checkAssignmentOperation();
				expressionsTable->push_back(exprNode);
				parametersList->push_back(new ParameterNode(tokenParameterType, tokenParameterId, 
					*exprNode));
			}
			else 
				parametersList->push_back(new ParameterNode(tokenParameterType, tokenParameterId));	
		}
		else 
			parametersList->push_back(new ParameterNode(tokenParameterType));
		if(match(COMMA) == 1 && match(CLOSE_BRACKET) == 1) {
			FunctionDefinitionErrorNode* node = new 
				FunctionDefinitionErrorNode("Function definition error: missing or incorrect typename.\n", 
				FunctionDefinitionErrorNode :: BAD_TYPENAME);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
	}
	if(match(OPEN_BRACE) == 1) {
		lexer.prevToken();
		functionBody = checkCompoundStatement();
		functionDefineTable->push_back(functionDefinition = 
				new FunctionDefinitionNode(tokenType, tokenId, *parametersList, *functionBody));
		return functionDefinition;
	}
	return NULL;
}

CompoundStatementNode* SyntaxAnalyzer :: checkCompoundStatement() {
	vector<StatementNode* >*	statementsList = new vector<StatementNode* >;

	match(OPEN_BRACE);

	Token dummy = *(lexer.iterator - 1);
	while(match(CLOSE_BRACE) == 0) {
		statementsList->push_back(checkStatement());	
	}
	return new CompoundStatementNode(dummy, *statementsList);
}

StatementNode* SyntaxAnalyzer :: checkStatement() {
	vector<StatementNode* >*	statementsList = new vector<StatementNode* >;
	
	Token*			tokenStatement		= new Token();
	StatementNode*	doBody				= NULL;
	ExpressionNode* doCondition			= NULL;
	ExpressionNode* whileCondition		= NULL;
	StatementNode*	whileBody			= NULL;
	StatementNode*	forInitialization	= NULL;
	StatementNode*	forCondition		= NULL;
	ExpressionNode*	forIteration		= NULL;
	StatementNode*	forBody				= NULL;
	ExpressionNode*	ifCondition			= NULL;
	StatementNode*	ifBody				= NULL;
	StatementNode*	elseBody			= NULL;
	ExpressionNode*	returnExpression	= NULL;
	ExpressionNode*	expressionNode		= NULL;

	int statementLine;

	if(match(SEMICOLON) == 1) 
		return new ExpressionStatementNode(*(lexer.iterator - 1));
	else if(match(OPEN_BRACE) == 1) {
		Token dummy = *(lexer.iterator - 1);
		while(match(CLOSE_BRACE) == 0) {
			statementsList->push_back(checkStatement());	
		}
		return new CompoundStatementNode(dummy, *statementsList);
	}
	else if(match(CONTINUE) == 1 && match(SEMICOLON) == 1) 
		return new ContinueStatementNode(*(lexer.iterator - 1));	
	else if(match(BREAK) == 1 && match(SEMICOLON) == 1)
		return new BreakStatementNode(*(lexer.iterator - 1));
	else if(match(DO) == 1) {
		*tokenStatement = *(lexer.iterator - 1);
		statementLine = lexer.getCurrentToken().line;
		doBody = checkStatement();
		if(match(WHILE) == 1 && match(OPEN_BRACKET) == 1) {
			doCondition = checkExpression();
			expressionsTable->push_back(doCondition);
			if(match(CLOSE_BRACKET) == 1 && match(SEMICOLON) == 1)
				return new DoStatementNode(*tokenStatement, *doBody, 
					*doCondition);
			else {
				StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'DO-WHILE' statement.\n",
						StatementErrorNode :: BAD_DO);
				errorsTable->push_back(SyntaxError(*node, statementLine));
				return node;
			}
		}
		else {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'DO-WHILE' statement.\n",
					StatementErrorNode :: BAD_DO);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
	}
	else if(match(WHILE) == 1 && match(OPEN_BRACKET) == 1) {
		*tokenStatement = *(lexer.iterator - 2);
		statementLine = tokenStatement->line;
		whileCondition = checkExpression();
		expressionsTable->push_back(whileCondition);
		if(match(CLOSE_BRACKET) == 1) {
			whileBody = checkStatement();
			return new WhileStatementNode(*tokenStatement, *whileBody, *whileCondition);
		}
		else {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'WHILE' statement.\n",
					StatementErrorNode :: BAD_WHILE);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
	}
	else if(match(FOR) == 1 && match(OPEN_BRACKET) == 1) {
		*tokenStatement = *(lexer.iterator - 2);
		statementLine = tokenStatement->line;
		forInitialization	= checkStatement();
		if(	forInitialization->nodeType != Node :: EXPRESSION_ST &&
			forInitialization->nodeType != Node :: DECLARE_ST) {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'FOR' statement (bad for-init expression).\n",
					StatementErrorNode :: BAD_FOR);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
		forCondition		= checkStatement();
		if(	forCondition->nodeType != Node :: EXPRESSION_ST &&
			forCondition->nodeType != Node :: DECLARE_ST) {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'FOR' statement (bad for-condition expression).\n",
					StatementErrorNode :: BAD_FOR);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
		if(match(CLOSE_BRACKET) == 0) {
			forIteration = checkExpression();
			expressionsTable->push_back(forIteration);
			if(match(CLOSE_BRACKET) == 0) {
				StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'FOR' statement.\n",
						StatementErrorNode :: BAD_FOR);
				errorsTable->push_back(SyntaxError(*node, statementLine));
				return node;
			}
		}
		forBody = checkStatement();
		return new ForStatementNode(*tokenStatement, *forInitialization, 
				*forCondition, *forIteration, *forBody);
	}
	else if(match(IF) == 1 && match(OPEN_BRACKET) == 1) {
		*tokenStatement = *(lexer.iterator - 2);
		ifCondition = checkExpression();
		expressionsTable->push_back(ifCondition);
		statementLine = lexer.getCurrentToken().line;
		if(match(CLOSE_BRACKET) == 0) {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'IF-ELSE' statement.\n",
					StatementErrorNode :: BAD_IF_ELSE);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
		ifBody = checkStatement();
		if(match(ELSE) == 0)
			return new IfStatementNode(*tokenStatement, *ifCondition, *ifBody);
		else {
			elseBody = checkStatement();
			return new IfElseStatementNode(*tokenStatement, *ifCondition, 
					*ifBody, *elseBody);
		}
	}
	else if(match(RETURN) == 1) {
		statementLine = lexer.getCurrentToken().line;
		if(match(SEMICOLON) == 1) 
			return new ReturnStatementNode(*(lexer.iterator - 1));
		returnExpression = checkExpression();
		expressionsTable->push_back(returnExpression);
		if(match(SEMICOLON) == 1) 
			return new ReturnStatementNode(*(lexer.iterator - 1), *returnExpression);
		else {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'RETURN' statement (missing ';'?).\n",
					StatementErrorNode :: BAD_RETURN);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
	}
	else if(match(KWRD_CHAR) == 1 || match(KWRD_FLOAT) == 1 
		|| match(KWRD_INT) == 1 || match(KWRD_BOOL) == 1) {
		statementLine = lexer.getCurrentToken().line;
		lexer.nextToken();
		return checkVariableDeclaration();
	}
	else {
		expressionNode = checkExpression();
		expressionsTable->push_back(expressionNode);
		statementLine = lexer.getCurrentToken().line;
		if(match(SEMICOLON) == 0) {
			StatementErrorNode* node = new StatementErrorNode("Statement error: bad 'EXPRESSION' statement (missing ';'?).\n",
					StatementErrorNode :: BAD_EXPRESSION);
			errorsTable->push_back(SyntaxError(*node, statementLine));
			return node;
		}
		return new ExpressionStatementNode(*expressionNode);
	}
}

SentenceNode* SyntaxAnalyzer :: checkSentence() {
	vector<Node*>*	declarations	= new vector<Node*>;
	Node*			declaration		= NULL;
	vector<Token>::iterator rollback;

	while(match(LEX_EOF) == 0) {
		if(match(KWRD_CHAR) == 0 && match(KWRD_FLOAT) == 0 &&
			match(KWRD_INT) == 0 && match(KWRD_VOID) == 0 && match(KWRD_BOOL) == 0) {
			SentenceErrorNode* node = new SentenceErrorNode("Sentence error: missing or incorrect typename.\n",
					SentenceErrorNode :: BAD_TYPENAME);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
		rollback = lexer.iterator - 1;
		if(match(LEX_ID) == 0) {
			SentenceErrorNode* node = new SentenceErrorNode("Sentence error: missing or incorrect ID.\n",
					SentenceErrorNode :: BAD_ID);
			errorsTable->push_back(SyntaxError(*node, lexer.getCurrentToken().line));
			return node;
		}
		if(match(OPEN_BRACKET) == 1) {
			rollback = lexer.iterator - 1;
			lexer.prevToken();
			declaration = checkFunctionPrototype();
			if(declaration == NULL) {
				lexer.iterator = rollback;
				lexer.loadToken();
				declaration = checkFunctionDefinition();
				declarations->push_back(declaration);
			}
			else {
				declarations->push_back(declaration);
			}
		}
		else {
			declaration = checkVariableDeclaration();
			declarations->push_back(declaration);
		}
	}
	return new SentenceNode(*declarations);
}

bool SyntaxAnalyzer :: match(const char* _data) {
	if(lexer.getCurrentToken().data == _data) {		
		if(lexer.getCurrentToken().data != "EOF")
			lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(const string _data) {
	if(lexer.getCurrentToken().data == _data) {
		if(lexer.getCurrentToken().data != "EOF")
			lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer::match(IdentifierType _type) {
	if(lexer.getCurrentToken().identifierType == _type) {
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(NumberType _type) {
	if(lexer.getCurrentToken().numberType == _type) {
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(DelimiterType _type) {
	if(lexer.getCurrentToken().delimiterType == _type) {
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(BracketType _type) {
	if(lexer.getCurrentToken().bracketType == _type) {
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(OperationType _type) {
	if(lexer.getCurrentToken().operationType == _type)	 {
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(KeywordType _type) {
	if(lexer.getCurrentToken().keywordType == _type)	{
		lexer.nextToken();
		return true;
	}
	else return false;
}

bool SyntaxAnalyzer :: match(TokenType _type) {
	if(lexer.getCurrentToken().tokenType == _type)	{ 
		if(lexer.getCurrentToken().tokenType != LEX_EOF)
			lexer.nextToken();
		return true;
	}
	else return false;
}