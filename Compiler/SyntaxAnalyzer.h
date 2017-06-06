// SyntaxAnalyzer.h

#ifndef __SYNTAX_ANALYZER_H
#define __SYNTAX_ANALYZER_H

#include "TokenParser.h"

using namespace token_types;

class Node {
public:
	enum	NodeType {	ABSTRACT = 0,	BIN_OP,			UNARY_OP, 
						USING_VAR,		USING_FUNC,		USING_NUMBER,
						DECLARE_ST,		DECLARE_FUNC,	DEF_FUNC,
						PARAMETER,		SENTENCE,		COMPOUND_ST,
						CONTINUE_ST,	BREAK_ST,		DO_ST,
						WHILE_ST,		FOR_ST,			IF_ST, 
						IF_ELSE_ST,		RETURN_ST,		EXPRESSION_ST,
						DECLARE_VAR,	USING_ARRAY,	SENTENCE_ER,
						DECLARE_ST_ER,  FUNC_DEF_ER,	FUNC_DECLARE_ER,
						EXPRESSION_ER,	STATEMENT_ER}	
			nodeType;
	Node*	parent;
	void			makeIndent(int&);
	virtual void	showTree(int)	{}
	virtual void	free()			{}
	virtual void	setParentLink()	{}
	Node(); 
};

class ExpressionNode: public Node {};

class DeclarationNode: public Node {};

class BinaryOperationNode: public ExpressionNode {
public:
	Token				operation;
	ExpressionNode*		leftOperand;
	ExpressionNode*		rightOperand;

	BinaryOperationNode(Token&, ExpressionNode&, ExpressionNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class UnaryOperationNode: public ExpressionNode {
public:	
	Token				operation;
	ExpressionNode*		operand;
	enum	Order		{PRE_ORDER, POST_ORDER}
			order;
	
	UnaryOperationNode(Token&, ExpressionNode&, Order);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class PrimaryExpressionNode: public ExpressionNode {};

class NumberNode: public PrimaryExpressionNode {
public:	
	Token	tokenNumber;
	
	NumberNode(Token&);

	virtual void	showTree(int);
};

class VariableNode: public Node {
public:
	Token						tokenId;
	ExpressionNode*				initExpression;
	ExpressionNode*				arraySize;
	
	VariableNode(Token&, ExpressionNode&, ExpressionNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class StatementNode: public Node {
public:
	Token				tokenStatement;
};

class CompoundStatementNode: public StatementNode {
public:
	vector<StatementNode* >*	statementsList;

	CompoundStatementNode(Token&, vector<StatementNode* >&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class ContinueStatementNode: public StatementNode {
public:
	ContinueStatementNode(Token& );

	virtual void showTree(int);
};

class BreakStatementNode: public StatementNode {
public:
	BreakStatementNode(Token& );

	virtual void showTree(int);
};

class DoStatementNode: public StatementNode {
public:
	StatementNode*				doBody;
	ExpressionNode*				doCondition;

	DoStatementNode(Token&, StatementNode&, ExpressionNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class WhileStatementNode: public StatementNode {
public:
	ExpressionNode*				whileCondition;
	StatementNode*				whileBody;

	WhileStatementNode(Token&, StatementNode&, ExpressionNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class ForStatementNode: public StatementNode {
public:
	StatementNode*				forInitialization;
	StatementNode*				forCondition;
	ExpressionNode*				forIteration;
	StatementNode*				forBody;

	ForStatementNode(Token&, StatementNode&, StatementNode&,
			ExpressionNode&, StatementNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class IfStatementNode: public StatementNode {
public:
	ExpressionNode*				ifCondition;
	StatementNode*				ifBody;

	IfStatementNode(Token&, ExpressionNode&,
			StatementNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class IfElseStatementNode: public StatementNode {
public:
	ExpressionNode*				ifCondition;
	StatementNode*				ifBody;
	StatementNode*				elseBody;

	IfElseStatementNode(Token&, ExpressionNode&,
			StatementNode&, StatementNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class ReturnStatementNode: public StatementNode {
public:
	ExpressionNode*				returnExpression;

	ReturnStatementNode(Token&, ExpressionNode&);
	ReturnStatementNode(Token&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class ExpressionStatementNode: public StatementNode {
public:
	ExpressionNode*				expressionNode;

	ExpressionStatementNode(Token&);
	ExpressionStatementNode(ExpressionNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class DeclarationStatementNode: public StatementNode {
public:
	Token						tokenType;
	vector<VariableNode* >*		variablesList;

	DeclarationStatementNode() {;}
	DeclarationStatementNode(Token&, Token&, vector<VariableNode* >&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class ParameterNode: public Node {
public:
	Token				tokenType;
	Token				tokenId;
	ExpressionNode*		initExpression;

	ParameterNode(Token&, Token&, ExpressionNode&);
	ParameterNode(Token&, Token&);
	ParameterNode(Token&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class FunctionDeclarationNode: public DeclarationNode {};

class FunctionPrototypeNode: public FunctionDeclarationNode {
public:
	Token						tokenType;
	Token						tokenId;
	vector<ParameterNode* >*	parametersList;

	FunctionPrototypeNode() {;}
	FunctionPrototypeNode(Token&, Token&);
	FunctionPrototypeNode(Token&, Token&, vector<ParameterNode* >&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class FunctionDefinitionNode: public FunctionDeclarationNode {
public:
	Token						tokenType;
	Token						tokenId;
	vector<ParameterNode* >*	parametersList;
	CompoundStatementNode*		functionBody;

	FunctionDefinitionNode() {;}
	FunctionDefinitionNode(Token&, Token&, vector<ParameterNode* >&, 
			CompoundStatementNode&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class FunctionCallNode: public PrimaryExpressionNode {
public:
	Token					 tokenId;
	vector<ExpressionNode*>* parametersList;
	
	FunctionDefinitionNode* definition;
	FunctionPrototypeNode*	prototype;
	
	FunctionCallNode(Token&, vector<ExpressionNode*>&);
	FunctionCallNode(Token&);

	virtual void	showTree(int);
	virtual void	free();
	virtual void	setParentLink();
};

class VariableCallNode: public PrimaryExpressionNode {
public:	
	Token						tokenId;
	
	DeclarationStatementNode*	declaration;
	ParameterNode*				parameter;
	
	VariableCallNode() {;}
	VariableCallNode(Token&);

	virtual void showTree(int);
};

class ArrayElementCallNode: public VariableCallNode {
public:
	ExpressionNode*				indexExpression;
	
	ArrayElementCallNode(Token&, ExpressionNode&);

	virtual void showTree(int);
	virtual void free();
	virtual void setParentLink();
};

class SentenceNode: public Node {
public:
	vector<Node*>*	declarations;

	SentenceNode() {;}
	SentenceNode(vector<Node*>&);

	virtual	void showTree(int);
	virtual void free();
	virtual void setParentLink();
};

class SentenceErrorNode: public SentenceNode {
public:
	enum	 ErrorType { BAD_TYPENAME = 0, BAD_ID}
			 errorType;
	string	 errorMessage;

	SentenceErrorNode(string, ErrorType);
};

class FunctionPrototypeErrorNode: public FunctionPrototypeNode {
public:
	enum	 ErrorType { BAD_TYPENAME = 0, BAD_ID}
			 errorType;
	string	 errorMessage;
	
	FunctionPrototypeErrorNode(string, ErrorType);
};

class FunctionDefinitionErrorNode: public FunctionDefinitionNode {
public:
	enum	 ErrorType { BAD_TYPENAME = 0, BAD_ID}
			 errorType;
	string	 errorMessage;

	FunctionDefinitionErrorNode(string, ErrorType);
};

class StatementDeclarationErrorNode: public DeclarationStatementNode {
public:
	enum	 ErrorType { BAD_TYPENAME = 0, BAD_ID, 
						 BAD_EXPRESSION, BAD_SEMICOLON}
			 errorType;
	string	 errorMessage;

	StatementDeclarationErrorNode(string, ErrorType);
};

class ExpressionErrorNode: public ExpressionNode {
public:
	enum	 ErrorType { BAD_PRIMARY_EXPR = 0 }
			 errorType;
	string	 errorMessage;

	ExpressionErrorNode(string, ErrorType);
};

class StatementErrorNode: public StatementNode {
public:
	enum	 ErrorType { BAD_DO = 0,  BAD_WHILE, BAD_FOR,
						 BAD_IF_ELSE, BAD_RETURN, 
						 BAD_EXPRESSION }
			 errorType;
	string	 errorMessage;

	StatementErrorNode(string, ErrorType);
};

struct SyntaxError {
		Node*	errorNode;
		int		errorLine;
		SyntaxError(Node&, int);
	};

class SyntaxAnalyzer {
public:
	SyntaxAnalyzer();
	~SyntaxAnalyzer();

	void showTokens();
	void showTree();

	vector<VariableCallNode*>*			idCallTable;
	vector<FunctionCallNode*>*			functionCallTable;
	vector<VariableNode*>*				idDeclareTable;
	vector<FunctionPrototypeNode*>*		functionDeclareTable;
	vector<FunctionDefinitionNode*>*	functionDefineTable;

	vector<ExpressionNode*>*		expressionsTable;

	vector<SyntaxError>*			errorsTable;

	SentenceNode*	syntacticTree;
private:
	TokenParser	lexer;

	void	checkErrors();

	bool	match(const char* );
	bool	match(const string );
	bool	match(IdentifierType );
	bool	match(NumberType );
	bool	match(DelimiterType );
	bool	match(BracketType );
	bool	match(OperationType );
	bool	match(KeywordType );
	bool	match(token_types::TokenType );

	ExpressionNode*	checkPrimaryExpression();
	ExpressionNode*	checkPostfixOperation();
	ExpressionNode*	checkPrefixOperation();
	ExpressionNode*	checkUnaryOperation();
	ExpressionNode*	checkMulOperation();
	ExpressionNode*	checkAddOperation();
	ExpressionNode*	checkShiftOperation();
	ExpressionNode*	checkRelationOperation();
	ExpressionNode*	checkEqualityOperation();
	ExpressionNode*	checkBitwiseAndOperation();
	ExpressionNode*	checkBitwiseXorOperation();
	ExpressionNode*	checkBitwiseOrOperation();
	ExpressionNode*	checkBooleanAndOperation();
	ExpressionNode*	checkBooleanOrOperation();
	ExpressionNode*	checkAssignmentOperation();
	ExpressionNode*	checkSequenceOperation();
	ExpressionNode* checkExpression();

	CompoundStatementNode*	checkCompoundStatement();
	StatementNode*			checkStatement();

	DeclarationStatementNode*	checkVariableDeclaration();
	FunctionPrototypeNode*		checkFunctionPrototype();
	FunctionDefinitionNode*		checkFunctionDefinition();

	SentenceNode*	checkSentence();
};

#endif // __SYNTAX_ANALYZER_H