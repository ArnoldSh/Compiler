// TokenParser.h

#ifndef __TOKEN_PARSER_H
#define __TOKEN_PARSER_H

#define TOTAL_KEYWORDS		16
#define	INPUT_FILE			"input.txt"

#include <ctype.h>
#include <string>
#include <vector>
#include <fstream>
#include <iostream>
#include <stack>

using namespace std;

namespace token_types {

	enum IdentifierType			{NONE_ID = 0, VARIABLE, FUNCTION, LABEL};

	enum NumberType				{NONE_NUM = 10, INTEGER, FLOAT, CHAR};

	enum DelimiterType			{NONE_DELIM = 20, SEMICOLON, QUOTE, COMMENT_LINE,
								COMMENT_PART};

	enum BracketType			{NONE_BRACKET = 30, OPEN_BRACKET, CLOSE_BRACKET, 
								OPEN_BRACE, CLOSE_BRACE, OPEN_INDEX, CLOSE_INDEX};

	enum OperationType			{NONE_OP = 40, COMMA, ADD, SUB, MUL, DIV, MOD, MINUS, 
								PLUS, BOOL_OR, BOOL_AND, BOOL_NOT, BIT_OR, BIT_AND, 
								BIT_XOR, BIT_NOT, INC, DCR, EQUALS, NOT_EQUALS, 
								GREATER, GREATER_OR_EQUALS, 
								LESS, LESS_OR_EQUALS, LEFT_BITSHIFT, RIGHT_BITSHIFT, 
								ASSIGN, ADD_ASSIGN, SUB_ASSIGN, MUL_ASSIGN, 
								DIV_ASSIGN, MOD_ASSIGN, LEFT_BS_ASSIGN, 
								RIGHT_BS_ASSIGN, BIT_OR_ASSIGN, BIT_AND_ASSIGN,
								BIT_NOT_ASSIGN, BIT_XOR_ASSIGN};

	enum KeywordType			{NONE_KEYWORD = 100, CONTINUE, IF, RETURN, DO, 
								KWRD_INT, BREAK, KWRD_FLOAT, KWRD_CHAR, ELSE, WHILE, 
								FOR, KWRD_CONST, KWRD_VOID, KWRD_BOOL, KWRD_TRUE, KWRD_FALSE};

	enum TokenType				{NONE_TOKEN = 120, LEX_ID, LEX_NUMBER, LEX_DELIM, 
								LEX_BRACKET, LEX_OP, LEX_KEYWORD, LEX_EOF};
}

using namespace token_types;

struct Token {

	string							data;
	token_types::TokenType			tokenType;
	union {
		token_types::IdentifierType		identifierType;
		token_types::NumberType			numberType;
		token_types::DelimiterType		delimiterType;
		token_types::BracketType		bracketType;
		token_types::OperationType		operationType;
		token_types::KeywordType		keywordType;	
	};
	int			pos;		// start position of lexeme
	int			line;		// line where lexeme is located
	Token();
};

struct Keyword {
	string						data;
	token_types::KeywordType	keywordType;
};

class TokenParser {	
private:
	char*			source;						// source code
	int				sourceIndex;				// current index in the array 'source'
	int				sourceSize;					// size (in symbols) of source code
	
	stack<Token>	bracketStack;				// for lookup of brackets disparity		
	
	Token			currentToken;				// current lexeme
	
	void			copySource();				// load source code from file to LEXER
	bool			checkBrackets(Token* );	
	void			scanToken();				// lookup of an one token
	void			scan();						// scan all tokens

public:
	vector<Token>*			tokens;				// vector of scanned tokens
	
	vector<Token>::iterator iterator;
	
	const Token&			getCurrentToken();	

	void					nextToken();		// load to currentToken *(++iterator)
	void					loadToken();		
	void					prevToken();		// load to currentToken *(--iterator)
	void					showTokens();
	
	TokenParser();
	~TokenParser();

};
	
#endif // __TOKEN_PARSER_H