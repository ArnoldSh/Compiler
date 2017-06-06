// TokenParser.cpp

#include "TokenParser.h"
#include "CompilerException.h"

#define CHECK_UNARY_OPERATION (currentToken.bracketType == OPEN_BRACKET || \
	currentToken.delimiterType == SEMICOLON || \
	currentToken.delimiterType == COMMENT_LINE || \
	currentToken.operationType >= ASSIGN && \
	currentToken.operationType <= BIT_XOR_ASSIGN)

#define CHECK_HEXADECIMAL (source[sourceIndex] >= 'a' && \
	source[sourceIndex] <= 'f' || \
	source[sourceIndex] >= 'A' && \
	source[sourceIndex] <= 'F' || \
	isdigit(source[sourceIndex]))

#define CHECK_OCTAL (isdigit(source[sourceIndex]) && \
	source[sourceIndex] != '8' && \
	source[sourceIndex] != '9')

#define CHECK_KEYWORD_TYPE (currentToken.keywordType == KWRD_INT || \
	currentToken.keywordType == KWRD_FLOAT || \
	currentToken.keywordType == KWRD_CHAR || \
	currentToken.keywordType == KWRD_VOID)

using namespace token_types;

Keyword keywordTable[] = {	{"continue",	CONTINUE},		// 0
							{"if",			IF},			// 1
							{"return",		RETURN},		// 2
							{"do",			DO},			// 3
							{"int",			KWRD_INT},		// 4
							{"break",		BREAK},			// 5
							{"float",		KWRD_FLOAT},	// 6
							{"char",		KWRD_CHAR},		// 7
							{"else",		ELSE},			// 8
							{"while",		WHILE},			// 9
							{"for",			FOR},			// 10
							{"const",		KWRD_CONST},	// 11
							{"void",		KWRD_VOID},		// 12	
							{"bool",		KWRD_BOOL},		// 13	
							{"true",		KWRD_TRUE},		// 14
							{"false",		KWRD_FALSE}};	// 15

void TokenParser :: showTokens() {
	printf("%-4c %-16s %-16s %-16s %-4s %-4s", '#', "Token", "Type", "Subtype", "Line", "Position");
	cerr << endl;

	char*	tokenType;
	char*	tokenSubtype;

	for(unsigned int i = 0; i < tokens->size(); i++) {
		switch(tokens->at(i).tokenType) {
			case NONE_TOKEN:		tokenType = "NONE_TOKEN";			break;
			case LEX_ID:			tokenType = "LEX_ID";				break;
			case LEX_NUMBER:		tokenType = "LEX_NUMBER";			break;
			case LEX_DELIM:			tokenType = "LEX_DELIM";			break;
			case LEX_BRACKET:		tokenType = "LEX_BRACKET";			break;
			case LEX_OP:			tokenType = "LEX_OP";				break;
			case LEX_KEYWORD:		tokenType = "LEX_KEYWORD";			break;
			case LEX_EOF:			tokenType = "LEX_EOF";				break;
		}
		switch(tokens->at(i).identifierType) {
			case NONE_ID:			tokenSubtype = "NONE_ID";				break;
			case VARIABLE:			tokenSubtype = "VARIABLE";				break;
			case FUNCTION:			tokenSubtype = "FUNCTION";				break;
			case LABEL:				tokenSubtype = "LABEL";					break;
			case NONE_NUM:			tokenSubtype = "NONE_NUM";				break;
			case INTEGER:			tokenSubtype = "INTEGER";				break;
			case FLOAT:				tokenSubtype = "FLOAT";					break;
			case CHAR:				tokenSubtype = "CHAR";					break;
			case NONE_DELIM:		tokenSubtype = "NONE_DELIM";			break;
			case SEMICOLON:			tokenSubtype = "SEMICOLON";				break;
			case QUOTE:				tokenSubtype = "QUOTE";					break;
			case COMMENT_LINE:		tokenSubtype = "COMMENT_LINE";			break;
			case COMMENT_PART:		tokenSubtype = "COMMENT_PART";			break;
			case NONE_BRACKET:		tokenSubtype = "NONE_BRACKET";			break;
			case OPEN_BRACKET:		tokenSubtype = "OPEN_BRACKET";			break;
			case CLOSE_BRACKET:		tokenSubtype = "CLOSE_BRACKET";			break;
			case OPEN_BRACE:		tokenSubtype = "OPEN_BRACE";			break;
			case CLOSE_BRACE:		tokenSubtype = "CLOSE_BRACE";			break;
			case OPEN_INDEX:		tokenSubtype = "OPEN_INDEX";			break;
			case CLOSE_INDEX:		tokenSubtype = "CLOSE_INDEX";			break;
			case NONE_OP:			tokenSubtype = "NONE_OP";				break;
			case COMMA:				tokenSubtype = "COMMA";					break;
			case ADD:				tokenSubtype = "ADD";					break;
			case SUB:				tokenSubtype = "SUB";					break;
			case MUL:				tokenSubtype = "MUL";					break;
			case DIV:				tokenSubtype = "DIV";					break;
			case MOD:				tokenSubtype = "MOD";					break;
			case MINUS:				tokenSubtype = "MINUS";					break;
			case PLUS:				tokenSubtype = "PLUS";					break;
			case BOOL_OR:			tokenSubtype = "BOOL_OR";				break;
			case BOOL_AND:			tokenSubtype = "BOOL_AND";				break;
			case BOOL_NOT:			tokenSubtype = "BOOL_NOT";				break;
			case BIT_OR:			tokenSubtype = "BIT_OR";				break;
			case BIT_AND:			tokenSubtype = "BIT_AND";				break;
			case BIT_XOR:			tokenSubtype = "BIT_XOR";				break;
			case BIT_NOT:			tokenSubtype = "BIT_NOT";				break;
			case INC:				tokenSubtype = "INC";					break;
			case DCR:				tokenSubtype = "DCR";					break;
			case EQUALS:			tokenSubtype = "EQUALS";				break;
			case NOT_EQUALS:		tokenSubtype = "NOT_EQUALS";			break;
			case GREATER:			tokenSubtype = "GREATER";				break;
			case GREATER_OR_EQUALS:	tokenSubtype = "GREATER_OR_EQUALS";		break;
			case LESS:				tokenSubtype = "LESS";					break;
			case LESS_OR_EQUALS:	tokenSubtype = "LESS_OR_EQUALS";		break;
			case LEFT_BITSHIFT:		tokenSubtype = "LEFT_BITSHIFT";			break;
			case RIGHT_BITSHIFT:	tokenSubtype = "RIGHT_BITSHIFT";		break;
			case ASSIGN:			tokenSubtype = "ASSIGN";				break;
			case ADD_ASSIGN:		tokenSubtype = "ADD_ASSIGN";			break;
			case SUB_ASSIGN:		tokenSubtype = "SUB_ASSIGN";			break;
			case MUL_ASSIGN:		tokenSubtype = "MUL_ASSIGN";			break;
			case DIV_ASSIGN:		tokenSubtype = "DIV_ASSIGN";			break;
			case MOD_ASSIGN:		tokenSubtype = "MOD_ASSIGN";			break;
			case LEFT_BS_ASSIGN:	tokenSubtype = "LEFT_BS_ASSIGN";		break;
			case RIGHT_BS_ASSIGN:	tokenSubtype = "RIGHT_BS_ASSIGN";		break;
			case BIT_OR_ASSIGN:		tokenSubtype = "BIT_OR_ASSIGN";			break;
			case BIT_AND_ASSIGN:	tokenSubtype = "BIT_AND_ASSIGN";		break;
			case BIT_NOT_ASSIGN:	tokenSubtype = "BIT_NOT_ASSIGN";		break;
			case BIT_XOR_ASSIGN:	tokenSubtype = "BIT_XOR_ASSIGN";		break;
			case NONE_KEYWORD:		tokenSubtype = "NONE_KEYWORD";			break;
			case CONTINUE:			tokenSubtype = "CONTINUE";				break;
			case IF:				tokenSubtype = "IF";					break;
			case RETURN:			tokenSubtype = "RETURN";				break;
			case DO:				tokenSubtype = "DO";					break;
			case KWRD_INT:			tokenSubtype = "KWRD_INT";				break;
			case BREAK:				tokenSubtype = "BREAK";					break;
			case KWRD_FLOAT:		tokenSubtype = "KWRD_FLOAT";			break;
			case KWRD_CHAR:			tokenSubtype = "KWRD_CHAR";				break;
			case ELSE:				tokenSubtype = "ELSE";					break;
			case WHILE:				tokenSubtype = "WHILE";					break;
			case FOR:				tokenSubtype = "FOR";					break;
			case KWRD_CONST:		tokenSubtype = "CONST";					break;
			case KWRD_VOID:			tokenSubtype = "KWRD_VOID";				break;
			case KWRD_BOOL:			tokenSubtype = "KWRD_BOOL";				break;
			case KWRD_TRUE:			tokenSubtype = "TRUE";					break;
			case KWRD_FALSE:		tokenSubtype = "FALSE";					break;
		}
		printf("%-4d %-16s %-16s %-16s %-4d %-4d", 
				i+1, 
				tokens->at(i).data.c_str(), 
				tokenType, 
				tokenSubtype,
				tokens->at(i).line, 
				tokens->at(i).pos);
		cerr << endl;
	}
	cerr << endl;
}

Token :: Token() {
	data.clear();
	tokenType			= NONE_TOKEN;
	identifierType		= NONE_ID;
	pos					= 0;
	line				= 0;
}

TokenParser :: TokenParser() {
	tokens = new vector<Token>;
	sourceIndex	= 0;
	copySource();
	currentToken.pos = currentToken.line = 1;
	scan();
	if(bracketStack.empty() == false)
		throw CompilerException("Brackets using error.");
	iterator = tokens->begin();
	currentToken = *iterator;
} 

TokenParser :: ~TokenParser() {
	free(source);		source = NULL;
	delete tokens;		tokens = NULL;
}

void TokenParser :: copySource() {
	FILE* file;
	string text;
	fopen_s(&file, INPUT_FILE, "r");
	int i = 0;
	while(!feof(file)) {
		text += fgetc(file);
		i++;
	}
	text += '\0'; 
	i++;
	source = (char*)calloc(sourceSize = i, sizeof(char));
	for(int j = 0; j < i; j++) {
		source[j] = text.at(j);
	}
	fclose(file);
} 

bool TokenParser :: checkBrackets(Token* _bracket) {
	Token pop;
	pop = bracketStack.top();
	bracketStack.pop();
	switch(_bracket->bracketType) {
	case CLOSE_BRACE: 
		if(pop.bracketType == OPEN_BRACE)	return true;
		else								return false;
	case CLOSE_BRACKET:
		if(pop.bracketType == OPEN_BRACKET)	return true;
		else								return false;
	case CLOSE_INDEX:
		if(pop.bracketType == OPEN_INDEX)	return true;
		else								return false;
	default:
		return false;						
	}
}

void TokenParser :: nextToken() {
	currentToken = *(++iterator);
}

void TokenParser :: prevToken() {
	currentToken = *(--iterator);
}

void TokenParser :: scanToken() {

	if(currentToken.data.length()) { 						// initialization
		currentToken.pos += currentToken.data.length();
	}
	currentToken.data.clear();

	while(source[sourceIndex] == ' ' || source[sourceIndex] == '\n' || source[sourceIndex] == '\t') {
		if(source[sourceIndex] == ' ') {
			currentToken.pos++;
		}
		if(source[sourceIndex] == '\n') {
			currentToken.pos = 1; currentToken.line++;
		}
		if(source[sourceIndex] == '\t') {
			currentToken.pos += 4 - currentToken.pos % 4 + 1 ;
		}
		sourceIndex++;			
	}

	switch(source[sourceIndex]) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9': case '.':
		int E, point, sign, digit;			// flags of recorded 'E'/'e', '.' and '+'/'-' 
		E = point = sign = digit = 0;
		while(isdigit(source[sourceIndex])	|| source[sourceIndex] == '.' || 
				source[sourceIndex] == 'E'	|| source[sourceIndex] == 'e' || 
				source[sourceIndex] == '+'	|| source[sourceIndex] == '-') {
			if(isdigit(source[sourceIndex])) {
				digit = 1;
			} 
			if(source[sourceIndex] == '.') {
				if(point == 1) {
					throw CompilerException("Point using error in floating point format: ", 
							currentToken.line, currentToken.pos);
				}
				point = 1;
			}
			else if(source[sourceIndex] == 'E' || source[sourceIndex] == 'e') {
				if(E == 1) {
					throw CompilerException("Exponent using error in floating point format: ", 
							currentToken.line, currentToken.pos);
				}
				E = 1;
			}
			else if(source[sourceIndex] == '+' || source[sourceIndex] == '-') {
				if(!(isdigit(source[sourceIndex+1])))
					break;	
				if(sign == 1) {
					throw CompilerException("Sign using error in floating point format: ", 
							currentToken.line, currentToken.pos);
				}
				sign = 1;
			}
			currentToken.data += source[sourceIndex];	sourceIndex++;		
			currentToken.pos++;
		}
		if(isalpha(source[sourceIndex])) {
			throw CompilerException("Illegal token: ", currentToken.line, currentToken.pos);
		}
		currentToken.tokenType = LEX_NUMBER;
		if(E || point) 
			currentToken.numberType = FLOAT;
		else 
			currentToken.numberType = INTEGER;
		break;
	case '{':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = OPEN_BRACE;
		currentToken.data += source[sourceIndex];		
		bracketStack.push(currentToken);					sourceIndex++;		currentToken.pos++;	
		break;
	case '}':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = CLOSE_BRACE;
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;	
		if(bracketStack.empty() == 1 || checkBrackets(&currentToken) == 0) 
			throw CompilerException("Brackets using error:", currentToken.line, currentToken.pos - 1);
		break;
	case '(':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = OPEN_BRACKET;
		currentToken.data += source[sourceIndex];		
		bracketStack.push(currentToken);					sourceIndex++;		currentToken.pos++;
		break;
	case ')':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = CLOSE_BRACKET;
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;	
		if(bracketStack.empty() == 1 || checkBrackets(&currentToken) == 0) 
			throw CompilerException("Brackets using error:", currentToken.line, currentToken.pos - 1);
		break;
	case '[':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = OPEN_INDEX;
		currentToken.data += source[sourceIndex];		
		bracketStack.push(currentToken);					sourceIndex++;		currentToken.pos++;
		break;
	case ']':
		currentToken.tokenType = LEX_BRACKET;				currentToken.bracketType = CLOSE_INDEX;
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;	
		if(bracketStack.empty() == 1 || checkBrackets(&currentToken) == 0) 
			throw CompilerException("Brackets using error:", currentToken.line, currentToken.pos - 1);
		break;
	case '+':
		currentToken.tokenType = LEX_OP;
		if(CHECK_UNARY_OPERATION) {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
			if(source[sourceIndex] == '+') {
				currentToken.data += source[sourceIndex];	sourceIndex++;		currentToken.pos++;	
				currentToken.operationType = INC;
				break;
			}
			currentToken.operationType = PLUS;
			break;
		}
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		currentToken.operationType = ADD;
		if(source[sourceIndex] == '+') {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
			currentToken.operationType = INC;
		}
		else if(source[sourceIndex] == '=') {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
			currentToken.operationType = ADD_ASSIGN;		
		}
		break;
	case '-': 
		currentToken.tokenType = LEX_OP;
		if(CHECK_UNARY_OPERATION) {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
			if(source[sourceIndex] == '-') {
				currentToken.data += source[sourceIndex];	sourceIndex++;		currentToken.pos++;	
				currentToken.operationType = DCR;
				break;
			}
			currentToken.operationType = MINUS;
			break;
		}
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		currentToken.operationType = SUB;
		if(source[sourceIndex] == '-') {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
			currentToken.operationType = DCR;
		}
		else if(source[sourceIndex] == '=') {
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
			currentToken.operationType = SUB_ASSIGN;		
		}
		break;
	case '=':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;	
		if(source[sourceIndex] == '=') {
			currentToken.operationType = EQUALS;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
		}
		else 
			currentToken.operationType = ASSIGN;	
		currentToken.tokenType = LEX_OP;
		break;
	case '!':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		currentToken.operationType = BOOL_NOT;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = NOT_EQUALS;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
		}
		currentToken.tokenType = LEX_OP;
		break;
	case '*': 
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = MUL_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else 
			currentToken.operationType = MUL;	
		currentToken.tokenType = LEX_OP;
		break;
	case '/':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = DIV_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else if(source[sourceIndex] == '/') {
			currentToken.data.clear();						sourceIndex++;		currentToken.pos++;
			currentToken.delimiterType = COMMENT_LINE;
			currentToken.tokenType = LEX_DELIM;
			while(source[sourceIndex] != '\n') 
				sourceIndex++;
			break;
		}
		else if(source[sourceIndex] == '*') {
			currentToken.data.clear();						sourceIndex++;		currentToken.pos++;
			int commentPos	= currentToken.pos - 2;
			int commentLine = currentToken.line;
			currentToken.delimiterType = COMMENT_PART;
			currentToken.tokenType = LEX_DELIM;
			while(source[sourceIndex - 1] != '*' || source[sourceIndex] != '/') {
				if(source[sourceIndex] == '\n') {
					currentToken.pos = 1;
					currentToken.line++;
				}
				if(source[sourceIndex] == -1) break;
				currentToken.pos++;
				sourceIndex++;
			}
			if(source[sourceIndex] == -1) 
				throw CompilerException("Missing end of comment part '*/': ", commentLine, commentPos);
			sourceIndex++;
			break;
		}
		else {
			currentToken.operationType = DIV;	
			currentToken.tokenType = LEX_OP;
		}
		break;
	case '%':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = MOD_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else
			currentToken.operationType = MOD;	
		currentToken.tokenType = LEX_OP;
		break;
	case '|': 
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = BIT_OR_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else if(source[sourceIndex] == '|') {
			currentToken.operationType = BOOL_OR;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else 
			currentToken.operationType = BIT_OR;
		currentToken.tokenType = LEX_OP;
		break;
	case '&': 
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = BIT_AND_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else if(source[sourceIndex] == '|') {
			currentToken.operationType = BOOL_AND;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else 
			currentToken.operationType = BIT_AND;
		currentToken.tokenType = LEX_OP;
		break;
	case '^':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = BIT_XOR_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else 
			currentToken.operationType = BIT_XOR;	
		currentToken.tokenType = LEX_OP;
		break;
	case '~':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = BIT_NOT_ASSIGN;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else 
			currentToken.operationType = BIT_NOT;
		currentToken.tokenType = LEX_OP;
		break;
	case '<':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		currentToken.operationType = LESS;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = LESS_OR_EQUALS;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else if (source[sourceIndex] == '<') {
			currentToken.operationType = LEFT_BITSHIFT;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
			if(source[sourceIndex] == '=') {
				currentToken.operationType = LEFT_BS_ASSIGN;
				currentToken.data += source[sourceIndex];	sourceIndex++;		currentToken.pos++;		
			}
		}
		currentToken.tokenType = LEX_OP;
		break;
	case '>':
		currentToken.data += source[sourceIndex];			sourceIndex++;		currentToken.pos++;
		currentToken.operationType = GREATER;
		if(source[sourceIndex] == '=') {
			currentToken.operationType = GREATER_OR_EQUALS;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		}
		else if (source[sourceIndex] == '>') {
			currentToken.operationType = RIGHT_BITSHIFT;
			currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;	
			if(source[sourceIndex] == '=') {
				currentToken.operationType = RIGHT_BS_ASSIGN;
				currentToken.data += source[sourceIndex];	sourceIndex++;		currentToken.pos++;		
			}
		}
		currentToken.tokenType = LEX_OP;
		break;
	case ',':
		currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		currentToken.operationType = COMMA;
		currentToken.tokenType = LEX_OP;
		break;
	case ';':
		currentToken.data += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
		currentToken.delimiterType = SEMICOLON;
		currentToken.tokenType = LEX_DELIM;
		break;
	case '\'':
		currentToken.tokenType = LEX_NUMBER;			sourceIndex++;		currentToken.pos++;
		currentToken.numberType = CHAR;
		if(source[sourceIndex] == '\\') {
			sourceIndex++;		currentToken.pos++;
			int startChar = currentToken.pos - 1;
			if(isdigit(source[sourceIndex])) {
				string buffer = "";
				if(source[sourceIndex] == '0') {
					buffer += source[sourceIndex];		sourceIndex++;		currentToken.pos++;
					if(source[sourceIndex] == 'x' || source[sourceIndex] == 'X') {
						buffer += source[sourceIndex];	sourceIndex++;		currentToken.pos++;
						while(source[sourceIndex] != '\'') {
							if(CHECK_HEXADECIMAL) {
								buffer += source[sourceIndex];	sourceIndex++;	currentToken.pos++;
							}
							else if(source[sourceIndex] != '\'') 
								throw CompilerException("Illegal hexadecimal code format\'s symbol:", 
										currentToken.line, startChar);
						}
					}
					else {
						buffer += source[sourceIndex];	sourceIndex++;		currentToken.pos++;
						while(source[sourceIndex] != '\'') {
							if(CHECK_OCTAL) {
								buffer += source[sourceIndex];	sourceIndex++;	currentToken.pos++;
							}
							else if(source[sourceIndex] != '\'')
								throw CompilerException("Illegal octal code format\'s symbol:", 
										currentToken.line, startChar);
						}
					}
				}
				else if(source[sourceIndex] != '0' && isdigit(source[sourceIndex])) {
					buffer += source[sourceIndex];	sourceIndex++;		currentToken.pos++;
					while(source[sourceIndex] != '\'') {
						if(isdigit(source[sourceIndex])) {
							buffer += source[sourceIndex];	sourceIndex++;	currentToken.pos++;
						}
						else if(source[sourceIndex] != '\'')
							throw CompilerException("Illegal decimal code format\'s symbol:", 
									currentToken.line, startChar);
					}
					buffer += 'd';	// means that char variable contains 
									// a code of character by decimal number
				}
				currentToken.data = buffer;
			}
		}
		else  {
			currentToken.data += source[sourceIndex];
			sourceIndex++;		currentToken.pos++;
		}
		if(source[sourceIndex] != '\'') {
			throw CompilerException("Illegal character format: ", 
					currentToken.line, currentToken.pos - 2);
		}
		sourceIndex++;		currentToken.pos++;
		break;
	case EOF:
		currentToken.tokenType = LEX_EOF;
		currentToken.identifierType = NONE_ID;
		currentToken.data += "EOF\0";
		break;
	default:
		if(isalpha(source[sourceIndex]) || source[sourceIndex] == '_') {
			currentToken.data += source[sourceIndex];			sourceIndex++;	currentToken.pos++;
			while(isalnum(source[sourceIndex]) || source[sourceIndex] == '_') {
				currentToken.data += source[sourceIndex];		sourceIndex++;	currentToken.pos++;
			}
		}
		else {
			throw CompilerException("Illegal unknown symbol: ", currentToken.line, currentToken.pos);
		}
		if(source[sourceIndex] == '(')		currentToken.identifierType = FUNCTION;
		else								currentToken.identifierType = VARIABLE;
											currentToken.tokenType		= LEX_ID;
		for(int i = 0; i < TOTAL_KEYWORDS; i++) {			// check token - is KEYWORD?
			if(currentToken.data == keywordTable[i].data) {
				currentToken.identifierType = NONE_ID;
				currentToken.tokenType = LEX_KEYWORD;
				currentToken.keywordType = keywordTable[i].keywordType;
				break;
			}
		}
	}
	if(currentToken.data.length() != 0 && currentToken.tokenType != LEX_EOF)
		currentToken.pos -= currentToken.data.length();
	if(currentToken.delimiterType != COMMENT_LINE && 
			currentToken.delimiterType != COMMENT_PART)
		tokens->push_back(currentToken);
}

void TokenParser :: scan() {
	while(currentToken.tokenType != LEX_EOF) {
		scanToken();
	}
}

const Token& TokenParser :: getCurrentToken() {
	return currentToken;
}

void TokenParser :: loadToken() {
	currentToken = *iterator;
}
