
#include <algorithm>
#include <atomic>
#include <complex>
#include <filesystem>
#include <locale>
#include <new>
#include <queue>
#include "lex.h"
#include "parserInterp.h"
#include "val.h"
#include <map>
#include <string>
#include <utility>
#include <variant>

map<string, bool> defVar;
map<string, Token> SymTable;
map<string, Value> TempsResults;

vector<string> * Ids_list;
string progname;
bool putln;

namespace Parser {
	bool pushed_back = false;
	LexItem	pushed_token;

	static LexItem GetNextToken(istream& in, int& line) {
		if( pushed_back ) {
			pushed_back = false;
			return pushed_token;
		}
		return getNextToken(in, line);
	}

	static void PushBackToken(LexItem & t) {
		if( pushed_back ) {
			abort();
		}
		pushed_back = true;
		pushed_token = t;	
	}

}

static int error_count = 0;

int ErrCount()
{
    return error_count;
}

void ParseError(int line, string msg)
{
	++error_count;
	cout << line << ": " << msg << endl;
}

bool Prog(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok != PROCEDURE) {
		ParseError(line, "Incorrect compilation file.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != IDENT) {
		ParseError(line, "Missing Procedure Name.");
		return false;
	}
	progname = tok.GetLexeme();
	defVar[tok.GetLexeme()] = false;

	tok = Parser::GetNextToken(in, line);
	if (tok != IS) {
		ParseError(line, "Missing IS Keyword");
		return false;
	}

	if (ProcBody(in, line)) {
		cout << endl;
		cout << "(" << "DONE" << ")" << endl;
		return true;
	}
	return false;
}

bool ProcBody(istream& in, int& line) {
	if (!DeclPart(in, line)) {
		ParseError(line, "Incorrect procedure body.");
		ParseError(line, "Incorrect Procedure Definition.");
		return false;
	}
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok != BEGIN) {
		ParseError(line, "Missing BEGIN Keyword");
		return false;
	}
	if (!StmtList(in, line)) {
		ParseError(line, "Incorrect Proedure Body.");
		ParseError(line, "Incorrect Procedure Definition.");
		return false;
	}
	tok = Parser::GetNextToken(in, line);
	if (tok != END) {
		ParseError(line, "Missing END Keyword");
		return false;
	}
	tok = Parser::GetNextToken(in, line);
	if (tok != IDENT) {
		ParseError(line, "Missing ProcName");
		return false;
	}
	if (tok.GetLexeme().compare(progname)) {
		ParseError(line, "Procedure name mismatch in closing end identifier.");
		ParseError(line, "Incorrect Procedure Definition.");
		return false;
	}
	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		ParseError(line, "Missing Semicolon");
		return false;
	}
	return true;
}

bool StmtList(istream& in, int& line)
{
	bool status;
	LexItem tok;
	status = Stmt(in, line);
	tok = Parser::GetNextToken(in, line);
	while(status && (tok != END && tok != ELSIF && tok != ELSE))
	{
		Parser::PushBackToken(tok);
		status = Stmt(in, line);
		tok = Parser::GetNextToken(in, line);
	}
	if(!status)
	{
		ParseError(line, "Syntactic error in statement list.");
		return false;
	}
	Parser::PushBackToken(tok); 
	return true;
}

bool DeclStmt(istream& in, int& line) {
	vector<string> variables;
	LexItem tok = Parser::GetNextToken(in, line);
	string varname = tok.GetLexeme();
	variables.push_back(varname);
	if (tok != IDENT) {
		ParseError(line, "Missing Identifier");
		return false;
	}

	if (defVar.count(varname) != 0) {
		ParseError(line, "Variable Redefinition.");
		return false;
	} 
	defVar[varname] = false;
	SymTable[varname] = tok.GetToken(); 

	while (true) {
		tok = Parser::GetNextToken(in, line);
		if (tok == COMMA) {
			tok = Parser::GetNextToken(in, line);
			if (tok != IDENT) {
				ParseError(line, "Missing Identifier after Comma");
				ParseError(line, "Incorrect identifiers list in Declaration Statement.");
				return false;
			}
			varname = tok.GetLexeme();
			variables.push_back(varname);
			if (defVar.count(varname) != 0) {
				ParseError(line, "Variable Redefinition.");
				ParseError(line, "Incorrect identifiers list in Declaration Statement.");
				return false;
			} 
			defVar[varname] = false;
			SymTable[varname] = tok.GetToken();
		} else {
			Parser::PushBackToken(tok);
			break;
		}
	}
	
	tok = Parser::GetNextToken(in, line);
	if (tok != COLON) {
		ParseError(line, "Missing comma in declaration statement.");
		ParseError(line, "Incorrect Identifiers list in Declaration Statement.");
		return false;
	}

	LexItem type;

	tok = Parser::GetNextToken(in, line);
	if (tok == INT || tok == STRING || tok == FLOAT || tok == CHAR || tok == BOOL) {
		type = tok;
	} else {
		ParseError(line, "Missing declaration type.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	Value left, right;
	if (tok == LPAREN) {
		Parser::PushBackToken(tok);
		if (!Range(in, line, left, right)) {
			ParseError(line, "Range is wrong");
			return false;
		}
	} else {
		Parser::PushBackToken(tok);
	}
	
	tok = Parser::GetNextToken(in, line);
	Value res;
	if (tok == ASSOP) {
		if (!Expr(in, line, res)) {
			ParseError(line, "Invalid initialization.");
			return false;
		}
		for (const auto& var : variables) {
			TempsResults[var] = res;
		}
	} else {
		Parser::PushBackToken(tok);
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		ParseError(line - 1, "Missing semicolon");
		return false;
	}

	for (const auto& var : variables) {
		SymTable[var] = type.GetToken();
	}

	return true;
}

bool DeclPart(istream& in, int& line) {
	bool status = false;
	LexItem tok;
	status = DeclStmt(in, line);
	if(status)
	{
		tok = Parser::GetNextToken(in, line);
		if(tok == BEGIN )
		{
			Parser::PushBackToken(tok);
			return true;
		}
		else 
		{
			Parser::PushBackToken(tok);
			status = DeclPart(in, line);
		}
	}
	else
	{
		ParseError(line, "Non-recognizable Declaration Part.");
	}
	return status;
}


bool Var(istream& in, int& line, LexItem & idtok) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok != IDENT) {
		return false;
	}
	if (defVar.count(tok.GetLexeme()) != 1) {
		return false;
	}
	idtok = tok;
	return true;
}

bool Type(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok == INT || tok == FLOAT || tok == BOOL || tok == STRING || tok == CHAR) {
		return true;
	}
	Parser::PushBackToken(tok);
	ParseError(line, "Incorrect Declaration Type.");
	return false;
}

bool Stmt(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (defVar.count(tok.GetLexeme()) != 0) {
		Parser::PushBackToken(tok);
		return AssignStmt(in, line);
	} else if (tok == PUTLN) {
		putln = true;
		return PrintStmts(in, line);
	} else if (tok == PUT) {
		return PrintStmts(in, line);
	} else if (tok == GET) {
		return GetStmt(in, line);
	} else if (tok == IF) {
		return IfStmt(in, line);
	} else {
		ParseError(line, "Invalid statement.");
		return false;
	}

}

bool PrintStmts(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	string varname;
	Value val;
	if (tok != LPAREN) {
		ParseError(line, "Missing Left Parenthesis");
		ParseError(line, "Invalid put statement.");
		return false;
	}


	tok = Parser::GetNextToken(in, line);
	if (tok == IDENT) {
		varname = tok.GetLexeme();
		Parser::PushBackToken(tok);
		LexItem tokahead = Parser::GetNextToken(in, line);
		if (tokahead == RPAREN) {
			if (defVar.count(varname) == 0) {
				ParseError(line, "Undefined variable in print statement.");
				return false;
			}
			if (TempsResults.count(varname) == 0) {
				ParseError(line, "Invalid use of an uninitialized variable.");
				ParseError(line, "Incorrect operand");
				return false;
			}
			val = TempsResults[varname];
		} else {
			Parser::PushBackToken(tok);
			if (!Expr(in, line, val)) {
				ParseError(line, "Missing expression for an output statement.");
				ParseError(line, "Invalid put statement.");
				return false;
			}
		}
	} else if (tok == SCONST) {
		val = Value(tok.GetLexeme());
	} else {
		Parser::PushBackToken(tok);
		if (!Expr(in, line, val)) {
			ParseError(line, "Missing expression for an output statement.");
			ParseError(line, "Invalid put statement.");
			return false;
		}

		tok = Parser::GetNextToken(in, line);
		if (tok != RPAREN) {
			ParseError(line, "Missing Right Parenthesis.");
			ParseError(line, "Invalid put statement.");
			return false;
		}
	}

	switch(val.GetType()) {
		case VINT: 
			cout << val;
			break;
		case VREAL:
			cout << val;
			break;
		case VSTRING:
			cout << val;
			break;
		case VCHAR:
			cout << val;
			break;
		case VBOOL:
			cout << val;
			break;
		default:
			ParseError(line, "Missing expression for an output statement.");
			ParseError(line, "Invalid put statement.");
			return false;
	}

	if (putln) {
		cout << '\n';
		putln = false;
	}
	
	tok = Parser::GetNextToken(in, line);
	if (tok != RPAREN) {
		ParseError(line, "Missing Right Parenthesis");
		ParseError(line, "Invalid put statement.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		line--;
		ParseError(line, "Missing semicolon at end of statement");
		ParseError(line, "Invalid put statement.");
		return false;
	}
	
	return true;
}

bool GetStmt(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok != LPAREN) {
		ParseError(line, "Missing Left Parenthesis.");
		return false;
	}

	LexItem idtok;
	if (!Var(in, line, idtok)) {
		ParseError(line, "Missing identifier in get statement.");
		return false;
	} 
	
	string varname = idtok.GetLexeme();
	Token var = SymTable[varname];

	tok = Parser::GetNextToken(in, line);

	if (tok != RPAREN) {
		ParseError(line, "Missing Right Parenthesis.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		ParseError(line, "Missing semicolon at the end of statement.");
		ParseError(line, "Invalid get statement.");
		return false;
	}

	string inputvar;
	getline(cin, inputvar);

	try {
		switch(var) {
			case INT: 
				TempsResults[varname] = Value(stoi(inputvar));
				break;
			case FLOAT:
				TempsResults[varname] = Value(stod(inputvar));
				break;
			case BOOL:
				if (inputvar == "true") {
					TempsResults[varname] = Value(true);
				} else if (inputvar == "false") {
					TempsResults[varname] = Value(false);
				} else {
					ParseError(line, "Invalid boolean value");
				}
				break;
			case CHAR:
				TempsResults[varname] = Value(inputvar[0]);
				break;
			case STRING:
				TempsResults[varname] = Value(inputvar);
				break;
			default:
				ParseError(line, "Unknown type");
				return false;
		}
	} catch (...) {
		ParseError(line, "Invalid input for variables.");
		return false;
	}

	return true;
}

bool IfStmt(istream& in, int& line) {
	LexItem tok;
	Value val;
	if (!Expr(in, line, val)) {
		ParseError(line, "Missing if statement condition");
		ParseError(line, "Invalid If statement.");
		return false;
	}

	if (val.GetType() != VBOOL) {
		ParseError(line, "Invalid expression type for an IF condition");
		ParseError(line, "Invalid If statement.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != THEN) {
		ParseError(line, "Missing THEN keyword.");
		return false;
	}

	bool condition = false;
	
	if (val.GetBool()) {
		if (!StmtList(in, line)) {
			ParseError(line, "Missing Statement for If-Stmt Then-clause");
			ParseError(line, "Invalid If statement.");
			return false;
		}
		condition = true;
	} else {
		while(true) {
			tok = Parser::GetNextToken(in, line);
			if (tok == ELSIF || tok == ELSE || tok == END) {
				Parser::PushBackToken(tok);
				break;
			}
		}
	}

	while (true) {
		tok = Parser::GetNextToken(in, line);
		if (tok == ELSIF) {
			if (condition) {
				if (!Expr(in, line, val)) {
					return false;
				}

				tok = Parser::GetNextToken(in, line);
				if (tok != THEN) {
					return false;
				}

				while(true) {
					tok = Parser::GetNextToken(in, line);
					if (tok == ELSIF || tok == ELSE || tok == END) {
						Parser::PushBackToken(tok);
						break;
					}
				}
			} else {
				if (!Expr(in, line, val)) {
					return false;
				}
				if (val.GetType() != VBOOL) {
					ParseError(line, "Invalid expression for an Elsif condition.");
					ParseError(line, "Invalid if statement.");
					return false;
				}
				tok = Parser::GetNextToken(in, line);
				if (tok != THEN) {
					return false;
				}
				
				if (val.GetBool()) {
					if (!StmtList(in, line)) {
						return false;
					}
					condition = true;
				} else {
					while(true) {
						tok = Parser::GetNextToken(in, line);
						if (tok == ELSIF || tok == ELSE || tok == END) {
							Parser::PushBackToken(tok);
							break;
						}
					}
				}
			}
		} else {
			break;
		}
	}

	if (tok == ELSE) {
		if (!condition) {
			if (!StmtList(in, line)) {
				ParseError(line, "Invalid statement list in else statement.");
				return false;
			}
		} else {
			while(true) {
				tok = Parser::GetNextToken(in, line);
				if (tok == ELSIF || tok == ELSE || tok == END) {
					Parser::PushBackToken(tok);
					break;
				}
			}
		}
		tok = Parser::GetNextToken(in, line);
	}

	if (tok != END) {
		ParseError(line, "Missing closing END IF for If-statement.");
		ParseError(line, "Invalid If statement.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != IF) {
		ParseError(line, "Missing IF after END");
		ParseError(line, "Invalid If statement.");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		ParseError(line, "Missing semicolon after END IF");
		ParseError(line, "Invalid If statement.");
		return false;
	}

	return true;
}

bool AssignStmt(istream& in, int& line) {
	LexItem tok = Parser::GetNextToken(in, line);
	string varname = tok.GetLexeme();
	Token type = SymTable[varname];

	tok = Parser::GetNextToken(in, line);
	if (tok != ASSOP) {
		ParseError(line, "Missing Assignment Operator.");
		ParseError(line, "Invalid assignment statement.");
		return false;
	} 

	Value val;
	if (!Expr(in, line, val)) {
		ParseError(line, "Illegal Expression type for the assigned variable");
		ParseError(line, "Invalid assign statement.");
		return false;
	}

	switch(val.GetType()) {
		case VINT:
			if (type == INT) {
				break;
			} else {
				ParseError(line, "Illegal Expression type for the assigned variable");
				ParseError(line, "Invalid assignment statement.");
				return false;
			}
		case VREAL:
			if (type == FLOAT || type == INT) {
				break;
			} else {
				ParseError(line, "Illegal Expression type for the assigned variable");
				ParseError(line, "Invalid assignment statement.");
				return false;
			}
		case VSTRING:
			if (type == STRING) {
				break;
			} else {
				ParseError(line, "Illegal Expression type for the assigned variable");
				ParseError(line, "Invalid assignment statement.");
				return false;
			}
		case VCHAR:
			if (type == CHAR) {
				break;
			} else {
				ParseError(line, "Illegal Expression type for the assigned variable");
				ParseError(line, "Invalid assignment statement.");
				return false;
			}
		case VBOOL:
			if (type == BOOL) {
				break;
			} else {
				ParseError(line, "Illegal Expression type for the assigned variable");
				ParseError(line, "Invalid assignment statement.");
				return false;
			}
		default:
			return false;
	}

	TempsResults[varname] = val;

	tok = Parser::GetNextToken(in, line);
	if (tok != SEMICOL) {
		ParseError(line, "Missing semicolon at end of statement");
		ParseError(line, "Invalid assign statement.");
		return false;
	}

	return true;
}

bool Expr(istream& in, int& line, Value & retVal) {
	Value left;
	if (!Relation(in, line, left)) {
		return false;
	}

	LexItem tok = Parser::GetNextToken(in, line);
	while (tok == AND || tok == OR) {
		Value right;
		if (!Relation(in, line, right)) {
			return false;
		}
		if (tok == AND) {
			left = left && right;
		}
		else {
			left = left || right;
		}
		tok = Parser::GetNextToken(in, line);
	}

	Parser::PushBackToken(tok);
	retVal = left;
	return true;
}

bool Relation(istream& in, int& line, Value & retVal) {
	Value left;
	if (!SimpleExpr(in, line, left)) {
		return false;
	}

	LexItem tok = Parser::GetNextToken(in, line);
	if (tok == EQ || tok == NEQ || tok == LTHAN || tok == LTE || tok == GTHAN || tok == GTE) {
		Value right;
		if (!SimpleExpr(in, line, right)) {
			ParseError(line, "Invalid Relation.");
			return false;
		}
		if (tok == EQ) {
			left = left == right;
		}
		if (tok == NEQ) {
			left = left != right;
		}
		if (tok == LTHAN) {
			left = left < right;
		}
		if (tok == LTE) {
			left = left <= right;
		}
		if (tok == GTHAN) {
			left = left > right;
		}
		if (tok == GTE) {
			left = left >= right;
		}
	} else {
		Parser::PushBackToken(tok);
	}

	retVal = left;
	return true;
}

bool SimpleExpr(istream& in, int& line, Value & retVal) {
	Value left;
	if (!STerm(in, line, left)) {
		return false;
	}

	LexItem tok = Parser::GetNextToken(in, line);
	while (tok == PLUS || tok == MINUS || tok == CONCAT) {
		Value right;
		if (!STerm(in, line, right)) {
			return false;
		}
		if (left.GetType() != right.GetType()) {
			ParseError(line, "Illegal operand type for the operation.");
			return false;
		}
		if (tok == PLUS) {
			left = left + right;
		}
		if (tok == MINUS) {
			left = left - right;
		}
		if (tok == CONCAT) {
			left = left.Concat(right);
		}
		tok = Parser::GetNextToken(in, line);
	}

	Parser::PushBackToken(tok);
	retVal = left;
	return true;
}

bool STerm(istream& in, int& line, Value & retVal) {
	LexItem tok = Parser::GetNextToken(in, line);
	int sign = 1;

	if (tok == PLUS) {
		sign = 1;
	} else if (tok == MINUS) {
		sign = -1;
	} else {
		Parser::PushBackToken(tok);
	}

	Value res;
	if (!Term(in, line, sign, res)) {
		return false;
	}

	if (sign == -1) {
		if (res.GetType() != VINT && res.GetType() != VREAL) {
			ParseError(line, "Illegal Operand type for Sign Operator");
			return false;
		}
	}

	retVal = res;
	return true;
}

bool Term(istream& in, int& line, int sign, Value & retVal) {
	Value left;
	if (!Factor(in, line, sign, left)) {
		return false;
	}

	while (true) {
		LexItem tok = Parser::GetNextToken(in, line);
		if (tok == MULT || tok == DIV || tok == MOD) {
			Value right;
			if (!Factor(in, line, sign, right)) {
				ParseError(line, "Missing operand after operator");
				return false;
			}
			if ((left.GetType() != VREAL && left.GetType() != VINT) && (right.GetType() != VREAL && right.GetType() != VINT)) {
				ParseError(line, "Missing expression in Assignment Statement.");
				return false;
			}
			if (tok == MULT) {
				left = left * right;
			}
			if (tok == DIV) {
				if (right.GetType() == VREAL && right.GetReal() == 0.0f) {
					ParseError(line, "Run-Time Error-Illegal division by Zero");
					return false;
				} else if (right.GetType() == VINT && right.GetInt() == 0) {
					ParseError(line, "Run-Time Error-Illegal division by Zero");
					return false;
				}
				left = left / right;
			}
			if (tok == MOD) {
				left = left % right;
			}
		} else {
			Parser::PushBackToken(tok);
			break;
		}
	}

	retVal = left;
	return true;
}

bool Factor(istream& in, int& line, int sign, Value & retVal) {
	LexItem tok = Parser::GetNextToken(in, line);
	Value res;
	if (tok == NOT) {
		if (!Primary(in, line, sign, res)) {
			ParseError(line, "Missing primary after NOT keyword.");
			return false;
		}
		retVal = !res;
		return true;
	}
	
	Parser::PushBackToken(tok);
	if (!Primary(in, line, sign, res)) {
		ParseError(line, "Incorrect operand");
		return false;
	}

	tok = Parser::GetNextToken(in, line);
	if (tok == EXP) {
		tok = Parser::GetNextToken(in, line);
		if (tok == PLUS) {
			sign = 1;
		} else if (tok == MINUS) {
			sign = -1;
		} else {
			Parser::PushBackToken(tok);
		}
		Value right;
		if (!Primary(in, line, sign, right)) {
			ParseError(line, "Incorrect operand");
			ParseError(line, "Missing operand after operator");
			return false;
		}
		retVal = res.Exp(right);
		return true;
	}

	Parser::PushBackToken(tok);
	retVal = res;
	return true;
}

bool Primary(istream& in, int& line, int sign, Value & retVal) {
	LexItem tok = Parser::GetNextToken(in, line);
	Value res;
	if (tok == IDENT) {
		Parser::PushBackToken(tok);
		if (!Name(in, line, sign, res)) {
			return false;
		}
		retVal = res;
		return true;
	} else if (tok == ICONST || tok == FCONST || tok == SCONST || tok == BCONST || tok ==CCONST) {
		if (tok == ICONST) {
			retVal = Value(stoi(tok.GetLexeme()));
		} else if (tok == FCONST) {
			retVal = Value(stod(tok.GetLexeme()));
		} else if (tok == BCONST) {
			retVal = Value(tok.GetLexeme() == "true");
		} else {
			retVal = Value(tok.GetLexeme());
		}

		if (retVal.GetType() == VINT && sign == -1) {
			retVal = Value(-retVal.GetInt());
		}
		else if (retVal.GetType() == VREAL && sign == -1) {
			retVal = Value(-retVal.GetReal());
		}

		return true;
	} else if (tok == LPAREN) {
		if (!Expr(in, line, retVal)) {
			ParseError(line, "Invalid Expression");
			return false;
		}
		tok = Parser::GetNextToken(in, line);
		if (tok != RPAREN) {
			ParseError(line, "Missing right parenthesis after expression");
			return false;
		}

		if (retVal.GetType() == VINT && sign == -1) {
			retVal = Value(-retVal.GetInt());
		}
		else if (retVal.GetType() == VREAL && sign == -1) {
			retVal = Value(-retVal.GetReal());
		}

		return true;
	}

	Parser::PushBackToken(tok);
	ParseError(line, "Invalid Expression");
	return false;
}

bool Name(istream& in, int& line, int sign, Value & retVal) {
	LexItem tok = Parser::GetNextToken(in, line);
	if (tok != IDENT) {
		return false;
	}
	string varname = tok.GetLexeme();
	if (defVar.count(varname) != 1) {
		ParseError(line, "Using Undefined Variable");
		ParseError(line, "Invalid reference to a variable.");
		return false;
	} 
	if (TempsResults.count(varname) == 0) {
		ParseError(line, "Invalid use of an uninitialized variable.");
		return false;
	}
	
	tok = Parser::GetNextToken(in, line);
	if (tok == LPAREN) {
		Value left, right;
		if (!Range(in, line, left, right)) {
			ParseError(line, "Missing range in Name");
			return false;
		}
		if (TempsResults[varname].GetType() != VSTRING) {
			ParseError(line, "String required for indexing and substringing.");
			return false;
		}

		string varval = TempsResults[varname].GetString();

		if (right.GetType() == VERR) {
			if (left.GetType() != VINT) {
				ParseError(line, "Index must be an integer.");
				return false;
			}
			int index = left.GetInt();
			if (index < 0 || index >= varval.length()) {
				ParseError(line, "Out of range index value.");
				return false;
			}

			char c = varval[index];
			retVal = Value(c);

			if (retVal.GetType() == VSTRING && retVal.GetString().length() == 1) {
				retVal.SetType(VCHAR);
			}
		} else {
			if (left.GetType() != VINT || right.GetType() != VINT) {
				ParseError(line, "Substring bounds must be integers.");
				return false;
			}
			int start = left.GetInt();
			int end = right.GetInt();
			if (start < 0 || end >= varval.length() || start > end) {
				ParseError(line, "Invalid substring syntax.");
				return false;
			}
			retVal = Value(varval.substr(start, end - start + 1));
		}

		tok = Parser::GetNextToken(in, line);
		if (tok != RPAREN) {
			ParseError(line, "Missing Right Parenthesis");
			return false;
		}
	} else {
		Parser::PushBackToken(tok);
		if (TempsResults.count(varname) == 0) {
			ParseError(line, "Invalid use of an unitialized variable.");
			return false;
		}
		retVal = TempsResults[varname];
	}

	if (retVal.GetType() == VINT && sign == -1) {
		retVal = Value(-retVal.GetInt());
	}
	else if (retVal.GetType() == VREAL && sign == -1) {
		retVal = Value(-retVal.GetReal());
	}

	return true;
}

bool Range(istream& in, int& line, Value & retVal1, Value & retVal2) {
	if (!SimpleExpr(in, line, retVal1)) {
		return false;
	}

	LexItem tok = Parser::GetNextToken(in, line);
	Value res;
	if (tok == DOT) {
		tok = Parser::GetNextToken(in, line);
		if (tok != DOT) {
			ParseError(line, "Missing second dot in Range");
			return false;
		}
		if (!SimpleExpr(in, line, retVal2)) {
		        return false;
		}
		return true;
	} 
	Parser::PushBackToken(tok);
	retVal2 = Value();
	return true;
}

