#include "lex.h"
#include <cctype>
#include <iostream>
#include <unordered_map>
#include <algorithm>

using namespace std;

LexItem getNextToken(istream& in, int& linenumber) {
    enum TokenState {
        START,
        INID,
        INSTRING,
        ININT,
        INFLOAT,
        INBOOL,
        INCHAR
    } lexstate = START;

    char ch;
    string lexeme = "";

    while(true) {
        switch(lexstate) {
            case START:
                in.get(ch);
                if (in.eof()) {
                    return LexItem(DONE, lexeme, linenumber);
                } else if (ch == '\n') {
                    linenumber++;
                    continue;
                } else if (isspace(ch)) {
                    continue;
                } else if (isalpha(ch)) {
                    lexstate = INID;
                    lexeme += ch;
                    break;
                } else if (isdigit(ch)) {
                    lexstate = ININT;
                    lexeme += ch;
                    break;
                } else if (ch == '"') {
                    lexstate = INSTRING;
                    break;
                } else if (ch == '\''){
                    lexstate = INCHAR;
                    break;
                } else if (ch == '+') {
                    return LexItem(PLUS, "+", linenumber);
                } else if (ch == '-') {
                    if (in.peek() == '-') {
                        in.get(ch);
                        while (in.get(ch)) {
                            if (ch == '\n') {
                                linenumber++;
                                break;
                            }
                        }
                        break;
                    } else {
                        return LexItem(MINUS, "-", linenumber);
                    }
                } else if (ch == '*') {
                    if (in.peek() == '*') {
                        in.get(ch);
                        return LexItem(EXP, "**", linenumber);
                    }
                    return LexItem(MULT, "*", linenumber);
                } else if (ch == '/') {
                    if (in.peek() == '=') {
                        in.get(ch);
                        return LexItem(NEQ, "/=", linenumber);
                    }
                    return LexItem(DIV, "/", linenumber);
                } else if (ch == ':') {
                    if (in.peek() == '=') {
                        in.get(ch);
                        return LexItem(ASSOP, ":=", linenumber);
                    }
                    return LexItem(COLON, ":", linenumber);
                } else if (ch == '=') {
                    return LexItem(EQ, "=", linenumber);
                } else if (ch == '<') {
                    if (in.peek() == '=') {
                        in.get(ch);
                        return LexItem(LTE, "<=", linenumber);
                    }
                    return LexItem(LTHAN, "<", linenumber);
                } else if (ch == '>') {
                    if (in.peek() == '=') {
                        in.get(ch);
                        return LexItem(GTE, ">=", linenumber);
                    }
                    return LexItem(GTHAN, ">", linenumber);
                } else if (ch == '&') {
                    return LexItem(CONCAT, "&", linenumber);
                } else if (ch == ',') {
                    return LexItem(COMMA, ",", linenumber);
                } else if (ch == '(') {
                    return LexItem(LPAREN, "(", linenumber);
                } else if (ch == ')') {
                    return LexItem(RPAREN, ")", linenumber);
                } else if (ch == '.') {
                    return LexItem(DOT, ".", linenumber);
                } else if (ch == ';') {
                    return LexItem(SEMICOL, ";", linenumber);
                } else {
                    lexeme += ch;
                    return LexItem(ERR, lexeme, linenumber);
                }
                break;
            case INID:
                while (in.get(ch) && (isalnum(ch) || ch == '_')) {
                    if (ch == '_') {
                        if (in.peek() == '_') {
                            lexeme += ch;
                            return id_or_kw(lexeme, linenumber);
                        }
                    }
                    lexeme += ch;
                }
                in.unget();
                return id_or_kw(lexeme, linenumber);
            case ININT:
                while (in.get(ch)) {
                    if (ch == '.') {
                        if (!isdigit(in.peek())) {
                            in.unget();
                            return LexItem(ICONST, lexeme, linenumber);
                        } else if (isdigit(in.peek())) {
                            lexeme += ch;
                            lexstate = INFLOAT;
                            break;
                        }
                    } else if (ch == 'E' || ch == 'e') {
                        if (!(isdigit(in.peek()) || in.peek() == '+' || in.peek() == '-')) {
                            in.unget();
                            return LexItem(ICONST, lexeme, linenumber);
                        } else if (isdigit(in.peek()) || in.peek() == '+' || in.peek() == '-') {
                            lexeme += ch;
                            in.get(ch);
                            lexeme += ch;
                            while (in.get(ch)) {
                                if (isdigit(ch)) {
                                    lexeme += ch;
                                }
                                else {
                                    break;
                                }
                            }
                            in.unget();
                            return LexItem(ICONST, lexeme, linenumber);
                        }
                    } else if (isdigit(ch)) {
                        lexeme += ch;
                    } else {
                        in.unget();
                        return LexItem(ICONST, lexeme, linenumber);
                    }
                } 
                break;
            case INFLOAT:
                while(in.get(ch)) {
                    if (isdigit(in.peek())) {
                        lexeme += ch;
                    } else if (in.peek() == '.') {
                        lexeme += ch;
                        in.get(ch);
                        lexeme += ch;
                        return LexItem(ERR, lexeme, linenumber);
                    } else if (in.peek() == 'E' || in.peek() == 'e') {
                        lexeme += ch;
                        in.get(ch);
                        if (isdigit(in.peek())|| in.peek() == '+' || in.peek() == '-') {
                            lexeme += ch;
                            in.get(ch);
                            lexeme += ch;
                        } else {
                            in.unget();
                            return LexItem(FCONST, lexeme, linenumber);
                        }
                        while(isdigit(in.peek())) {
                            in.get(ch);
                            lexeme += ch;
                        }
                        return LexItem(FCONST, lexeme, linenumber);
                    } else if (!isdigit(in.peek())) {
                        lexeme += ch;
                        return LexItem(FCONST, lexeme, linenumber);
                    } else {
                        return LexItem(ERR, lexeme, linenumber);
                    }
                } 
                return LexItem(FCONST, lexeme, linenumber);
            case INSTRING:
                while (in.get(ch)) {
                    string msg;
                    if (ch == '"') {
                        return LexItem(SCONST, lexeme, linenumber);
                    }
                    if (ch == '\n') {
                        msg = " Invalid string constant \"";
                        return LexItem(ERR, msg + lexeme, linenumber);
                    }
                    lexeme += ch;
                }
                return LexItem(ERR, "Invalid string constant \"" + lexeme, linenumber);
            case INBOOL:
                if (lexeme == "true" || lexeme == "false") {
                    return id_or_kw(lexeme, linenumber);
                }
            case INCHAR:
                if (in.peek() >= 32 && in.peek() <= 126) {
                    in.get(ch);
                    lexeme += ch;
                }
                if (in.peek() == '\'') {
                    in.get(ch);
                    return LexItem(CCONST, lexeme, linenumber);
                } else if (in.peek() == '\n') {
                    return LexItem(ERR, "New line is an invalid character constant.", linenumber);
                }
                else {
                    lexeme += in.get();
                    lexeme = " Invalid character constant " + lexeme + "\'";
                    return LexItem(ERR, lexeme, linenumber);
                }
            default:
                return LexItem(ERR, string(1, ch), linenumber);
        }
    }
    return LexItem(DONE, lexeme, linenumber);
}

LexItem id_or_kw(const string& lexeme, int linenum) {
    unordered_map<string, Token> reservedWords = {{"procedure", PROCEDURE}, {"string", STRING}, {"else", ELSE}, {"if", IF},
    {"integer", INT}, {"float", FLOAT}, {"character", CHAR}, {"put", PUT}, {"putline", PUTLN}, {"get", GET}, {"boolean", BOOL},
    {"true", TRUE}, {"false", FALSE}, {"elsif", ELSIF}, {"is", IS}, {"end", END}, {"begin", BEGIN}, {"then", THEN}, {"constant", CONST}};

    unordered_map<string, Token> operators = {{"mod", MOD}, {"and", AND}, {"or", OR}, {"not", NOT}};

    string lowerLexeme = lexeme;
    transform(lowerLexeme.begin(), lowerLexeme.end(), lowerLexeme.begin(), ::tolower);

    if (reservedWords.find(lowerLexeme) != reservedWords.end()) {
        if (lowerLexeme == "true" || lowerLexeme == "false") {
            return LexItem(BCONST, lexeme, linenum);
        }
        return LexItem(reservedWords[lowerLexeme], lowerLexeme, linenum);
    }

    if (operators.find(lowerLexeme) != operators.end()) {
        return LexItem(operators[lowerLexeme], lowerLexeme, linenum);
    }

    return LexItem(IDENT, lowerLexeme, linenum);
}

ostream& operator<<(ostream& out, const LexItem& tok) {
    static unordered_map<Token, string> outputStrings = {{IF, "IF"}, {ELSE, "ELSE"}, {ELSIF, "ELSIF"}, {PUT, "PUT"}, {PUTLN, "PUTLN"}, 
    {GET, "GET"}, {INT, "INT"}, {FLOAT, "FLOAT"}, {CHAR, "CHAR"}, {STRING, "STRING"}, {BOOL, "BOOL"}, {PROCEDURE, "PROCEDURE"},
    {TRUE, "TRUE"}, {FALSE, "FALSE"}, {END, "END"}, {IS, "IS"}, {BEGIN, "BEGIN"}, {THEN, "THEN"}, {CONST, "CONST"}, {IDENT, "IDENTIFIER"},
    {ICONST, "ICONST"}, {FCONST, "FCONST"}, {SCONST, "SCONST"}, {BCONST, "BCONST"}, {CCONST, "CCONST"},
    {PLUS, "PLUS"}, {MINUS, "MINUS"}, {MULT, "MULT"}, {DIV, "DIV"}, {ASSOP, "ASSOP"}, {EQ, "EQ"}, {NEQ, "NEQ"}, {EXP, "EXP"}, {CONCAT, "CONCAT"}, 
    {GTHAN, "GTHAN"}, {LTHAN, "LTHAN"}, {LTE, "LTE"}, {GTE, "GTE"}, {AND, "AND"}, {OR, "OR"}, {NOT, "NOT"}, {MOD, "MOD"}, {COMMA, "COMMA"}, 
    {SEMICOL, "SEMICOL"}, {LPAREN, "LPAREN"}, {RPAREN, "RPAREN"}, {DOT, "DOT"}, {COLON, "COLON"}, {ERR, "ERR"}, {DONE, "DONE"}};

    if (tok.GetToken() == ICONST) {
        out << "ICONST: (" << tok.GetLexeme() << ")" << endl; 
    } else if (tok.GetToken() == FCONST) {
        out << "FCONST: (" << tok.GetLexeme() << ")" << endl;
    } else if (tok.GetToken() == BCONST) {
        out << "BCONST: (" << tok.GetLexeme() << ")" << endl;
    } else if (tok.GetToken() == IDENT) {
        out << "IDENT: <" << tok.GetLexeme() << ">" << endl;
    } else if (tok.GetToken() == SCONST) {
        out << "SCONST: \"" << tok.GetLexeme() << '"' << endl;
    } else if (tok.GetToken() == CCONST) {
        out << "CCONST: \'" << tok.GetLexeme() << '\'' << endl;
    } else if (tok.GetToken() == ERR){
        out << outputStrings[tok.GetToken()] << ": In line " << tok.GetLinenum() << ", Error Message {" << tok.GetLexeme() << "}" << endl;
        exit(1);
    } else if (tok.GetLexeme() == "\n") {
        out << "In line " << tok.GetLinenum() << ", Error Message {New line is an invalid character constant.}" << endl;
    } else {
        out << outputStrings[tok.GetToken()] << endl;
    }
    return out;
}