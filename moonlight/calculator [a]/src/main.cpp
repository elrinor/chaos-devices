#include <string>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

float strToFloat(const string& s) {
    float result;
    istringstream stream(s);
    stream >> result;
    return result;
}

string intToStr(int v) {
    ostringstream stream;
    stream << v;
    return stream.str();
}

enum TokenType {
    NUMBER,
    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    LPAREN,
    RPAREN,

    UMINUS,
    UPLUS,
    END,

    NOTHING
};

class Token {
private:
    TokenType type;
    string s;
    int pos;
public:
    Token(TokenType type, string s, int pos): type(type), s(s), pos(pos) {}
    TokenType getType() const { return this->type; }
    string getString() const { return this->s; }
    int getPos() const { return this->pos; }
    string getTypeString() const {
        switch(type) {
            case NUMBER:  return "NUMBER";
            case PLUS:    return "PLUS";
            case MINUS:   return "MINUS";
            case TIMES:   return "TIMES";
            case DIVIDE:  return "DIVIDE";
            case LPAREN:  return "LPAREN";
            case RPAREN:  return "RPAREN";
            case UMINUS:  return "UMINUS";
            case UPLUS:   return "UPLUS";
            case END:     return "END";
            case NOTHING: return "NOTHING";
            default:
                throw runtime_error(string() + "Unknown token type (" + intToStr((int) type) + ")");
        }
    }
};


class Lexer {
private:
    string s;
    int pos;
    Token token;

    void unexpected() const {
        throw runtime_error(string() + "Unexpected symbol '" + s[pos - 1] + "' at position " + intToStr(pos - 1));
    }

    Token getNumberToken() {
        int oldPos = pos - 1;
        while(true) {
            switch(s[pos++]) {
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                break;
            default:
                pos--;
                return Token(NUMBER, s.substr(oldPos, pos - oldPos), oldPos);
            }
        }
    }

public:
    Lexer(string source): s(source + '\0'), pos(0), token(Token(NOTHING, "", 0)) {}

    Token getNextToken() {
        if(token.getType() != NOTHING) {
            Token result = token;
            token = Token(NOTHING, "", 0);
            return result;
        }
        while(true) {
            switch(s[pos++]) {
            case ' ': case '\t': case '\n': case '\r':
                break;
            case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': case '8': case '9':
                return getNumberToken();
            case '-':
                return Token(MINUS, "-", pos - 1);
            case '+':
                return Token(PLUS, "+", pos - 1);
            case '*':
                return Token(TIMES, "*", pos - 1);
            case '/':
                return Token(DIVIDE, "/", pos - 1);
            case '(':
                return Token(LPAREN, "(", pos - 1);
            case ')':
                return Token(RPAREN, ")", pos - 1);
            case '\0':
                return Token(END, "", pos - 1);
            default:
                unexpected();
            }
        }
    }

    Token peekNextToken() {
        if(token.getType() == NOTHING)
            token = getNextToken();
        return token;
    }
};

class Parser {
private:
    Lexer* lexer;
    vector<Token> rpn; /* Reverse Polish Notation */

    void unexpected(Token t) const {
        throw runtime_error(string() + "Unexpected token " + t.getTypeString() + "(\"" + t.getString() + "\") at position " + intToStr(t.getPos()));
    }

    void require(TokenType type) {
        Token t = lexer->getNextToken();
        if(t.getType() != type)
            unexpected(t);
    }

    void factor() {
        /* factor ::= INT | '(' expr ')' | ('-' | '+') factor */
        Token t = lexer->peekNextToken();
        switch(t.getType()) {
        case NUMBER:
            require(NUMBER);
            rpn.push_back(t);
            break;
        case LPAREN:
            require(LPAREN);
            expr();
            require(RPAREN);
            break;
        
        case MINUS:
        case PLUS:
            require(t.getType());
            factor();
            if(t.getType() == PLUS)
                rpn.push_back(Token(UPLUS, t.getString(), t.getPos()));
            if(t.getType() == MINUS)
                rpn.push_back(Token(UMINUS, t.getString(), t.getPos()));
            break;

        default:
            unexpected(t);
        }
    }

    void term() {
        /* term ::= factor {('*' | '/') factor} */
        Token t = lexer->peekNextToken();
        switch(t.getType()) {
        case NUMBER:
        case LPAREN:

        case MINUS:
        case PLUS:

            factor();
            break;
        default:
            unexpected(t);
        }
        while(true) {
            t = lexer->peekNextToken();
            switch(t.getType()) {
            case TIMES:
            case DIVIDE:
                require(t.getType());
                factor();
                rpn.push_back(t);
                break;
            default:
                return;                
            }
        }
    }

    void expr() {
        /* expr ::= term {('+' | '-') term} */
        Token t = lexer->peekNextToken();
        switch(t.getType()) {
            case NUMBER:
            case LPAREN:

            case MINUS:
            case PLUS:

                term();
                break;
            default:
                unexpected(t);
        }
        
        while(true) {
            t = lexer->peekNextToken();
            switch(t.getType()) {
            case PLUS:
            case MINUS:
                require(t.getType());
                term();
                rpn.push_back(t);
                break;
            default:
                return;
            }
        }
    }

public:
    Parser(Lexer* lexer): lexer(lexer) {};

    vector<Token> parse() {
        rpn.clear();
        expr();
        Token t = lexer->peekNextToken();
        if(t.getType() != END)
            unexpected(t);
        return rpn;
    }
};

class Calculator {
private:
    Parser* parser;

    float pop_back(vector<float>& v) {
        if(v.size() == 0)
            throw runtime_error("Bad stack size during RPN evaluation: 0");
        float result = v.back();
        v.pop_back();
        return result;
    }

    void unexpected(Token t) const {
        throw runtime_error(string() + "Unexpected token " + t.getTypeString() + "(\"" + t.getString() + "\") in RPN (position in input string:" + intToStr(t.getPos()) + ")");
    }

public:
    Calculator(Parser* parser): parser(parser) {}

    float calculate() {
        vector<Token> rpn = parser->parse();
        vector<float> stack;

        for(unsigned int i = 0; i < rpn.size(); i++) {
            Token t = rpn[i];
            float v1, v2;
            switch(t.getType()) {
            case NUMBER:
                stack.push_back(strToFloat(t.getString()));
                break;
            case PLUS:
                v2 = pop_back(stack);
                v1 = pop_back(stack);
                stack.push_back(v1 + v2);
                break;
            case MINUS:
                v2 = pop_back(stack);
                v1 = pop_back(stack);
                stack.push_back(v1 - v2);
                break;
            case TIMES:
                v2 = pop_back(stack);
                v1 = pop_back(stack);
                stack.push_back(v1 * v2);
                break;
            case DIVIDE:
                v2 = pop_back(stack);
                v1 = pop_back(stack);
                stack.push_back(v1 / v2);
                break;
            case UMINUS:
                v1 = pop_back(stack);
                stack.push_back(- v1);
                break;
            case UPLUS:
                v1 = pop_back(stack);
                stack.push_back(+ v1);
                break;
            default:
                unexpected(t);
            }
        }

        if(stack.size() != 1)
            throw runtime_error(string() + "Bad stack size after RPN evaluation: " + intToStr((int) rpn.size()));

        return stack[0];
    }
};

int main() {
    while(true) {
        string s;
        cout << ">";
        getline(cin, s);
        if(s == "end")
            return 0;
        else {
            Lexer* lexer = new Lexer(s);
            Parser* parser = new Parser(lexer);
            Calculator* calc = new Calculator(parser);
            try {
                cout << "= " << calc->calculate() << endl;
            } catch (runtime_error e) {
                cout << e.what() << endl;
            }
            delete calc;
            delete parser;
            delete lexer;
        }
    }
}
