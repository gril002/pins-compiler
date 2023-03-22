/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.List;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.lexer.TokenType;
import compiler.parser.ast.Ast;

public class Parser {
    /**
     * Seznam leksikalnih simbolov.
     */
    private final List<Symbol> symbols;

    int pos = 0;

    /**
     * Ciljni tok, kamor izpisujemo produkcije. Če produkcij ne želimo izpisovati,
     * vrednost opcijske spremenljivke nastavimo na Optional.empty().
     */
    private final Optional<PrintStream> productionsOutputStream;

    public Parser(List<Symbol> symbols, Optional<PrintStream> productionsOutputStream) {
        requireNonNull(symbols, productionsOutputStream);
        this.symbols = symbols;
        this.productionsOutputStream = productionsOutputStream;
    }

    /**
     * Izvedi sintaksno analizo.
     */
    public Ast parse() {
        var ast = parseSource();
        return ast;
    }

    private Ast parseSource() {
        dump("source -> definitions");
        parseDefinitions();
        return null;
    }

    /**
     * Izpiše produkcijo na izhodni tok.
     */
    private void dump(String production) {
        if (productionsOutputStream.isPresent()) {
            productionsOutputStream.get().println(production);
        }
    }

    private boolean check(TokenType token) {
        return symbols.get(pos).tokenType.equals(token);
    }

    private void skip() {
        pos++;
    }

    private void syntaxError(String msg) {
        Symbol symbol = symbols.get(pos);
        Report.error(symbol.position, String.format("Syntax analysis ERROR. Expected %s but got '%s' at symbol position %d.", msg, symbol.lexeme, pos));
    }

    private void syntaxError(String msg, String fName) {
        Symbol symbol = symbols.get(pos);
        //Report.error(symbol.position, String.format("Syntax analysis ERROR in %s. Expected %s but got '%s'", fName, msg, symbol.lexeme));
        syntaxError(msg); // TODO implement better error reporting
    }
    void parseDefinitions() {
        dump("definitions -> definition definitions_");
        parseDefinition();
        parseDefinitions_();
    }

    void parseDefinition() {
        if(check(KW_TYP)) {
            dump("definition -> type_definition");
            parseTypeDefinition();
        } else if (check(KW_FUN)) {
            dump("definition -> function_definition");
            parseFunctionDefinition();
        } else if (check(KW_VAR)) {
            dump("definition -> variable_definition");
            parseVariableDefinition();
        } else
            syntaxError("a definition", "definition -> type_definition | function_definition | variable_definition");
    }

    void parseTypeDefinition() {
        if (check(KW_TYP)) {
            dump("type_definition -> typ id : type");
            skip();
            if (check(IDENTIFIER)) {
                skip();
                if (check(OP_COLON)) {
                    skip();
                    parseType();
                } else
                    syntaxError("a ':'", "type_definition -> typ id : type");
            } else
                syntaxError("an identifier", "type_definition -> typ id : type");
        } else
            syntaxError("the keyword 'TYP'", "type_definition -> typ id : type");
    }

    void parseType() {
        if (check(IDENTIFIER)) {
            dump("type -> id");
            skip();
        } else if (check(AT_LOGICAL)) {
            dump("type -> logical");
            skip();
        } else if (check(AT_INTEGER)) {
            dump("type -> integer");
            skip();
        } else if (check(AT_STRING)) {
            dump("type -> string");
            skip();
        } else if (check(KW_ARR)) {
            dump("type -> arr [ int_constant ] type");
            skip();
            if (check(OP_LBRACKET)) {
                skip();
                if (check(C_INTEGER)) {
                    skip();
                    if (check(OP_RBRACKET)) {
                        skip();
                        parseType();
                    } else
                        syntaxError("']'", "type -> [ int_constant ]");
                } else
                    syntaxError("an integer constant", "type -> [ int_constant");
            } else
                syntaxError("'['", "type -> [");
        } else
            syntaxError("type: identifier, logical, integer string or arr", "type -> id | logical | integer string | arr [ int_constant ] type");
    }

    void parseFunctionDefinition() {
        dump("function_definition -> fun identifier ( parameters ) : type = expression");
        if (check(KW_FUN)) {
            skip();
            if (check(IDENTIFIER)) {
                skip();
                if (check(OP_LPARENT)) {
                    skip();
                    parseParameters();
                    if (check(OP_RPARENT)) {
                        skip();
                        if (check(OP_COLON)) {
                            skip();
                            parseType();
                            if (check(OP_ASSIGN)) {
                                skip();
                                parseExpression();
                            } else
                                syntaxError("'='", "function_definition -> fun identifier ( parameters ) : type = expression");
                        } else
                            syntaxError("':'", "function_definition -> fun identifier ( parameters ) : type = expression");
                    } else
                        syntaxError("')'","function_definition -> fun identifier ( parameters ) : type = expression");
                } else
                    syntaxError("'('", "function_definition -> fun identifier ( parameters ) : type = expression");
            } else
                syntaxError("an identifier","function_definition -> fun identifier ( parameters ) : type = expression");
        } else
            syntaxError("the keyword 'fun'", "function_definition -> fun identifier ( parameters ) : type = expression");
    }

    void parseParameters() {
        dump("parameters -> parameter parameters_");
        parseParameter();
        parseParameters_();
    }

    void parseParameter() {
        if (check(IDENTIFIER)) {
            dump("parameter -> id : type");
            skip();
            if (check(OP_COLON)) {
                skip();
                parseType();
            } else
                syntaxError("a ':'", "parameter -> id : type");
        } else
            syntaxError("an identifier", "parameter -> id : type");
    }

    void parseParameters_() {
        if (check(OP_COMMA)) {
            dump("parameters_ -> , parameter parameters_");
            skip();
            parseParameter();
            parseParameters_();
        } else if (check(OP_RPARENT)) {
            dump("parameters_ -> ε");
        } else
            syntaxError("a ',' or ')'", "parameters_ -> , parameter parameters_ | ε");
    }

    void parseExpression() {
        dump("expression -> logical_ior_expression expression_");
        parseLogicalIorExpression();
        parseExpression_();
    }

    void parseLogicalIorExpression() {
        dump("logical_ior_expression -> logical_and_expression logical_ior_expression_");
        parseLogicalAndExpression();
        parseLogicalIorExpression_();
    }

    void parseLogicalAndExpression() {
        dump("logical_and_expression -> compare_expression logical_and_expression_");
        parseCompareExpression();
        parseLogicalAndExpression_();
    }

    void parseCompareExpression() {
        dump("compare_expression -> additive_expression compare_expression_");
        parseAdditiveExpression();
        parseCompareExpression_();
    }

    void parseAdditiveExpression() {
        dump("additive_expression -> multiplicative_expression additive_expression_");
        parseMultiplicativeExpression();
        parseAdditiveExpression_();
    }

    void parseMultiplicativeExpression() {
        dump("multiplicative_expression -> prefix_expression multiplicative_expression_");
        parsePrefixExpression();
        parseMultiplicativeExpression_();
    }

    void parsePrefixExpression() {
        if (check(OP_ADD)) {
            dump("prefix_expression -> + prefix_expression");
            skip();
            parsePrefixExpression();
        } else if (check(OP_SUB)) {
            dump("prefix_expression -> - prefix_expression");
            skip();
            parsePrefixExpression();
        } else if (check(OP_NOT)) {
            dump("prefix_expression -> ! prefix_expression");
            skip();
            parsePrefixExpression();
        } else {
            dump("prefix_expression -> postfix_expression");
            parsePostfixExpression();
        }
    }

    void parsePostfixExpression() {
        dump("postfix_expression -> atomic_expression postfix_expression_");
        parseAtomicExpression();
        parsePostfixExpression_();
    }

    void parseAtomicExpression() {
        if (check(IDENTIFIER)) {
            dump("atomic_expression -> identifier atomic_expression_");
            skip();
            parseAtomicExpression_();
        } else if (check(OP_LPARENT)) {
            dump("atomic_expression -> ( expressions )");
            skip();
            parseExpressions();
            if (check(OP_RPARENT)) {
                skip();
            } else
                syntaxError("')'");
        } else if (check(OP_LBRACE)) {
            dump("atomic_expression -> { atomic_expression___");
            skip();
            parseAtomicExpression__();
        } else if (check(C_INTEGER)) {
            dump("atomic_expression -> int_constant");
            skip();
        } else if (check(C_LOGICAL)) {
            dump("atomic_expression -> logical_constant");
            skip();
        } else if (check(C_STRING)) {
            dump("atomic_expression -> str_constant");
            skip();
        } else
            syntaxError("an identifier, a '(', '{' or a constant: integer, logical or string",
                    "atomic_expression -> identifier atomic_expression_ | ( expressions ) | atomic_expression " +
                            "-> { atomic_expression___ | int_constant | logical_constant | str_constant");
    }

    void parseAtomicExpression_() {
        if (check(OP_LPARENT)) {
            dump("atomic_expression_ -> ( expressions )");
            skip();
            parseExpressions();
            if (check(OP_RPARENT)) {
                skip();
            } else
                syntaxError("')'");
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_LBRACKET) || check(OP_RBRACKET) || check(OP_RPARENT)
                || check(OP_ASSIGN) || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) ||
                check(OP_AND) || check(OP_EQ) || check(OP_NEQ) || check(OP_LEQ) || check(OP_GEQ) || check(OP_LT) ||
                check(OP_GT) || check(OP_ADD) || check(OP_SUB) || check(OP_MUL) || check(OP_DIV) || check(OP_MOD) ||
                check(KW_THEN) || check(KW_ELSE) || check(EOF)) {
            dump("atomic_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '[', ']', '(', ')', '=', ',', '{', '}', '|', '&', '==', '!=', " +
                    "'<=', '>=', '<', '>', '+', '-', '*', '/', '%', the keywords 'THEN' and 'ELSE' and EOF",
                    "atomic_expression_ -> ( expressions ) | ε");
    }

    void parseExpressions() {
        if (check(IDENTIFIER) || check(OP_LPARENT) || check(OP_LBRACE) || check(OP_ADD) || check(OP_SUB)
                || check(OP_NOT) || check(C_INTEGER) || check(C_LOGICAL) || check(C_STRING)) {
            dump("expressions -> expression expressions_");
            parseExpression();
            parseExpressions_();
        } else
            syntaxError(" -> an identifier, a '(', '{', '+', '-', '!' or a constant: integer, logical or string",
                    "expressions -> expression expressions_");
    }

    void parseExpressions_() {
        if (check(OP_COMMA)) {
            dump("definitions_ -> , expression expressions_");
            skip();
            parseExpression();
            parseExpressions_();
        } else if (check(OP_RPARENT)) {
            dump("definitions_ -> , ε");
        } else
            syntaxError("a ',' or ')'", "definitions_ -> , expression expressions_ | , ε");
    }

    void parseAtomicExpression__() { // atom_expr4
        if (check(KW_IF)) {
            dump("atomic_expression__ -> if expression then expression atomic_expression___");
            skip();
            parseExpression();
            if (check(KW_THEN)) {
                skip();
                parseExpression();
                parseAtomicExpression___();
            } else
                syntaxError("keyword 'THEN'");
        } else if (check(KW_WHILE)) {
            dump("atomic_expression__ -> while expression : expression }");
            skip();
            parseExpression();
            if (check(OP_COLON)) {
                skip();
                parseExpression();
                if (check(OP_RBRACE)) {
                    skip();
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("a ':");
        } else if (check(KW_FOR)) {
            dump("atomic_expression__ -> for id = expression , expression , expression : expression }");
            skip();
            if (check(IDENTIFIER)) {
                skip();
                if (check(OP_ASSIGN)) {
                    skip();
                    parseExpression();
                    if (check(OP_COMMA)) {
                        skip();
                        parseExpression();
                        if (check(OP_COMMA)) {
                            skip();
                            parseExpression();
                            if (check(OP_COLON)) {
                                skip();
                                parseExpression();
                                if (check(OP_RBRACE)) {
                                    skip();
                                } else
                                    syntaxError("a '}'");
                            } else
                                syntaxError("a ','");
                        } else
                            syntaxError("a ','");
                    } else
                        syntaxError("a ','");
                } else
                    syntaxError("a '='");
            } else
                syntaxError("an identifier");
        } else if (check(IDENTIFIER) || check(OP_LPARENT) || check(OP_LBRACE) || check(OP_ADD) || check(OP_SUB) ||
                check(OP_NOT) || check(C_INTEGER) || check(C_LOGICAL) || check(C_STRING)) {
            dump("atomic_expression__ -> expression = expression }");
            parseExpression();
            if (check(OP_ASSIGN)) {
                skip();
                parseExpression();
                if (check(OP_RBRACE)) {
                    skip();
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("a '='");
        } else
            syntaxError(" -> an identifier, a '(', '{', '+', '-', '!' or constant: integer, logical or string");
    }

    void parseAtomicExpression___() { // atom_expr3
        if (check(OP_RBRACE)) {
            dump("atomic_expression___ -> }");
            skip();
        } else if (check(KW_ELSE)) {
            dump("atomic_expression___ -> else expression }");
            skip();
            parseExpression();
            if (check(OP_RBRACE)) {
                skip();
            } else
                syntaxError("a }");
        } else
            syntaxError("a '}' or keyword 'ELSE'", "atomic_expression___ -> } | else expression");
    }

    void parsePostfixExpression_() {
        if (check(OP_LBRACKET)) {
            dump("postfix_expression_ -> [ expression ] postfix_expression_");
            skip();
            parseExpression();
            if (check(OP_RBRACKET)) {
                skip();
                parsePostfixExpression_();
            } else
                syntaxError("a ']'");
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(OP_ADD)
                || check(OP_SUB) || check(OP_MUL) || check(OP_DIV) || check(OP_MOD) || check(KW_THEN) || check(KW_ELSE)
                || check(EOF) ) {
            dump("postfix_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', '[', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', '*', '/', '%', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseMultiplicativeExpression_() {
        if (check(OP_MUL)) {
            dump("multiplicative_expression_ -> * prefix_expression multiplicative_expression_");
            skip();
            parsePrefixExpression();
            parseMultiplicativeExpression_();
        } else if (check(OP_DIV)) {
            dump("multiplicative_expression_ -> / prefix_expression multiplicative_expression_");
            skip();
            parsePrefixExpression();
            parseMultiplicativeExpression_();
        } else if (check(OP_MOD)) {
            dump("multiplicative_expression_ -> % prefix_expression multiplicative_expression_");
            skip();
            parsePrefixExpression();
            parseMultiplicativeExpression_();
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(OP_ADD)
                || check(OP_SUB) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("multiplicative_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', '*', '/', '%', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseAdditiveExpression_() {
        if (check(OP_ADD)) {
            dump("additive_expression_ -> + multiplicative_expression additive_expression_");
            skip();
            parseMultiplicativeExpression();
            parseAdditiveExpression_();
        } else if (check(OP_SUB)) {
            dump("additive_expression_ -> - multiplicative_expression additive_expression_");
            skip();
            parseMultiplicativeExpression();
            parseAdditiveExpression_();
        }  else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(KW_THEN)
                || check(KW_ELSE) || check(EOF) ) {
            dump("additive_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseCompareExpression_() {
        if (check(OP_EQ)) {
            dump("compare_expression_ -> == additive_expression");
            skip();
            parseAdditiveExpression();
        } else if (check(OP_NEQ)) {
            dump("compare_expression_ -> != additive_expression");
            skip();
            parseAdditiveExpression();
        }  else if (check(OP_LEQ)) {
            dump("compare_expression_ -> <= additive_expression");
            skip();
            parseAdditiveExpression();
        } else if (check(OP_GEQ)) {
            dump("compare_expression_ -> >= additive_expression");
            skip();
            parseAdditiveExpression();
        } else if (check(OP_LT)) {
            dump("compare_expression_ -> < additive_expression");
            skip();
            parseAdditiveExpression();
        } else if (check(OP_GT)) {
            dump("compare_expression_ -> > additive_expression");
            skip();
            parseAdditiveExpression();
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND)
                || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("compare_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseLogicalAndExpression_() {
        if (check(OP_AND)) {
            dump("logical_and_expression_ -> & compare_expression logical_and_expression_");
            skip();
            parseCompareExpression();
            parseLogicalAndExpression_();
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(KW_THEN)
                || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseLogicalIorExpression_() {
        if (check(OP_OR)) {
            dump("logical_ior_expression_ -> | logical_and_expression logical_ior_expression_");
            skip();
            parseLogicalAndExpression();
            parseLogicalIorExpression_();
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseExpression_() {
        if (check(OP_LBRACE)) {
            dump("expression_ -> { WHERE definitions }");
            skip();
            if (check(KW_WHERE)) {
                skip();
                parseDefinitions();
                if (check(OP_RBRACE)) {
                    skip();
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("the keyword 'WHERE'");
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_RBRACE) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', the keywords 'THEN' or 'ELSE' or EOF");
    }

    void parseVariableDefinition() {
        if (check(KW_VAR)) {
            dump("variable_definition -> var id : type");
            skip();
            if (check(IDENTIFIER)) {
                skip();
                if (check(OP_COLON)) {
                    skip();
                    parseType();
                } else
                    syntaxError("a ':'");
            } else
                syntaxError("an identifier");
        } else
            syntaxError("the keyword 'VAR'");
    }

    void parseDefinitions_() {
        if (check(OP_SEMICOLON)) {
            dump("definitions_ -> ; def defs_");
            skip();
            parseDefinition();
            parseDefinitions_();
        } else if (check(OP_RBRACE) || check(EOF)) {
            dump("definitions_ ->  ε");
        } else
            syntaxError("a ';', '}' or EOF");
    }
}