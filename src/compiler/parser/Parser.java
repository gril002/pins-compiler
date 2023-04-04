/**
 * @Author: turk
 * @Description: Sintaksni analizator.
 */

package compiler.parser;

import static compiler.lexer.TokenType.*;
import static common.RequireNonNull.requireNonNull;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import common.Report;
import compiler.lexer.Position;
import compiler.lexer.Symbol;
import compiler.lexer.TokenType;
import compiler.parser.ast.Ast;
import compiler.parser.ast.def.*;
import compiler.parser.ast.expr.*;
import compiler.parser.ast.type.*;

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
        return parseSource();
    }

    private Ast parseSource() {
        dump("source -> definitions");
        return parseDefinitions();
    }

    /**
     * Izpiše produkcijo na izhodni tok.
     */
    private void dump(String production) {
        productionsOutputStream.ifPresent(printStream -> printStream.println(production));
    }

    private boolean check(TokenType token) {
        return symbols.get(pos).tokenType.equals(token);
    }

    private void skip() {
        pos++;
    }

    private Position getCurTokenPos() {
        return symbols.get(pos).position;
    }

    private Symbol getCurSymbol() {
        return symbols.get(pos);
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
    private Defs parseDefinitions() {
        dump("definitions -> definition definitions_");
        List<Def> defList = new ArrayList<>();
        Position.Location start = getCurTokenPos().start;
        defList.add(parseDefinition());
        return parseDefinitions_(start, defList);
    }

    private Def parseDefinition() {
        if(check(KW_TYP)) {
            dump("definition -> type_definition");
            return parseTypeDefinition();
        } else if (check(KW_FUN)) {
            dump("definition -> function_definition");
            return parseFunctionDefinition();
        } else if (check(KW_VAR)) {
            dump("definition -> variable_definition");
            return parseVariableDefinition();
        } else
            syntaxError("a definition", "definition -> type_definition | function_definition | variable_definition");
        return null;
    }

    private Def parseTypeDefinition() {
        Position.Location startLoc = getCurTokenPos().start;
        if (check(KW_TYP)) {
            dump("type_definition -> typ id : type");
            skip();
            if (check(IDENTIFIER)) {
                String name = getCurSymbol().lexeme;
                skip();
                if (check(OP_COLON)) {
                    skip();
                    Type type = parseType();
                    assert type != null;
                    return new TypeDef(new Position(startLoc, type.position.end), name, type);
                } else
                    syntaxError("a ':'", "type_definition -> typ id : type");
            } else
                syntaxError("an identifier", "type_definition -> typ id : type");
        } else
            syntaxError("the keyword 'TYP'", "type_definition -> typ id : type");
        return null;
    }

    private Type parseType() {
        Position.Location start = getCurTokenPos().start;
        if (check(IDENTIFIER)) {
            dump("type -> id");
            Symbol symbol = symbols.get(pos);
            Position.Location end = getCurTokenPos().end;
            skip();
            return new TypeName(new Position(start, end), symbol.lexeme);
        } else if (check(AT_LOGICAL)) {
            dump("type -> logical");
            Position.Location end = getCurTokenPos().end;
            skip();
            return Atom.LOG(new Position(start, end));
        } else if (check(AT_INTEGER)) {
            dump("type -> integer");
            Position.Location end = getCurTokenPos().end;
            skip();
            return Atom.INT(new Position(start, end));
        } else if (check(AT_STRING)) {
            dump("type -> string");
            Position.Location end = getCurTokenPos().end;
            skip();
            return Atom.STR(new Position(start, end));
        } else if (check(KW_ARR)) {
            dump("type -> arr [ int_constant ] type");
            skip();
            if (check(OP_LBRACKET)) {
                skip();
                if (check(C_INTEGER)) {
                    int size = Integer.parseInt(getCurSymbol().lexeme);
                    skip();
                    if (check(OP_RBRACKET)) {
                        skip();
                        Type arrType = parseType();
                        assert arrType != null;
                        return new Array(new Position(start, arrType.position.end), size, arrType);
                    } else
                        syntaxError("']'", "type -> [ int_constant ]");
                } else
                    syntaxError("an integer constant", "type -> [ int_constant");
            } else
                syntaxError("'['", "type -> [");
        } else
            syntaxError("type: identifier, logical, integer string or arr", "type -> id | logical | integer string | arr [ int_constant ] type");
        return null;
    }

    private FunDef parseFunctionDefinition() {
        dump("function_definition -> fun identifier ( parameters ) : type = expression");
        Position.Location start = getCurTokenPos().start;
        if (check(KW_FUN)) {
            skip();
            if (check(IDENTIFIER)) {
                String name = getCurSymbol().lexeme;
                skip();
                if (check(OP_LPARENT)) {
                    skip();
                    List<FunDef.Parameter> params = new ArrayList<>();
                    parseParameters(params);
                    if (check(OP_RPARENT)) {
                        skip();
                        if (check(OP_COLON)) {
                            skip();
                            Type type = parseType();
                            if (check(OP_ASSIGN)) {
                                skip();
                                Expr expr = parseExpression();
                                return new FunDef(new Position(start, expr.position.end), name, params, type, expr);
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
        return null;
    }

    private void parseParameters(List<FunDef.Parameter> params) {
        dump("parameters -> parameter parameters_");
        params.add(parseParameter());
        parseParameters_(params);
    }

    private FunDef.Parameter parseParameter() {
        Position.Location start = getCurTokenPos().start;
        if (check(IDENTIFIER)) {
            dump("parameter -> id : type");
            String name = getCurSymbol().lexeme;
            skip();
            if (check(OP_COLON)) {
                skip();
                Type type = parseType();
                assert type != null;
                return new FunDef.Parameter(new Position(start, type.position.end), name, type);
            } else
                syntaxError("a ':'", "parameter -> id : type");
        } else
            syntaxError("an identifier", "parameter -> id : type");
        return null;
    }

    private void parseParameters_(List<FunDef.Parameter> params) {
        if (check(OP_COMMA)) {
            dump("parameters_ -> , parameter parameters_");
            skip();
            params.add(parseParameter());
            parseParameters_(params);
        } else if (check(OP_RPARENT)) {
            dump("parameters_ -> ε");
        } else
            syntaxError("a ',' or ')'", "parameters_ -> , parameter parameters_ | ε");
    }

    private Expr parseExpression() {
        dump("expression -> logical_ior_expression expression_");
        Position.Location start = getCurTokenPos().start;
        Expr left = parseLogicalIorExpression(start);
        return parseExpression_(left);
    }

    private Expr parseLogicalIorExpression(Position.Location parentStart) {
        dump("logical_ior_expression -> logical_and_expression logical_ior_expression_");
        Expr left = parseLogicalAndExpression(parentStart);
        return parseLogicalIorExpression_(left);
    }

    private Expr parseLogicalAndExpression(Position.Location parentStart) {
        dump("logical_and_expression -> compare_expression logical_and_expression_");
        Expr left = parseCompareExpression(parentStart);
        return parseLogicalAndExpression_(left);
    }

    private Expr parseCompareExpression(Position.Location parentStart) {
        dump("compare_expression -> additive_expression compare_expression_");
        Expr left = parseAdditiveExpression(parentStart);
        return parseCompareExpression_(left);
    }

    private Expr parseAdditiveExpression(Position.Location parentStart) {
        dump("additive_expression -> multiplicative_expression additive_expression_");
        Expr left = parseMultiplicativeExpression(parentStart);
        return parseAdditiveExpression_(left);
    }

    private Expr parseMultiplicativeExpression(Position.Location parentStart) {
        dump("multiplicative_expression -> prefix_expression multiplicative_expression_");
        Expr left = parsePrefixExpression(parentStart);
        return parseMultiplicativeExpression_(left);
    }

    private Expr parsePrefixExpression(Position.Location parentStart) {
        if (check(OP_ADD)) {
            dump("prefix_expression -> + prefix_expression");
            skip();
            Expr expr = parsePrefixExpression(getCurTokenPos().start);
            return new Unary(new Position(parentStart, expr.position.end), expr, Unary.Operator.ADD);
        } else if (check(OP_SUB)) {
            dump("prefix_expression -> - prefix_expression");
            skip();
            Expr expr = parsePrefixExpression(getCurTokenPos().start);
            return new Unary(new Position(parentStart, expr.position.end), expr, Unary.Operator.SUB);
        } else if (check(OP_NOT)) {
            dump("prefix_expression -> ! prefix_expression");
            skip();
            Expr expr = parsePrefixExpression(getCurTokenPos().start);
            return new Unary(new Position(parentStart, expr.position.end), expr, Unary.Operator.NOT);
        } else {
            dump("prefix_expression -> postfix_expression");
            return parsePostfixExpression(parentStart);
        }
    }

    private Expr parsePostfixExpression(Position.Location parentStart) {
        dump("postfix_expression -> atomic_expression postfix_expression_");
        Expr left = parseAtomicExpression(parentStart);
        assert left != null;
        return parsePostfixExpression_(left);
    }

    private Expr parseAtomicExpression(Position.Location parentStart) {
        Symbol symbol = getCurSymbol();
        if (check(IDENTIFIER)) {
            dump("atomic_expression -> identifier atomic_expression_");
            String name = getCurSymbol().lexeme;
            Position.Location end = getCurSymbol().position.end;
            skip();
            Expr expr = parseAtomicExpression_();
            return Objects.requireNonNullElseGet(expr, () -> new Name(new Position(parentStart, end), name));
        } else if (check(OP_LPARENT)) {
            dump("atomic_expression -> ( expressions )");
            skip();
            List<Expr> exprList = new ArrayList<>();
            parseExpressions(exprList);
            if (check(OP_RPARENT)) {
                Position.Location end = getCurTokenPos().end;
                skip();
                return new Block(new Position(symbol.position.start, end), exprList);
            } else
                syntaxError("')'");
        } else if (check(OP_LBRACE)) {
            dump("atomic_expression -> { atomic_expression___");
            skip();
            return parseAtomicExpression__(parentStart);
        } else if (check(C_INTEGER)) {
            dump("atomic_expression -> int_constant");
            skip();
            return new Literal(new Position(parentStart, symbol.position.end), symbol.lexeme, Atom.Type.INT);
        } else if (check(C_LOGICAL)) {
            dump("atomic_expression -> logical_constant");
            skip();
            return new Literal(new Position(parentStart, symbol.position.end), symbol.lexeme, Atom.Type.LOG);
        } else if (check(C_STRING)) {
            dump("atomic_expression -> str_constant");
            skip();
            return new Literal(new Position(parentStart, symbol.position.end), symbol.lexeme, Atom.Type.STR);
        } else
            syntaxError("an identifier, a '(', '{' or a constant: integer, logical or string",
                    "atomic_expression -> identifier atomic_expression_ | ( expressions ) | atomic_expression " +
                            "-> { atomic_expression___ | int_constant | logical_constant | str_constant");
        return null;
    }

    private Expr parseAtomicExpression_() {
        Position.Location start = getCurTokenPos().start;
        if (check(OP_LPARENT)) {
            Symbol prev = symbols.get(pos-1);
            dump("atomic_expression_ -> ( expressions )");
            skip();
            List<Expr> exprList = new ArrayList<>();
            parseExpressions(exprList);
            if (check(OP_RPARENT)) {
                Position.Location end = getCurTokenPos().end;
                skip();
                if (prev.tokenType == IDENTIFIER)
                    return new Call(new Position(prev.position.start, end), exprList, prev.lexeme);
                else
                    return new Block(new Position(start, exprList.get(exprList.size()-1).position.end), exprList);
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
        return null;
    }

    private void parseExpressions(List<Expr> exprList) {
        if (check(IDENTIFIER) || check(OP_LPARENT) || check(OP_LBRACE) || check(OP_ADD) || check(OP_SUB)
                || check(OP_NOT) || check(C_INTEGER) || check(C_LOGICAL) || check(C_STRING)) {
            dump("expressions -> expression expressions_");
            exprList.add(parseExpression());
            parseExpressions_(exprList);
        } else
            syntaxError(" -> an identifier, a '(', '{', '+', '-', '!' or a constant: integer, logical or string",
                    "expressions -> expression expressions_");
    }

    private void parseExpressions_(List<Expr> exprList) {
        if (check(OP_COMMA)) {
            dump("definitions_ -> , expression expressions_");
            skip();
            exprList.add(parseExpression());
            parseExpressions_(exprList);
        } else if (check(OP_RPARENT)) {
            dump("definitions_ -> , ε");
        } else
            syntaxError("a ',' or ')'", "definitions_ -> , expression expressions_ | , ε");
    }

    private Expr parseAtomicExpression__(Position.Location parentStart) { // atom_expr4
        if (check(KW_IF)) {
            dump("atomic_expression__ -> if expression then expression atomic_expression___");
            skip();
            Expr condition = parseExpression();
            if (check(KW_THEN)) {
                skip();
                Expr thenExpr = parseExpression();
                //parseAtomicExpression___();

                if (check(KW_ELSE)) {
                    dump("atomic_expression___ -> else expression }");
                    skip();
                    Expr elseExpr = parseExpression();
                    if (check(OP_RBRACE)) {
                        Position.Location end = getCurTokenPos().end;
                        skip();
                        return new IfThenElse(new Position(parentStart, end), condition, thenExpr, elseExpr);
                    } else
                        syntaxError("a '}'");
                }
                if (check(OP_RBRACE)) {
                    Position.Location end = getCurTokenPos().end;
                    skip();
                    return new IfThenElse(new Position(parentStart, end), condition, thenExpr);
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("keyword 'THEN'");
        } else if (check(KW_WHILE)) {
            dump("atomic_expression__ -> while expression : expression }");
            skip();
            Expr condition = parseExpression();
            if (check(OP_COLON)) {
                skip();
                Expr expr = parseExpression();
                if (check(OP_RBRACE)) {
                    Position.Location end = getCurTokenPos().end;
                    skip();
                    return new While(new Position(parentStart, end), condition, expr);
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("a ':");
        } else if (check(KW_FOR)) {
            dump("atomic_expression__ -> for id = expression , expression , expression : expression }");
            skip();
            if (check(IDENTIFIER)) {
                Name name = new Name(getCurTokenPos(), getCurSymbol().lexeme);
                skip();
                if (check(OP_ASSIGN)) {
                    skip();
                    Expr low = parseExpression();
                    if (check(OP_COMMA)) {
                        skip();
                        Expr high = parseExpression();
                        if (check(OP_COMMA)) {
                            skip();
                            Expr step = parseExpression();
                            if (check(OP_COLON)) {
                                skip();
                                Expr expr = parseExpression();
                                if (check(OP_RBRACE)) {
                                    Position.Location end = getCurTokenPos().end;
                                    skip();
                                    return new For(new Position(parentStart, end), name ,low, high, step, expr);
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
            Expr left = parseExpression();
            if (check(OP_ASSIGN)) {
                skip();
                Expr right = parseExpression();
                if (check(OP_RBRACE)) {
                    Position.Location end = getCurTokenPos().end;
                    skip();
                    return new Binary(new Position(parentStart, end), left, Binary.Operator.ASSIGN, right);
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("a '='");
        } else
            syntaxError(" -> an identifier, a '(', '{', '+', '-', '!' or constant: integer, logical or string");
        return null;
    }

    private Expr parsePostfixExpression_(Expr parentLeft) {
        if (check(OP_LBRACKET)) {
            dump("postfix_expression_ -> [ expression ] postfix_expression_");
            skip();
            Expr expr = parseExpression();
            if (check(OP_RBRACKET)) {
                Position.Location end = getCurTokenPos().end;
                skip();
                Binary bin = new Binary(new Position(parentLeft.position.start, end), parentLeft, Binary.Operator.ARR, expr);
                Expr right = parsePostfixExpression_(bin);
                assert right != null;
                return right;
            } else
                syntaxError("a ']'");
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(OP_ADD)
                || check(OP_SUB) || check(OP_MUL) || check(OP_DIV) || check(OP_MOD) || check(KW_THEN) || check(KW_ELSE)
                || check(EOF) ) {
            dump("postfix_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', '[', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', '*', '/', '%', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseMultiplicativeExpression_(Expr parentLeft) {
        Position.Location parentStart = parentLeft.position.start;
        if (check(OP_MUL)) {
            dump("multiplicative_expression_ -> * prefix_expression multiplicative_expression_");
            skip();
            Expr left = parsePrefixExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentStart, left.position.end), parentLeft, Binary.Operator.MUL, left);
            Expr right = parseMultiplicativeExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_DIV)) {
            dump("multiplicative_expression_ -> / prefix_expression multiplicative_expression_");
            skip();
            Expr left = parsePrefixExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentStart, left.position.end), parentLeft, Binary.Operator.DIV, left);
            Expr right = parseMultiplicativeExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_MOD)) {
            dump("multiplicative_expression_ -> % prefix_expression multiplicative_expression_");
            skip();
            Expr left = parsePrefixExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentStart, left.position.end), parentLeft, Binary.Operator.MOD, left);
            Expr right = parseMultiplicativeExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(OP_ADD)
                || check(OP_SUB) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("multiplicative_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', '*', '/', '%', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseAdditiveExpression_(Expr parentLeft) {
        Position.Location parentStart = parentLeft.position.start;
        if (check(OP_ADD)) {
            dump("additive_expression_ -> + multiplicative_expression additive_expression_");
            skip();
            Expr left = parseMultiplicativeExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentStart, left.position.end), parentLeft, Binary.Operator.ADD, left);
            Expr right = parseAdditiveExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_SUB)) {
            dump("additive_expression_ -> - multiplicative_expression additive_expression_");
            skip();
            Expr left = parseMultiplicativeExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentStart, left.position.end), parentLeft, Binary.Operator.SUB, left);
            Expr right = parseAdditiveExpression_(bin);
            assert right != null;
            return right;
        }  else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND) || check(OP_EQ)
                || check(OP_NEQ) || check(OP_LEQ)|| check(OP_GEQ) || check(OP_LT) || check(OP_GT) || check(KW_THEN)
                || check(KW_ELSE) || check(EOF) ) {
            dump("additive_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', '+', '-', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseCompareExpression_(Expr parentLeft) {
        Position.Location parentStart = parentLeft.position.start;
        if (check(OP_EQ)) {
            dump("compare_expression_ -> == additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.EQ, right);
        } else if (check(OP_NEQ)) {
            dump("compare_expression_ -> != additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.NEQ, right);
        }  else if (check(OP_LEQ)) {
            dump("compare_expression_ -> <= additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.LEQ, right);
        } else if (check(OP_GEQ)) {
            dump("compare_expression_ -> >= additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.GEQ, right);
        } else if (check(OP_LT)) {
            dump("compare_expression_ -> < additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.LT, right);
        } else if (check(OP_GT)) {
            dump("compare_expression_ -> > additive_expression");
            skip();
            Expr right = parseAdditiveExpression(getCurTokenPos().start);
            return new Binary(new Position(parentStart, right.position.end), parentLeft, Binary.Operator.GT, right);
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(OP_AND)
                || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("compare_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', '==', '!=', '<=', '>=', '<'," +
                    " '>', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseLogicalAndExpression_(Expr parentLeft) {
        if (check(OP_AND)) {
            dump("logical_and_expression_ -> & compare_expression logical_and_expression_");
            skip();
            Expr left = parseCompareExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentLeft.position.start, left.position.end), parentLeft, Binary.Operator.AND, left);
            Expr right = parseLogicalAndExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(OP_OR) || check(KW_THEN)
                || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', '&', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseLogicalIorExpression_(Expr parentLeft) {
        if (check(OP_OR)) {
            dump("logical_ior_expression_ -> | logical_and_expression logical_ior_expression_");
            skip();
            Expr left = parseLogicalAndExpression(getCurTokenPos().start);
            Binary bin = new Binary(new Position(parentLeft.position.start, left.position.end), parentLeft, Binary.Operator.OR, left);
            Expr right = parseLogicalIorExpression_(bin);
            assert right != null;
            return right;
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_LBRACE) || check(OP_RBRACE) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', '|', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Expr parseExpression_(Expr parentLeft) {
        if (check(OP_LBRACE)) {
            dump("expression_ -> { WHERE definitions }");
            skip();
            if (check(KW_WHERE)) {
                skip();
                Defs defs = parseDefinitions();
                if (check(OP_RBRACE)) {
                    Position.Location end = getCurTokenPos().end;
                    skip();
                    return new Where(new Position(parentLeft.position.start, end), parentLeft, defs);
                } else
                    syntaxError("a '}'");
            } else
                syntaxError("the keyword 'WHERE'");
        } else if (check(OP_SEMICOLON) || check(OP_COLON) || check(OP_RBRACKET) || check(OP_RPARENT) || check(OP_ASSIGN)
                || check(OP_COMMA) || check(OP_RBRACE) || check(KW_THEN) || check(KW_ELSE) || check(EOF) ) {
            dump("logical_and_expression_ -> ε");
            return parentLeft;
        } else
            syntaxError("a ';', ':', '', ']', ')', '=', ',', '{', '}', the keywords 'THEN' or 'ELSE' or EOF");
        return null;
    }

    private Def parseVariableDefinition() {
        Position.Location start = getCurTokenPos().start;
        if (check(KW_VAR)) {
            dump("variable_definition -> var id : type");
            skip();
            if (check(IDENTIFIER)) {
                String name = symbols.get(pos).lexeme;
                skip();
                if (check(OP_COLON)) {
                    skip();
                    Type type = parseType();
                    assert type != null;
                    return new VarDef(new Position(start, type.position.end), name, type);
                } else
                    syntaxError("a ':'");
            } else
                syntaxError("an identifier");
        } else
            syntaxError("the keyword 'VAR'");
        return null;
    }

    private Defs parseDefinitions_(Position.Location parentStart, List<Def> defList) {
        if (check(OP_SEMICOLON)) {
            dump("definitions_ -> ; def defs_");
            skip();
            defList.add(parseDefinition());
            return parseDefinitions_(parentStart, defList);
        } else if (check(OP_RBRACE)) {
            dump("definitions_ ->  ε");
            return new Defs(new Position(parentStart, defList.get(defList.size()-1).position.end), defList);
        } else if (check(EOF)) {
            dump("definitions_ ->  ε");
            return new Defs(new Position(parentStart, defList.get(defList.size()-1).position.end), defList);
        } else
            syntaxError("a ';', '}' or EOF");
        return null;
    }
}