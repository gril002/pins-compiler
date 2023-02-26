package compiler.lexer;

import java.util.AbstractMap;
import java.util.Map;

public class TypeRules {
    static final Map<String, TokenType> kws = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("arr", TokenType.KW_ARR),
            new AbstractMap.SimpleEntry<>("else", TokenType.KW_ELSE),
            new AbstractMap.SimpleEntry<>("for", TokenType.KW_FOR),
            new AbstractMap.SimpleEntry<>("fun", TokenType.KW_FUN),
            new AbstractMap.SimpleEntry<>("if", TokenType.KW_IF),
            new AbstractMap.SimpleEntry<>("then", TokenType.KW_THEN),
            new AbstractMap.SimpleEntry<>("typ", TokenType.KW_TYP),
            new AbstractMap.SimpleEntry<>("var", TokenType.KW_VAR),
            new AbstractMap.SimpleEntry<>("where", TokenType.KW_WHERE),
            new AbstractMap.SimpleEntry<>("while", TokenType.KW_WHILE));
    //Map<String, TokenType> kw = Map.of("arr", TokenType.KW_ARR, "else", TokenType.KW_ELSE, "for", TokenType.KW_FOR,
    //        "fun", TokenType.KW_FUN, "if", TokenType.KW_IF, "then", TokenType.KW_THEN, "typ", TokenType.KW_TYP,
    //        "var", TokenType.KW_VAR, "where", TokenType.KW_WHERE, "while", TokenType.KW_WHILE);
    //String[] keyWords = {"arr", "else", "for", "fun", "if", "then", "typ", "var", "where", "while"};

    static final Map<String, TokenType> atomic = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("logical", TokenType.AT_LOGICAL),
            new AbstractMap.SimpleEntry<>("integer", TokenType.AT_INTEGER),
            new AbstractMap.SimpleEntry<>("string", TokenType.AT_STRING));

    static final Map<String, TokenType> atomicConst = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("true", TokenType.C_LOGICAL),
            new AbstractMap.SimpleEntry<>("false", TokenType.C_LOGICAL),
            new AbstractMap.SimpleEntry<>("integer", TokenType.C_INTEGER),
            new AbstractMap.SimpleEntry<>("string", TokenType.C_STRING));

    static final Map<String, TokenType> ops = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("+", TokenType.OP_ADD),
            new AbstractMap.SimpleEntry<>("-", TokenType.OP_SUB),
            new AbstractMap.SimpleEntry<>("*", TokenType.OP_MUL),
            new AbstractMap.SimpleEntry<>("/", TokenType.OP_DIV),
            new AbstractMap.SimpleEntry<>("%", TokenType.OP_MOD),
            new AbstractMap.SimpleEntry<>("&", TokenType.OP_AND),
            new AbstractMap.SimpleEntry<>("|", TokenType.OP_OR),
            new AbstractMap.SimpleEntry<>("!", TokenType.OP_NOT),
            new AbstractMap.SimpleEntry<>("==", TokenType.OP_EQ),
            new AbstractMap.SimpleEntry<>("!=", TokenType.OP_NEQ),
            new AbstractMap.SimpleEntry<>("<", TokenType.OP_LT),
            new AbstractMap.SimpleEntry<>(">", TokenType.OP_GT),
            new AbstractMap.SimpleEntry<>("<=", TokenType.OP_LEQ),
            new AbstractMap.SimpleEntry<>(">=", TokenType.OP_GEQ),
            new AbstractMap.SimpleEntry<>("(", TokenType.OP_LPARENT),
            new AbstractMap.SimpleEntry<>(")", TokenType.OP_RPARENT),
            new AbstractMap.SimpleEntry<>("[", TokenType.OP_LBRACKET),
            new AbstractMap.SimpleEntry<>("]", TokenType.OP_RBRACKET),
            new AbstractMap.SimpleEntry<>("{", TokenType.OP_LBRACE),
            new AbstractMap.SimpleEntry<>("}", TokenType.OP_RBRACE),
            new AbstractMap.SimpleEntry<>(":", TokenType.OP_COLON),
            new AbstractMap.SimpleEntry<>(";", TokenType.OP_SEMICOLON),
            new AbstractMap.SimpleEntry<>(".", TokenType.OP_DOT),
            new AbstractMap.SimpleEntry<>(",", TokenType.OP_COMMA),
            new AbstractMap.SimpleEntry<>("=", TokenType.OP_ASSIGN));

    public static boolean isKeyWord(String str) {
        return kws.containsKey(str);
    }

    public static boolean isAtomic(String str) {
        return atomic.containsKey(str);
    }

    public static boolean isAtomicConst(String str) {
        if (str.equals("true") ||str.equals("false"))
            return true;

        char c = str.charAt(0);
        if (c == '+' || c == '-' || Character.isDigit(c)) {
            // TODO implement number checking
            return true;
        }

        for (int i = 0; i < str.length(); i++) {
            if (str.charAt(i) < 32 || str.charAt(i) > 126)
                return false;
            // TODO check for ' escape
        }

        return true;
    }

    public static boolean isId(String str) {
        if (isKeyWord(str) || isAtomic(str) || isAtomicConst(str))
            return false;

        char c = str.charAt(0);
        if (!(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_'))
            return false;

        for (int i = 1; i < str.length(); i++) {
            c = str.charAt(i);
            if (!(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '_'))
                return false;
        }

        return true;
    }

    public static boolean isOp(String str) {
        return ops.containsKey(str);
    }

    //public static boolean isWt(String str) {
    //
    //}

    //public static TokenType getStartType(String str) {
    //    if (isKeyWord(str))
    //        return
    //}
}
