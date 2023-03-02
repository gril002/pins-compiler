package compiler.lexer;

import java.util.AbstractMap;
import java.util.Map;

public class TypeRules {
    static final Map<String, TokenType> kws = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("arr", TokenType.KW_ARR), new AbstractMap.SimpleEntry<>("else", TokenType.KW_ELSE),
            new AbstractMap.SimpleEntry<>("for", TokenType.KW_FOR), new AbstractMap.SimpleEntry<>("fun", TokenType.KW_FUN),
            new AbstractMap.SimpleEntry<>("if", TokenType.KW_IF), new AbstractMap.SimpleEntry<>("then", TokenType.KW_THEN),
            new AbstractMap.SimpleEntry<>("typ", TokenType.KW_TYP), new AbstractMap.SimpleEntry<>("var", TokenType.KW_VAR),
            new AbstractMap.SimpleEntry<>("where", TokenType.KW_WHERE), new AbstractMap.SimpleEntry<>("while", TokenType.KW_WHILE));

    static final Map<String, TokenType> atomic = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("logical", TokenType.AT_LOGICAL), new AbstractMap.SimpleEntry<>("integer", TokenType.AT_INTEGER),
            new AbstractMap.SimpleEntry<>("string", TokenType.AT_STRING));

    static final Map<String, TokenType> atomicConst = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("true", TokenType.C_LOGICAL), new AbstractMap.SimpleEntry<>("false", TokenType.C_LOGICAL),
            new AbstractMap.SimpleEntry<>("integer", TokenType.C_INTEGER), new AbstractMap.SimpleEntry<>("string", TokenType.C_STRING));

    static final Map<String, TokenType> ops = Map.ofEntries(
            new AbstractMap.SimpleEntry<>("+", TokenType.OP_ADD), new AbstractMap.SimpleEntry<>("-", TokenType.OP_SUB),
            new AbstractMap.SimpleEntry<>("*", TokenType.OP_MUL), new AbstractMap.SimpleEntry<>("/", TokenType.OP_DIV),
            new AbstractMap.SimpleEntry<>("%", TokenType.OP_MOD), new AbstractMap.SimpleEntry<>("&", TokenType.OP_AND),
            new AbstractMap.SimpleEntry<>("|", TokenType.OP_OR), new AbstractMap.SimpleEntry<>("!", TokenType.OP_NOT),
            new AbstractMap.SimpleEntry<>("==", TokenType.OP_EQ), new AbstractMap.SimpleEntry<>("!=", TokenType.OP_NEQ),
            new AbstractMap.SimpleEntry<>("<", TokenType.OP_LT), new AbstractMap.SimpleEntry<>(">", TokenType.OP_GT),
            new AbstractMap.SimpleEntry<>("<=", TokenType.OP_LEQ), new AbstractMap.SimpleEntry<>(">=", TokenType.OP_GEQ),
            new AbstractMap.SimpleEntry<>("(", TokenType.OP_LPARENT), new AbstractMap.SimpleEntry<>(")", TokenType.OP_RPARENT),
            new AbstractMap.SimpleEntry<>("[", TokenType.OP_LBRACKET), new AbstractMap.SimpleEntry<>("]", TokenType.OP_RBRACKET),
            new AbstractMap.SimpleEntry<>("{", TokenType.OP_LBRACE), new AbstractMap.SimpleEntry<>("}", TokenType.OP_RBRACE),
            new AbstractMap.SimpleEntry<>(":", TokenType.OP_COLON), new AbstractMap.SimpleEntry<>(";", TokenType.OP_SEMICOLON),
            new AbstractMap.SimpleEntry<>(".", TokenType.OP_DOT), new AbstractMap.SimpleEntry<>(",", TokenType.OP_COMMA),
            new AbstractMap.SimpleEntry<>("=", TokenType.OP_ASSIGN));

    public static boolean isKeyWord(String str) {
        return kws.containsKey(str);
    }

    public static boolean isAtomic(String str) {
        return atomic.containsKey(str);
    }

    public static boolean isAtomicConst(String str) {
        return getAtomicConst(str) != null;
    }

    public static boolean isId(String str) {
        return getId(str) != null;
    }

    public static boolean isOp(String str) {
        return ops.containsKey(str);
    }
    
    public static boolean isComment(String str) {
        if (str.charAt(0) != '#')
            return false;
        if (str.length() == 1)
            return true;
        if (str.charAt(str.length()-2) == '\n')
            return false;

        return true;
    }
    
    public static boolean isWt(String str) {
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            if (c != '\n' && c != '\r' && c != '\t' && c != ' ') {
                return false;
            }
        }
        return true;
    }

    public static TokenType getKeyWord(String str) {
        return kws.get(str);
    }

    public static TokenType getAtomic(String str) {
        return atomic.get(str);
    }
    public static TokenType getAtomicConst(String str) {
        int len = str.length();
        if (len == 0)
            return null;
        if (str.equals("true") || str.equals("false"))
            return atomicConst.get(str);

        char c = str.charAt(0);
        if (Character.isDigit(c)) {
            for (int i = 1; i < len; i++) {
                c = str.charAt(i);
                if (c < '0' || c > '9')
                    return null;
            }
            return TokenType.C_INTEGER;
        }

        if (c != '\'')
            return null;
        if (len == 1)
            return TokenType.C_STRING;
        for (int i = 1; i < len; i++) {
            if (str.charAt(i) < 32 || str.charAt(i) > 126) {
                return null;
            }
        }

        int qNum = 0;
        for (int i = 0; i < len; i++) {
            if (str.charAt(i) == '\'')
                qNum++;
        }
        if (qNum%2 == 0) {
            if (str.charAt(len-1) != '\'')
                return null;
        }

        return TokenType.C_STRING;
    }
    public static TokenType getId(String str) {
        if (str.length() == 0)
            return null;
        if (isKeyWord(str) || isAtomic(str) || isAtomicConst(str))
            return null;

        char c = str.charAt(0);
        if (!(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_'))
            return null;

        for (int i = 1; i < str.length(); i++) {
            c = str.charAt(i);
            if (!(c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9' || c == '_'))
                return null;
        }

        return TokenType.IDENTIFIER;
    }
    public static TokenType getOp(String str) {
        return ops.get(str);
    }

    public static String completeString(String str) {
        int len = str.length();
        if (len < 2)
            return null;
        char first = str.charAt(0);
        char last = str.charAt(len-1);

        if(first != '\'' || last != '\'')
            return null;

        int qNum = 0;
        for (int i = 0; i < len; i++) {
            if (str.charAt(i) == '\'')
                qNum++;
        }
        if (qNum%2 == 1) {
            return null;
        }
        StringBuilder compStr = new StringBuilder();
        for (int i = 1; i < len-1; i++) {
            if (len >= 4 && str.charAt(i) == '\'' && str.charAt(i+1) == '\'') {
                compStr.append("'");
                i++;
            }
            else
                compStr.append(str.charAt(i));
        }

        return compStr.toString();
    }
}
