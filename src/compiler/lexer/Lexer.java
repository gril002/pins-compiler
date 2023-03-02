/**
 * @Author: turk
 * @Description: Leksikalni analizator.
 */

package compiler.lexer;

import static common.RequireNonNull.requireNonNull;
import static compiler.lexer.TokenType.*;
import compiler.lexer.Position.Location;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import common.Report;

public class Lexer {
    /**
     * Izvorna koda.
     */
    private final String source;

    /**
     * Preslikava iz ključnih besed v vrste simbolov.
     */
    private final static Map<String, TokenType> keywordMapping;

    static {
        keywordMapping = new HashMap<>();
        for (var token : TokenType.values()) {
            var str = token.toString();
            if (str.startsWith("KW_")) {
                keywordMapping.put(str.substring("KW_".length()).toLowerCase(), token);
            }
            if (str.startsWith("AT_")) {
                keywordMapping.put(str.substring("AT_".length()).toLowerCase(), token);
            }
        }
    }

    /**
     * Ustvari nov analizator.
     * 
     * @param source Izvorna koda programa.
     */
    public Lexer(String source) {
        requireNonNull(source);
        this.source = source;
    }

    /**
     * Izvedi leksikalno analizo.
     * 
     * @return seznam leksikalnih simbolov.
     */
    public List<Symbol> scan() {
        var symbols = new ArrayList<Symbol>();
        int pos = 0;
        int line = 1;
        int column = 1;
        StringBuilder str = new StringBuilder();

        boolean[] prevTypes = new boolean[6];
        int prevValid = 0;

        while (pos <= source.length() && source.length() != 0) {
            boolean last = pos == source.length();

            char c = 't';
            if (!last) {
                c = this.source.charAt(pos);
                str.append(c);
            } else
                c = str.charAt(0);

            //if ((c < 32 || c > 126) && c != '\n' && c != '\r' && c != '\t')
            //    Report.error(Position.fromLocation(new Location(line, column)), String.format("Invalid character %c", c));

            boolean[] possibleTypes = new boolean[6];
            boolean valid = TypeRules.isKeyWord(str.toString());
            possibleTypes[0] = valid;
            valid = TypeRules.isAtomic(str.toString());
            possibleTypes[1] = valid;
            valid = TypeRules.isAtomicConst(str.toString());
            possibleTypes[2] = valid;
            valid = TypeRules.isId(str.toString());
            possibleTypes[3] = valid;
            valid = TypeRules.isOp(str.toString());
            possibleTypes[4] = valid;
            valid = TypeRules.isComment(str.toString());
            possibleTypes[5] = valid;

            int curValid = 0;
            for (boolean possibleType : possibleTypes) {
                if (possibleType)
                    curValid++;
            }

            if (last)
                prevValid = curValid;
            if (curValid == 0 || last) {
                if (prevValid >= 1) {

                    TokenType type = null;
                    String prevStr = str.substring(0, str.length()-1);
                    if (last) {
                        prevStr = str.toString();
                        prevTypes = possibleTypes.clone();
                    }

                    if (prevTypes[0])
                        type = TypeRules.getKeyWord(prevStr);
                    else if (prevTypes[1])
                        type = TypeRules.getAtomic(prevStr);
                    else if (prevTypes[2])
                        type = TypeRules.getAtomicConst(prevStr);
                    else if (prevTypes[3])
                        type = TypeRules.getId(prevStr);
                    else if (prevTypes[4])
                        type = TypeRules.getOp(prevStr);
                    if (type != null) {
                        symbols.add(new Symbol(new Position(new Location(line, column-prevStr.length()), new Location(line, column)), type, prevStr));
                    }
                    str = new StringBuilder("");
                    possibleTypes = new boolean[6];
                    if (!last) {
                        pos--;
                        column--;
                    }
                } else if (TypeRules.isWt(str.toString())){
                    str = new StringBuilder("");
                    if (c == '\n') {
                        column = 0;
                        line++;
                    } else if (c == '\t') {
                        column += 4;
                    }
                } else
                    Report.error(Position.fromLocation(new Location(line, column)), String.format("Lexeme %s does not match any type!", str.toString()));
            }
            prevValid = curValid;
            prevTypes = possibleTypes.clone();
            pos++;
            column++;
        }
        symbols.add(new Symbol(Position.fromLocation(new Location(line, column)), EOF, "$"));
        return symbols;
    }
}
