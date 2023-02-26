/**
 * @Author: turk
 * @Description: Leksikalni analizator.
 */

package compiler.lexer;

import static common.RequireNonNull.requireNonNull;
import static compiler.lexer.TokenType.*;
import compiler.lexer.Position.Location;

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
        TokenType curToken = null;
        int pos = 0;
        int line = 0;
        int column = 0;

        for (pos = 0; pos < this.source.length(); pos++) {
            char c = this.source.charAt(pos);
            if ((c < 32 && c != '\n' && c != '\r' && c != '\t') || c == 127)
                Report.error(Position.fromLocation(new Location(line, column)), String.format("Invalid character %c", c));
            //if (curToken == null) {
            //    if (Character.is)
            //}
            column++;
        }
        symbols.add(new Symbol(Position.fromLocation(new Location(line, column)), EOF, "EOF"));
        return symbols;
    }
}
