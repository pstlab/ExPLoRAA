package it.cnr.istc.pst.exploraa.mobile.ctx;

import androidx.annotation.NonNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.pst.exploraa.api.Message.Token;

public class TokensContext {

    private static final TokensContext instance = new TokensContext();
    /**
     * The tokens received so far.
     */
    private final List<Token> tokens = new ArrayList<>();
    private final Collection<TokensListener> listeners = new ArrayList<>();

    public static TokensContext getInstance() {
        return instance;
    }

    public void addToken(@NonNull final Token token) {
        tokens.add(token);
        for (TokensListener listener : listeners)
            listener.tokenAdded(token);
    }

    public void removeToken(@NonNull final Token token) {
        tokens.remove(token);
        for (TokensListener listener : listeners)
            listener.tokenRemoved(token);
    }

    public List<Token> getTokens() {
        return Collections.unmodifiableList(tokens);
    }

    public void clear() {
        tokens.clear();
        for (TokensListener listener : listeners)
            listener.tokensCleared();
    }

    public void addTokensListener(TokensListener l) {
        listeners.add(l);
    }

    public void removeTokensListener(TokensListener l) {
        listeners.remove(l);
    }

    public interface TokensListener {

        void tokenAdded(Token token);

        void tokenRemoved(Token token);

        void tokensCleared();
    }
}
