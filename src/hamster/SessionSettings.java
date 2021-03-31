package hamster;

/**
 * Session based settings based off account:character name
 *
 * No longer really used anymore as people don't like the idea, but will keep around in
 * the event we go back to using them.
 */
public class SessionSettings {

    public SessionSettings(final String account, final String character) {
        final Settings local = new Settings(String.format("%s:%s", account, character));
    }
}
