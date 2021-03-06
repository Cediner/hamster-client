package hamster;

import java.util.function.Function;

/**
 * Indirect way of accessing a settings value. Widgets using these should be aware as to allow itself to update
 * its state to match the actual value of this setting rather than saving a local copy
 * <p>
 * Keeps the access logic more simple and keeps verification out of ui logic
 */
public class IndirSetting<T> {
    /**
     * Additional indirection with Keys to support ones that will vary over time
     * Prime case: Things related to the current Theme.
     */
    public interface Key {
        String key();
    }

    public static class StaticKey implements Key {
        private final String key;

        public StaticKey(final String key) {
            this.key = key;
        }

        public String key() {
            return key;
        }
    }

    public static class IndirFormatKey implements Key {
        private final String fmt;
        private final IndirSetting<String> vkey;

        public IndirFormatKey(final String fmt, final IndirSetting<String> vkey) {
            this.fmt = fmt;
            this.vkey = vkey;
        }

        public String key() {
            return String.format(fmt, vkey.get());
        }
    }

    private final Settings settings;
    private final Key key;
    private final T defval;

    //Some settings require verification before actually setting them to the value.
    // If it fails the new value will be ignored
    private final Function<T, Boolean> verification;

    public IndirSetting(final Settings settings, final Key name, final T defval, final Function<T, Boolean> verification) {
        this.key = name;
        this.defval = defval;
        this.settings = settings;
        this.verification = verification;
    }

    public IndirSetting(final Settings settings, final String name, final T defval, final Function<T, Boolean> verification) {
        this(settings, new StaticKey(name), defval, verification);
    }

    //Most things don't need verification
    public IndirSetting(final Settings settings, final Key name, final T defval) {
        this(settings, name, defval,null);
    }

    //Most things don't need verification and most keys are static
    public IndirSetting(final Settings settings, final String name, final T defval) {
        this(settings, new StaticKey(name), defval, null);
    }

    @SuppressWarnings("unchecked")
    public T get() {
        return (T) settings.get(key.key(), defval);
    }

    public void set(T val) {
        if (verification == null || verification.apply(val))
            settings.set(key.key(), val);
    }
}
