package hamster.script;

public class ScriptDescription {
    enum Type {
        Lisp, Lua
    }

    public final String name;
    public final Type type;

    public ScriptDescription(final String name, final Type type) {
        this.name = name;
        this.type = type;
    }
}
