package hamster;

import haven.Coord;
import haven.Coord2d;
import haven.Coord3f;
import haven.render.BaseColor;

import hamster.io.Storage;

import java.awt.*;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Simple ini-like file reader/writer for storing our settings that will persistent across sessions
 * <p>
 * All settings will be read in and parsed according to what they look like. It will support:
 * - String
 * - Boolean 		^(true)|(false)$
 * - Int 		^([0-9]+)$
 * - Double 		^([0-9]*\.[0-9]+)$
 * - Coord             ^((Int), (Int))$
 * - Coord2d           ^((Double), (Double))$
 * - Coord3f           ^((Double), (Double), (Double))$
 * - String arrays 	^[ String, ... ]$
 * - RGBA 0-255        ^(Int),(Int),(Int),(Int)$
 * <p>
 * It also has section support like ini's
 * <p>
 * Values are stored via (section, key, value)
 * Values are fetched via (section, key) or (section, key, default)
 * <p>
 * # at the start of a line will be comments
 */
public class Settings {
    private static final Storage setdb = Storage.create("jdbc:sqlite:data/settings.db")
            .orElseThrow(() -> new RuntimeException("Failed to open map database"));
    private static final PreparedStatement update;
    private static final Pattern
            bool = Pattern.compile("^((true)|(false))$"),
            intval = Pattern.compile("^(-?[0-9]+)$"),
            dval = Pattern.compile("^(-?[0-9]*\\.[0-9]+)$"),
            coord = Pattern.compile("^\\((-?[0-9]+), (-?[0-9]+)\\)$"),
            coord2d = Pattern.compile("^\\((-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+)\\)$"),
            coord3f = Pattern.compile("^\\((-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+), (-?[0-9]*\\.[0-9]+)\\)$"),
            strarr = Pattern.compile("^\\[(.+)]$"),
            color = Pattern.compile("^([0-9]+),([0-9]+),([0-9]+),([0-9]+)$"),
            basecolor = Pattern.compile("#<basecolor ([0-9]+\\.[0-9]+) ([0-9]+\\.[0-9]+) ([0-9]+\\.[0-9]+) ([0-9]+\\.[0-9]+)>");
    private static final Map<Pattern, ParseFun> parsers;

    static {
        setdb.ensure(sql -> {
            try(final Statement stmt = sql.createStatement()) {
                stmt.execute("CREATE TABLE IF NOT EXISTS settings ( account TEXT, key TEXT, value TEXT, " +
                        "CONSTRAINT pk_settings PRIMARY KEY (account, key))");
            }
        });

        update = setdb.ensurePrepare("INSERT INTO settings (account, key, value) VALUES (?, ?, ?) " +
                "ON CONFLICT (account, key) DO UPDATE SET " +
                "value=excluded.value");

        parsers = new HashMap<>();
        parsers.put(bool, (settings, key, val) -> settings.rawset(key, val.group(1).equals("true")));
        parsers.put(intval, (settings, key, val) -> settings.rawset(key, Integer.parseInt(val.group(1))));
        parsers.put(dval, (settings, key, val) -> settings.rawset(key, Double.parseDouble(val.group(1))));
        parsers.put(coord, (settings, key, val) ->
                settings.rawset(key, new Coord(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)))));
        parsers.put(coord2d, (settings, key, val) ->
                settings.rawset(key, new Coord2d(Double.parseDouble(val.group(1)), Double.parseDouble(val.group(2)))));
        parsers.put(coord3f, (settings, key, val) ->
                settings.rawset(key, new Coord3f(Float.parseFloat(val.group(1)), Float.parseFloat(val.group(2)), Float.parseFloat(val.group(3)))));
        parsers.put(strarr, (settings, key, val) ->
                settings.rawset(key, val.group(1).split(",")));
        parsers.put(color, (settings, key, val) ->
                settings.rawset(key,
                        new Color(Integer.parseInt(val.group(1)), Integer.parseInt(val.group(2)),
                                Integer.parseInt(val.group(3)), Integer.parseInt(val.group(4)))));
        parsers.put(basecolor, (settings, key, val) ->
            settings.rawset(key,
                    new BaseColor(Float.parseFloat(val.group(1)), Float.parseFloat(val.group(2)),
                            Float.parseFloat(val.group(3)), Float.parseFloat(val.group(4)))));
    }

    @FunctionalInterface
    interface ParseFun {
        void parse(final Settings settings, final String key, final Matcher val);
    }

    private final Map<String, Object> settings;
    private final String key;
    private final boolean writeback;


    Settings(final String key, final boolean writeback) {
        this.key = key;
        this.writeback = writeback;
        settings = new TreeMap<>();
        load();
    }

    Settings(final String key) {
        this(key, true);
    }

    /**
     * Loads our settings from our file
     */
    private void load() {
        try {
            final PreparedStatement stmt = setdb.ensurePrepare("SELECT key, value FROM settings WHERE account = ?");
            stmt.setString(1, key);
            try (final ResultSet res = stmt.executeQuery()) {
                read:
                while (res.next()) {
                    final String k = res.getString(1);
                    final String v = res.getString(2);
                    for (final Pattern pat : parsers.keySet()) {
                        final Matcher find = pat.matcher(v);
                        if (find.find()) {
                            parsers.get(pat).parse(this, k, find);
                            continue read;
                        }
                    }
                    settings.put(k, v);
                }
            }
        } catch (SQLException sqle) {
            sqle.printStackTrace();
            System.exit(1);
        }
    }

    /**
     * Fetches a value from our settings, if it exists. Returns the `def` value if not
     */
    public <T> T get(final String key, final T def, final Class<T> cls) {
        if (settings.containsKey(key)) {
            return cls.cast(settings.get(key));
        } else {
            return def;
        }
    }

    /**
     * Fetches a value from our settings, if it exists. Return null if not.
     */
    public <T> T get(final String key, final Class<T> cls) {
        return get(key, null, cls);
    }

    /**
     * Fetches a value from our settings, if it exists. Return null if not.
     */
    public Object get(final String key, final Object def) {
        return settings.getOrDefault(key, def);
    }

    public Settings rawset(final String key, final Object val) {
        settings.put(key, val);
        return this;
    }

    /**
     * Sets a setting to the specified value
     */
    public Settings set(final String key, final Object val) {
        settings.put(key, val);
        if(writeback) {
            setdb.write(sql -> {
                update.setString(1, this.key);
                update.setString(2, key);
                if (!(val instanceof Color)) {
                    update.setString(3, val.toString());
                } else {
                    final Color c = (Color) val;
                    update.setString(3, String.format("%d,%d,%d,%d",
                            c.getRed(), c.getGreen(), c.getBlue(), c.getAlpha()));
                }
                update.execute();
            });
        }
        return this;
    }
}
