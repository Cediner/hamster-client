package hamster.gob;

import hamster.io.Storage;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.Map;


public class Growth  {
    private static final Map<String, Integer> max = new HashMap<>();
    private static final Map<String, Integer> min = new HashMap<>();

    public static void init(final Storage internal) {
        internal.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("SELECT object.name, growth.min_stage, growth.final_stage FROM object JOIN growth USING (object_id)")) {
                    while (res.next()) {
                        min.put(res.getString(1), res.getInt(2));
                        max.put(res.getString(1), res.getInt(3));
                    }
                }
            }
        });
    }

    public static boolean isGrowth(final String resname) {
        return max.containsKey(resname);
    }

    public static int maxstage(final String resname) {
        return max.getOrDefault(resname, -1);
    }
    public static int minstage(final String resname) { return min.getOrDefault(resname, 99); }
}
