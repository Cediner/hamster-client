package hamster.gob;

import hamster.io.Storage;

import java.sql.ResultSet;
import java.sql.Statement;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public enum Tag {
    PLANT,
    MULTISTAGE_PLANT,
    HUMAN,
    VEHICLE,
    WATER_VEHICLE,
    LAND_VEHICLE,
    SNOW_VEHICLE,
    ANIMAL,
    TAMED_ANIMAL,
    MEAN_ANIMAL,
    SMALL_ANIMAL,
    CAN_PICK,
    CAN_OPEN,
    CAN_BOARD,
    CAN_FIGHT,
    SIEGE_VEHICLE;

    private static final Map<String, Set<Tag>> tags = new HashMap<>();

    public static void init(final Storage local) {
        local.ensure(sql -> {
            try (final Statement stmt = sql.createStatement()) {
                try (final ResultSet res = stmt.executeQuery("select name, tag_name from object_tag join object using(object_id) join tags using (tag_id)")) {
                    while (res.next()) {
                        final String name = res.getString(1);
                        final Set<Tag> mytags = tags.getOrDefault(name, new HashSet<>());
                        mytags.add(Tag.valueOf(res.getString(2)));
                        tags.put(name, mytags);
                    }
                }
            }
        });
    }

    public static Set<Tag> getTags(final String name) {
        return tags.getOrDefault(name, new HashSet<>());
    }
}
