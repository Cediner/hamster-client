package hamster.script.pathfinding.waypoint;

import java.util.HashMap;
import java.util.Map;

public class WaypointMap {
    public final Map<Long, Waypoint> waypoints = new HashMap<>();
    public WaypointMap(final Map<Long, Waypoint> waypoints) {
        this.waypoints.putAll(waypoints);
    }

    public Waypoint getWaypoint(final long id) {
        return waypoints.get(id);
    }
}
