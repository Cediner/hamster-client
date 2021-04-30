package hamster.script.pathfinding.waypoint;

import com.google.common.flogger.FluentLogger;

import java.util.HashMap;
import java.util.Map;

public class WaypointMap {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public final Map<Long, Waypoint> waypoints = new HashMap<>();
    public final Waypoint start, goal;

    public WaypointMap(final Map<Long, Waypoint> waypoints, final Waypoint start, final Waypoint goal) {
        this.start = start;
        this.goal = goal;
        this.waypoints.putAll(waypoints);
    }

    public Waypoint getWaypoint(final long id) {
        return waypoints.get(id);
    }
}
