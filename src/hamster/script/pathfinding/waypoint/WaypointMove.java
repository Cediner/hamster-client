package hamster.script.pathfinding.waypoint;

import hamster.script.pathfinding.Move;

/**
 * Future iterations of this may include pathfinding to the waypoint
 * as an option. May not be worth it and would rely on the user
 * having specific options on or the performance would be awful
 * over very large distances
 */
public class WaypointMove extends Move {
    public WaypointMove(final Waypoint wp) {
        super(wp.c);
    }
}
