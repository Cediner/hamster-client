package hamster.script.pathfinding.waypoint;

import haven.Coord2d;

import java.util.*;

public class Waypoint {
    public final long id;
    public final Coord2d c; // Map coord relative to your character
    public final Set<Long> links = new HashSet<>();

    public Waypoint(final long id, final Coord2d c, final Set<Long> links) {
        this.id = id;
        this.c = c;
        this.links.addAll(links);
    }

    public double dist(final Waypoint ot) {
        return c.dist(ot.c);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Waypoint waypoint = (Waypoint) o;
        return id == waypoint.id;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return "Waypoint{" + id + '}';
    }
}
