package hamster.script.pathfinding.waypoint;

import haven.Coord2d;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Waypoint {
    public final long id;
    public final Coord2d c; // Map coord relative to your character
    public final List<Long> links = new ArrayList<>();

    public Waypoint(final long id, final Coord2d c, final List<Long> links) {
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
