package hamster.script.pathfinding.waypoint;

import java.util.Objects;

public class Node implements Comparable<Node> {
    //Actual waypoint of this node
    public final Waypoint wp;
    //Our current length g(n)
    public final double g;
    //Our Heuristic value h(n)
    public final double h;
    //Computed length f(n) = g(n) + h(n)
    public final double f;
    //Parent node
    public final Node parent;

    Node(final Node parent, final Waypoint wp, final double g, final double h) {
	this.parent = parent;
	this.wp = wp;
	this.g = g;
	this.h = h;
	this.f = g + h;
    }

    @Override
    public int hashCode() {
	return Objects.hash(wp);
    }

    @Override
    public boolean equals(Object obj) {
	return obj instanceof Node && ((Node) obj).wp.equals(wp);
    }

    @Override
    public int compareTo(Node o) {
	//for tie breaking look at h(n)
	return f != o.f ? Double.compare(f, o.f) : Double.compare(h, o.h);
    }
}
