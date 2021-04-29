package hamster.script.pathfinding.waypoint;

import hamster.script.pathfinding.Move;

import java.util.*;

/**
 * Pathfinder specific to Waypoints
 * This is based off NBA method and is very slimmed down
 * as we don't have all the same constraints normal pathfinding does
 */
public class WaypointPathfinder {
    @FunctionalInterface
    interface HeuristicFun {
	double distance(final Waypoint c, final Waypoint goal);
    }

    // Our Waypoint mapping for the segment we're in
    private final WaypointMap map;
    // Our start and goal waypoints
    private final Waypoint start;
    private final Waypoint goal;

    // NBA Properties
    private double best = Double.MAX_VALUE;
    private Waypoint touched = null;
    private final HeuristicFun heuristic;
    private static final double D = 1;

    public WaypointPathfinder(final WaypointMap map, final Waypoint start, final Waypoint goal) {
        this.map = map;
        this.start = start;
        this.goal = goal;
        this.heuristic = this::manhattanDistance;
    }

    private double manhattanDistance(final Waypoint cur, final Waypoint goal) {
        final double dx = Math.abs(cur.c.x - goal.c.x);
        final double dy = Math.abs(cur.c.y - goal.c.y);
        return D * (dx + dy);
    }

    /**
     * This executes from both sides simultaneously
     * s is the source for this side
     * t is the destination for this side
     * <p>
     * 1: S = ∅;
     * 2: R = ∅; // S and R are sets
     * 3: M = V ; // M is a shared set of all verts in our map
     * 4: L = ∞; // L is a shared real value
     * 5: for all v ∈ V do
     * 6:   g(v) = ∞;
     * 7: end for
     * 8: g(s) = 0;        //First g(s) is 0
     * 9: f = g(s) + h(s); //First f is g(s) + h(s) -> distance to t
     * 10: while any v ∈ M has g(v) < ∞ do //While we have verts that have g(v) defined
     * 11:   u0 = arg min{g(v) + h(v) | v ∈ M}; // u0 is selected by taking the min vert with g(v) + h(v)
     * 12:   M = M − {u0}; //remove it from the set
     * 13:   if g(u0) + h(u0) − h(t) ≥ L or g(u0) + ˜f − h˜(u0) ≥ L then
     * 14:     R = R + {u0}; // u0 is rejected if
     * 15:   else
     * 16:     S = S + {u0}; // u0 is stabilized, create children
     * 17:     for all edges (u0, v) ∈ E with v ∈ M do           //for each child
     * 18:       g(v) = min(g(v), g(u0) + d(u0, v));              //define g(v)
     * 19:       L = min(L, g(v) + ˜g(v)); //Set L to min of L or hypothetically the distance from start -> v + v -> target
     * 20:     end for
     * 21:   end if
     * 22:   f = min{g(v) + h(v) | v ∈ M};
     * 23: end while
     */
    private List<Waypoint> findpath() {
        final PriorityQueue<Node> startpq = new PriorityQueue<>();
        final PriorityQueue<Node> goalpq = new PriorityQueue<>();
        final Map<Waypoint, Node> startNodes = new HashMap<>();
        final Map<Waypoint, Node> endNodes = new HashMap<>();
        final Set<Waypoint> ignore = new HashSet<>();
        double fs;
        double ft;

        {//Init
            /*
             * 8: g(s) = 0;        //First g(s) is 0
             * 9: f = g(s) + h(s); //First f is g(s) + h(s) -> distance to t
             */
            final Node stnode = new Node(null, start, 0, heuristic.distance(start, goal));
            final Node endnode = new Node(null, goal, 0, heuristic.distance(goal, start));
            startpq.add(stnode);
            startNodes.put(start, stnode);
            goalpq.add(endnode);
            endNodes.put(goal, endnode);
            fs = ft = heuristic.distance(start, goal);
        }

        //10: while any v ∈ M has g(v) < ∞ do //While we have verts that have g(v) defined
        while (!startpq.isEmpty() && (!goalpq.isEmpty())) {
            //Do it in lock step to simulate it being simultaneous.
            fs = expand(startpq, goal, start, fs, ft, startNodes, endNodes, ignore);
            ft = expand(goalpq, start, goal, ft, fs, endNodes, startNodes, ignore);

            if (touched == null)
                continue;
            break;
        }

        if (touched != null) {
            //start -> mid
            final List<Waypoint> fhalf = collect(startNodes.get(touched));
            //goal -> mid
            final List<Waypoint> lhalf = collect(endNodes.get(touched));
            lhalf.remove(lhalf.size() - 1);
            Collections.reverse(lhalf);
            final List<Waypoint> combined = new ArrayList<>(fhalf.size() + lhalf.size());
            combined.addAll(fhalf);
            combined.addAll(lhalf);
            return combined;
        } else {
            return null;
        }
    }

    private double expand(final PriorityQueue<Node> M, final Waypoint target, final Waypoint source,
                          final double f, final double ftilda,
                          final Map<Waypoint, Node> myNodes, final Map<Waypoint, Node> otherNodes,
                          final Set<Waypoint> rejected) {
        if (!M.isEmpty()) {
            //11:   u0 = arg min{g(v) + h(v) | v ∈ M}; // u0 is selected by taking the min vert with g(v) + h(v)
            final Node node = M.poll();
            //13:   if g(u0) + h(u0) − h(t) ≥ L or g(u0) + ˜f − h˜(u0) ≥ L then // - h(t) -> -0???
            //negate to only consider non-rejected nodes
            if (!(node.f >= best || (node.g + ftilda - heuristic.distance(node.wp, source)) >= best)) {
                //16:     S = S + {u0}; // u0 is stabilized, create children
                //17:     for all edges (u0, v) ∈ E with v ∈ M do
                // for each child
                for (final var wpid : source.links) {
                    final Waypoint nc = map.getWaypoint(wpid);
                    //with v ∈ M do
                    if (!rejected.contains(nc)) {
                        //12:   M = M − {u0}; //remove it from the set
                        rejected.add(nc);
                        //18:       g(v) = min(g(v), g(u0) + d(u0, v));
                        final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, target));
                        M.add(child);
                        myNodes.put(nc, child);
                    } else if (otherNodes.containsKey(nc)) { //19:       L = min(L, g(v) + ˜g(v));
                        final Node child = new Node(node, nc, node.g + 1, heuristic.distance(nc, target));
                        myNodes.put(nc, child);
                        if (child.g + otherNodes.get(nc).g < best) {
                            best = child.g + otherNodes.get(nc).g;
                            touched = nc;
                        }
                    }
                }
            }
        }

        return !M.isEmpty() ? M.peek().f : f;
    }


    final List<Waypoint> collect(final Node end) {
        final ArrayList<Waypoint> moves = new ArrayList<>();
        moves.add(end.wp);
        for (Node next = end.parent; next != null; next = next.parent) {
            moves.add(next.wp);
        }
        //reverse start -> finish
        Collections.reverse(moves);
        return moves;
    }
    
    public List<Move> path() {
        final var path = findpath();
        final var moves = new ArrayList<Move>();
        if(path != null) {
            for (final var wp : path) {
                moves.add(new WaypointMove(wp));
            }
        }
        return moves;
    }
}
