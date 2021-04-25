package hamster.script.pathfinding;

import haven.*;

public class Move {
    private static final Coord fake = new Coord(1, 1);
    private final Coord2d dest;

    public Move(final Coord2d c) {
        dest = c;
    }

    public void apply(MapView mv) {
        mv.wdgmsg("click", fake, dest.floor(OCache.posres), 1, 0);
    }

    public Coord2d dest() {
        return dest;
    }

    public static class ClickInfo {
        final int olclicked;
        final Gob gob;
        final Coord2d tc;
        final long gobid;
        final int olid;
        final int meshid;

        /**
         * inf comes in as:
         * { 0, gob, 0, -1 } if no ol or fastmesh clicked
         * { 1, gob, ol.id, -1 } if ol clicked
         * { 0, gob, 0, FastMesh.id } if fastmesh clicked
         * { 1, gob, ol.id, FastMesh.id } if ol clicked and fastmesh clicked
         */
        public ClickInfo(final Object[] inf) {
            olclicked = (Integer) inf[0];
            gob = (Gob) inf[1];
            gobid = gob.id;
            olid = (Integer) inf[2];
            meshid = (Integer) inf[3];
            tc = null;
        }

        public ClickInfo(final int olclicked, final long gobid, final Coord2d tc, final int olid, final int meshid) {
            this.olclicked = olclicked;
            this.gob = null;
            this.gobid = gobid;
            this.tc = tc;
            this.olid = olid;
            this.meshid = meshid;
        }

        public int id() {
            return (int) gobid;
        }

        public Coord2d rc() {
            if(gob != null)
                return gob.rc;
            else
                return tc;
        }
    }

    public static class Interact extends Move {
        final ClickInfo inf;
        final int cb, flags;

        public Interact(final ClickInfo inf, int clickb, int flags) {
            super(inf.rc());
            this.inf = inf;
            this.flags = flags;
            cb = clickb;
        }

        public void apply(MapView mv) {
            if (inf.olclicked == 0) {
                mv.wdgmsg("click", fake, inf.rc().floor(OCache.posres), cb, flags, 0, inf.id(), inf.rc().floor(OCache.posres), 0, inf.meshid);
            } else {
                mv.wdgmsg("click", fake, inf.rc().floor(OCache.posres), cb, flags, 1, inf.id(), inf.rc().floor(OCache.posres), inf.olid, inf.meshid);
            }

        }
    }

    public static class Repath extends Move {
        private final Coord2d goal;
        private final Gob g;

        public Repath(final Coord2d c, final Coord2d goal, final Gob g) {
            super(c);
            this.goal = goal;
            this.g = g;
        }

        public void apply(MapView mv) {
            if (g != null)
                mv.pathto(g);
            else
                mv.pathto(goal);
        }
    }
}
