package hamster.gob.attrs.draw2d;

import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;
import haven.render.RenderTree;

/**
 * This is for drawing our speed above HUMAN and non-tamed ANIMAL gobs
 */
public class Speed extends GAttrib implements RenderTree.Node, PView.Render2D {
    private Tex speed;
    private double lspeed;

    public Speed(final Gob g) {
        super(g);
    }

    public void draw(GOut g, Pipe state) {
        if (speed != null) {
            Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 15), state, Area.sized(g.sz())).round2();
            if (sc.isect(Coord.z, g.sz())) {
                g.aimage(speed, sc, 0.5, 2.0);
            }
        }
    }

    @Override
    public void ctick(double dt) {
        final double spd = gob.getv();
        if (spd != lspeed || speed == null) {
            speed = Text.renderstroked(String.format("%.2f", spd)).tex();
            lspeed = spd;
        }
    }

    @Override
    public void dispose() {
        if(speed  != null)
            speed.dispose();
    }
}
