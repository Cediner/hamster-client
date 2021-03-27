package hamster.gob.attrs.draw2d;

import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;
import haven.render.RenderTree;

/**
 * This is for placing objects only. Puts a locked icon over them when you lock it to move around
 */
public class Locked extends GAttrib implements RenderTree.Node, PView.Render2D {
    public static final Tex lockt = Resource.loadtex("custom/inv/locked");

    public Locked(final Gob g) {
	super(g);
    }

    public void draw(GOut g, Pipe state) {
	Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 5), state, Area.sized(g.sz())).round2();
	if (sc.isect(Coord.z, g.sz())) {
	    g.aimage(lockt, sc, 0.5, 0.5);
	}
    }
}
