package hamster.gob.attrs.info;

import haven.*;
import haven.render.Homo3D;
import haven.render.Pipe;

/**
 * This is an information Attrib for getting a Gob's `sc` which is no longer stored in `Gob`.
 * This allows  us to only need it for Gobs that require it
 */
public class ScreenLocation extends GAttrib implements PView.Render2D {
    private Coord sc;

    public ScreenLocation(final Gob g) {
        super(g);
    }

    @Override
    public void draw(GOut g, Pipe state) {
         sc = Homo3D.obj2view(new Coord3f(0, 0, 0), state).round2();
    }

    public Coord sc() {
        return sc;
    }
}
