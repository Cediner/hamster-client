package hamster.gob.sprites;

import haven.*;
import haven.render.*;

import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.Optional;

public class GobPathSprite extends Sprite {
    public static final int id = -45141951;
    private final VertexBuf.VertexData posa;
    private final VertexBuf vbuf;
    private final Model model;
    private final State col;
    private boolean alive = true;
    public Coord2d dest;
    private Coord2d lc;

    /**
     * @param dest Destination coordinate, for knowing if we changed
     * @param rc   Current coordinate, for knowing if we changed
     * @param col  The color of this sprite
     */
    public GobPathSprite(final Gob owner, final Coord2d dest, final Coord2d rc, final BaseColor col) {
        super(owner, null);
        this.dest = dest;
        this.col = col;

        final float dist = (float)rc.dist(dest);
        final FloatBuffer posb = Utils.wfbuf(2 * 3);
        final FloatBuffer nrmb = Utils.wfbuf(2 * 3);
        float s = (float)Math.sin(owner.a);
        float c = (float)Math.cos(owner.a);

        posb.put(0).put(0).put(11f);
        nrmb.put(0).put(0).put(0);
        posb.put(c*dist).put(s*-dist).put(0);
        nrmb.put(c*dist).put(s*-dist).put(0);

        this.posa = new VertexBuf.VertexData(posb);
        VertexBuf.NormalData nrma = new VertexBuf.NormalData(nrmb);
        this.vbuf = new VertexBuf(posa, nrma);
        this.model = new Model(Model.Mode.LINE_STRIP, vbuf.data(),
                new Model.Indices(2, NumberFormat.UINT16, DataBuffer.Usage.STATIC, this::sidx));
    }

    private FillBuffer sidx(final Model.Indices dst,  final Environment env) {
        FillBuffer ret = env.fillbuf(dst);
        ShortBuffer buf = ret.push().asShortBuffer();
        buf.put((short)0).put((short)1);
        return(ret);
    }

    private void update(final Render g, final Glob glob, final Coord2d cc,
                        final double dist, final double a) {
        FloatBuffer posb = posa.data;
        try {
            final float dx = (float)(dist * Math.cos(a));
            final float dy = -(float)(dist * Math.sin(a));
            final float bz = (float)(glob.map.getcz(cc.x + dx, cc.y - dy) - glob.map.getcz(cc.x, cc.y));
            posb.put(2, 11f);
            posb.put(3, dx).put(4, dy).put(5, bz + 11f);
        } catch(Loading e) {
            return;
        }
        vbuf.update(g);
    }

    @Override
    public void gtick(Render g) {
        super.gtick(g);
        final Coord2d cc = ((Gob)owner).rc;
        final Moving mv = ((Gob)owner).getattr(Moving.class);
        if(mv == null) {
            alive = false;
        } else {
            final Optional<Coord2d> ndest = mv.getDest();
            if ((lc == null) || !lc.equals(cc) || (ndest.isPresent() && !ndest.get().equals(dest))) {
                dest = ndest.orElseGet(() -> dest);
                update(g, owner.context(Glob.class), cc, cc.dist(dest), ((Gob)owner).a);
                lc = cc;
            }
        }
    }

    @Override
    public void added(RenderTree.Slot slot) {
        super.added(slot);
        slot.ostate(Pipe.Op.compose(Rendered.postpfx,
                new States.Facecull(States.Facecull.Mode.NONE),
                Location.goback("gobx")));
        final UI ui = ((Gob)owner).glob.ui.get();
        slot.add(model, Pipe.Op.compose(col, new States.LineWidth(ui != null ? ui.gui.settings.PATHWIDTH.get() : 4)));
    }

    @Override
    public boolean tick(double dt) {
        final Gob g = (Gob)owner;
        final UI ui = g.glob.ui.get();
        return !alive || (ui != null && ui.gui != null && !ui.gui.settings.SHOWGOBPATH.get());
    }
}
