/* Preprocessed source code */
import hamster.ui.DowseWnd;
import haven.*;
import haven.render.*;

import java.awt.*;
import java.nio.*;
import static java.lang.Math.*;

/* >spr: DowseFx */
/**
 * Spawns a window stating the angles, closing the window will close this.
 * You'll be able to customize the color in the window
 * <p>
 * TODO: As the owner gob moves around a1, a2 should update
 * On constructor once we have a1, a2 get our coordinate
 * multiple this coordinate out by 10000? tiles in the direction of each angle to get c1, c2
 * Anytime the owner gob moves update a1, a2 by getting the angles from new coordinate to c1, c2.
 * - not worth the time right now due to the coordinate systems being different..
 */
@SuppressWarnings("unused") //Dynamically created by resource
public class DowseFx extends Sprite {
    static final VertexArray.Layout fmt =
	    new VertexArray.Layout(new VertexArray.Layout.Input(Homo3D.vertex, new VectorFormat(3, NumberFormat.FLOAT32), 0, 0, 16),
		    new VertexArray.Layout.Input(VertexColor.color, new VectorFormat(4, NumberFormat.UNORM8), 0, 12, 16));
    public static final double ln = 2, r = 100;
    public final double a1, a2;  //Arc is a1 to a2, a1 < a2
    private Coord2d targeta1, targeta2;
    private final Coord2d startc;
    private final Model d2;

    // Persistent tracking based off the DowseWnd
    private byte[] color = Utils.c2ba(new Color(255, 0, 0, 128));
    private int dist = 500;
    private boolean delete = false;
    private boolean upd;

    public DowseFx(Owner owner, Resource res, Message sdt) {
	super(owner, res);
	if(sdt.eom()) {
	    a1 = -PI / 8;
	    a2 = PI / 8;
	} else {
	    double a2 = -(sdt.uint8() / 256.0) * PI * 2;
	    double a1 = -(sdt.uint8() / 256.0) * PI * 2;
	    while(a2 < a1)
		a2 += PI * 2;
	    this.a1 = a1;
	    this.a2 = a2;
	}
	if (owner instanceof Gob) {
	    final Gob g = (Gob) owner;
	    startc = g.rc;
	    targeta1 = startc.add(new Coord2d(cos(a1), sin(a1)).mul(dist));
	    targeta2 = startc.add(new Coord2d(cos(a2), sin(a2)).mul(dist));
	    final UI ui = g.glob.ui.get();
	    if (ui != null) {
		ui.gui.makeDowseWnd(new DowseWnd(g.rc, a1, a2, dist, this::update, this::updatedist, this::delete));
	    }
	} else {
	    //Won't realistically happen currently, but just in case.
	    targeta1 = targeta2 = startc = null;
	}
	d2 = new Model(Model.Mode.TRIANGLE_FAN, new VertexArray(fmt, new VertexArray.Buffer(v2(), DataBuffer.Usage.STREAM)), null);
	upd = true;
    }

    private void updatedist(final int dist) {
	this.dist = dist;
	targeta1 = startc.add(new Coord2d(cos(a1), sin(a1)).mul(dist));
	targeta2 = startc.add(new Coord2d(cos(a2), sin(a2)).mul(dist));
    }

    private void update(final Color state) {
        this.color = Utils.c2ba(state);
        this.upd = true;
    }

    private void delete() {
	this.delete = true;
    }

    // This renders the actual tracking slice which is what we care about
    private ByteBuffer v2() {
	ByteBuffer buf = ByteBuffer.allocate(128);
	buf.order(ByteOrder.nativeOrder());
	buf.putFloat(0).putFloat(0).putFloat(0);
	buf.put(color);
	for(double ca = a1; ca < a2; ca += PI * 0x0.04p0) {
	    buf = Utils.growbuf(buf, 16);
	    buf.putFloat((float)(cos(ca) * r)).putFloat((float)(sin(ca) * r)).putFloat(15);
	    buf.put(color);
	}
	buf = Utils.growbuf(buf, 16);
	buf.putFloat((float)(cos(a2) * r)).putFloat((float)(sin(a2) * r)).putFloat(15);
	buf.put(color);
	buf.flip();
	return(buf);
    }

    public void added(RenderTree.Slot slot) {
	slot.ostate(Pipe.Op.compose(VertexColor.instance, States.maskdepth, Location.goback("gobx"),
		/* Rendered.eyesort XXXRENDER */ Rendered.postpfx));
	slot.add(d2);
    }

    public void gtick(Render g) {
        if(upd) {
	    g.update(d2.va.bufs[0], DataBuffer.Filler.of(v2()));
	    upd = false;
	}
    }

    public boolean tick(double dt) {
	return delete; // Don't delete until told to
    }
}
