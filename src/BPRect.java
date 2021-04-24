/* Preprocessed source code */
import hamster.GlobalSettings;
import haven.*;
import haven.render.*;
import haven.render.Model.Indices;
import java.util.*;
import java.nio.*;
import static haven.MCache.tilesz;

/* >spr: BPRect */
public class BPRect extends Sprite {
    static final Pipe.Op smat = new BaseColor(new java.awt.Color(192, 0, 0, 128));
    static final Pipe.Op emat = Pipe.Op.compose(new BaseColor(new java.awt.Color(255, 224, 96)), new States.LineWidth(4));
    final boolean tilefix;
    final VertexBuf.VertexData posa;
    final VertexBuf vbuf;
    final Model smod, emod;
    private final List<RenderTree.Slot> slots = new ArrayList<>(1);
    private Coord2d lc;

    public BPRect(Owner owner, Resource res, Message sdt) {
	super(owner, res);
	int fl = sdt.uint8();
	tilefix = (fl & 1) != 0;
	float l = Utils.hfdec((short)sdt.int16()) * 11;
	float u = Utils.hfdec((short)sdt.int16()) * 11;
	float r, b;
	if(sdt.eom()) {
	    r = -(l = -l / 2);
	    b = -(u = -u / 2);
	} else {
	    r = Utils.hfdec((short)sdt.int16()) * 11;
	    b = Utils.hfdec((short)sdt.int16()) * 11;
	}
	int xn = Math.max(2, 1 + (int)((r - l) / 11.0));
	int yn = Math.max(2, 1 + (int)((b - u) / 11.0));
	int hn = xn + yn, n = hn * 2;
	FloatBuffer posb = Utils.wfbuf(n * 3 * 2);
	FloatBuffer nrmb = Utils.wfbuf(n * 3 * 2);
	ShortBuffer sidx = Utils.wsbuf(n * 6);
	ShortBuffer eidx = Utils.wsbuf(n);
	int I, ii, v, N = n * 3;
	for(int i = 0; i < xn; i++) {
	    float x = l + ((r - l) * i) / (xn - 1);
	    I = i;
	    v = I * 3;
	    posb.put(v +     0, x).put(v +     1, -u).put(v +     2,  10);
	    posb.put(v + N + 0, x).put(v + N + 1, -u).put(v + N + 2, -10);
	    nrmb.put(v +     0, 0).put(v +     1,  1).put(v +     2,   0);
	    nrmb.put(v + N + 0, 0).put(v + N + 1,  1).put(v + N + 2,   0);
	    if(i < xn - 1) {
		ii = i * 6;
		sidx.put(ii + 0, (short)(I + 1)).put(ii + 1, (short)(I + 1 + n)).put(ii + 2, (short)I);
		sidx.put(ii + 3, (short)(I + 1 + n)).put(ii + 4, (short)(I + n)).put(ii + 5, (short)I);
	    }
	    I = i + hn;
	    v = I * 3;
	    posb.put(v +     0, x).put(v +     1, -b).put(v +     2,  10);
	    posb.put(v + N + 0, x).put(v + N + 1, -b).put(v + N + 2, -10);
	    nrmb.put(v +     0, 0).put(v +     1, -1).put(v +     2,   0);
	    nrmb.put(v + N + 0, 0).put(v + N + 1, -1).put(v + N + 2,   0);
	    if(i < xn - 1) {
		ii = (i + hn) * 6;
		sidx.put(ii + 0, (short)I).put(ii + 1, (short)(I + n)).put(ii + 2, (short)(I + 1));
		sidx.put(ii + 3, (short)(I + n)).put(ii + 4, (short)(I + 1 + n)).put(ii + 5, (short)(I + 1));
	    }
	}
	for(int i = 0; i < yn; i++) {
	    float y = u + ((b - u) * i) / (yn - 1);
	    I = i + xn;
	    v = I * 3;
	    posb.put(v +     0,  r).put(v +     1, -y).put(v +     2,  10);
	    posb.put(v + N + 0,  r).put(v + N + 1, -y).put(v + N + 2, -10);
	    nrmb.put(v +     0,  1).put(v +     1,  0).put(v +     2,   0);
	    nrmb.put(v + N + 0,  1).put(v + N + 1,  0).put(v + N + 2,   0);
	    if(i < yn - 1) {
		ii = (i + xn) * 6;
		sidx.put(ii + 0, (short)I).put(ii + 1, (short)(I + n)).put(ii + 2, (short)(I + 1));
		sidx.put(ii + 3, (short)(I + n)).put(ii + 4, (short)(I + 1 + n)).put(ii + 5, (short)(I + 1));
	    }
	    I = i + xn + hn;
	    v = I * 3;
	    posb.put(v +     0,  l).put(v +     1, -y).put(v +     2,  10);
	    posb.put(v + N + 0,  l).put(v + N + 1, -y).put(v + N + 2, -10);
	    nrmb.put(v +     0, -1).put(v +     1,  0).put(v +     2,   0);
	    nrmb.put(v + N + 0, -1).put(v + N + 1,  0).put(v + N + 2,   0);
	    if(i < yn - 1) {
		ii = (i + xn + hn) * 6;
		sidx.put(ii + 0, (short)(I + 1)).put(ii + 1, (short)(I + 1 + n)).put(ii + 2, (short)I);
		sidx.put(ii + 3, (short)(I + 1 + n)).put(ii + 4, (short)(I + n)).put(ii + 5, (short)I);
	    }
	}
	for(int i = 0; i < xn; i++) {
	    eidx.put(i, (short)i);
	    eidx.put(i + hn, (short)((xn - i - 1) + hn));
	}
	for(int i = 0; i < yn; i++) {
	    eidx.put(i + xn, (short)(i + xn));
	    eidx.put(i + hn + xn, (short)((yn - i - 1) + xn + hn));
	}
	VertexBuf.VertexData posa = new VertexBuf.VertexData(Utils.bufcp(posb));
	VertexBuf.NormalData nrma = new VertexBuf.NormalData(Utils.bufcp(nrmb));
	VertexBuf vbuf = new VertexBuf(posa, nrma);
	this.smod = new Model(Model.Mode.TRIANGLES, vbuf.data(), new Indices(n * 6, NumberFormat.UINT16, DataBuffer.Usage.STATIC, DataBuffer.Filler.of(sidx.array())));
	this.emod = new Model(Model.Mode.LINE_STRIP, vbuf.data(), new Indices(n + 1, NumberFormat.UINT16, DataBuffer.Usage.STATIC, DataBuffer.Filler.of(eidx.array())));
	this.posa = posa;
	this.vbuf = vbuf;
    }

    private void setz(Render g, Glob glob, Coord2d c) {
	FloatBuffer posb = posa.data;
	int n = posa.size() / 2;
	try {
	    float bz = !GlobalSettings.FLATWORLD.get() ? (float)glob.map.getcz(c) : 0;
	    pos = Location.xlate(new Coord3f((float)c.x, -(float)c.y, bz));
	    for(int i = 0; i < n; i++) {
		float z = !GlobalSettings.FLATWORLD.get() ? (float)glob.map.getcz(c.x + posb.get(i * 3), c.y - posb.get(i * 3 + 1)) - bz : 0;
		posb.put(i * 3 + 2, z + 10);
		posb.put((n + i) * 3 + 2, z - 10);
	    }
	} catch(Loading e) {}
	vbuf.update(g);
    }

    private Pipe.Op state() {
	// XXXRENDER rl.prepo(Rendered.eyesort);
	return(Pipe.Op.compose(Rendered.postpfx,
		new States.Facecull(States.Facecull.Mode.NONE),
		p -> p.put(Homo3D.loc, null),
		pos));
    }

    Location pos = null;
    public void gtick(Render g) {
	Coord2d cc = ((Gob)owner).rc;
	if(tilefix)
	    cc = cc.floor(tilesz).mul(tilesz).add(tilesz.div(2));
	if((lc == null) || !lc.equals(cc)) {
	    setz(g, owner.context(Glob.class), cc);
	    lc = cc;
	    for(RenderTree.Slot slot : slots)
		slot.ostate(state());
	}
    }

    public void added(RenderTree.Slot slot) {
	slot.ostate(state());
	slot.add(smod, smat);
	slot.add(emod, emat);
	slots.add(slot);
    }

    public void removed(RenderTree.Slot slot) {
	slots.remove(slot);
    }
}
