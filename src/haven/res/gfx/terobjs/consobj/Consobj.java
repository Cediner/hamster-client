/* Preprocessed source code */
package haven.res.gfx.terobjs.consobj;

import haven.*;
import haven.render.*;
import static haven.MCache.tilesz;

/* >spr: Consobj */
public class Consobj extends Sprite implements Sprite.CUpd {
    public final static Indir<Resource> signres = Resource.remote().load("gfx/terobjs/sign", 6);
    public final static Indir<Resource> poleres = Resource.remote().load("gfx/terobjs/arch/conspole", 2);
    private static Material bmat = null;
    public final static float bscale = 1f / 11;
    public final Coord ul, br;
    public final ResData built;
    public float done;
    final Coord3f cc;
    final Sprite sign, pole;
    final Location[] poles;
    final MCache map;
    final RenderTree.Node bound;

    Coord3f gnd(float rx, float ry) {
	double a = -((Gob)owner).a;
	float s = (float)Math.sin(a), c = (float)Math.cos(a);
	float gx = rx * c + ry * s, gy = ry * c - rx * s;
	return(new Coord3f(rx, -ry, map.getcz(gx + cc.x, gy + cc.y) - cc.z));
    }

    public Consobj(Owner owner, Resource res, Message sdt) {
	super(owner, res);
	this.map = owner.context(Glob.class).map;
	if(bmat == null)
	    bmat = res.layer(Material.Res.class).get();
	ul = new Coord(sdt.int8(), sdt.int8());
	br = new Coord(sdt.int8(), sdt.int8());
	done = sdt.uint8() / 255.0f;
	if(!sdt.eom()) {
	    int resid = sdt.uint16();
	    built = new ResData(owner.context(Resource.Resolver.class).getres(resid), new MessageBuf(sdt.bytes()));
	} else {
	    built = null;
	}
	sign = Sprite.create(owner, signres.get(), Message.nil);
	pole = Sprite.create(owner, poleres.get(), Message.nil);
	if(owner instanceof Gob) {
	    final var gob = (Gob)owner;
	    this.cc = gob.getrc();
	    gob.updateHitbox(new Coord2d(ul), new Coord2d(br.sub(ul)));
	} else {
	    this.cc = Coord3f.o;
	}
	poles = new Location[] {
		Location.xlate(gnd(ul.x, ul.y)),
		Location.xlate(gnd(br.x, ul.y)),
		Location.xlate(gnd(br.x, br.y)),
		Location.xlate(gnd(ul.x, br.y)),
	};
	if(((br.x - ul.x) > 22) || ((br.y - ul.y) > 22))
	    bound = mkbound();
	else
	    bound = null;
    }

    void trace(MeshBuf buf, float x1, float y1, float x2, float y2) {
	float dx = x2 - x1, dy = y2 - y1, ed = (float)Math.sqrt(dx * dx + dy * dy);
	float lx = x1, ly = y1;
	Coord3f nrm = new Coord3f(dy / ed, dx / ed, 0);
	MeshBuf.Tex tex = buf.layer(MeshBuf.tex);
	MeshBuf.Vertex ll = buf.new Vertex(gnd(lx, ly), nrm);
	MeshBuf.Vertex lh = buf.new Vertex(gnd(lx, ly).add(0, 0, 3), nrm);
	tex.set(ll, new Coord3f(0, 1, 0));
	tex.set(lh, new Coord3f(0, 0, 0));
	int lim = 0;
	while(true) {
	    boolean end = true;
	    float ma = 1.0f, a;
	    float nx = x2, ny = y2;
	    if(dx != 0) {
		float ex;
		if(dx > 0) {
		    a = ((ex = (float)((Math.floor(lx / tilesz.x) + 1) * tilesz.x)) - x1) / dx;
		} else {
		    a = ((ex = (float)((Math.ceil (lx / tilesz.x) - 1) * tilesz.x)) - x1) / dx;
		}
		if(a < ma) {
		    nx = ex; ny = y1 + dy * a;
		    ma = a;
		    end = false;
		}
	    }
	    if(dy != 0) {
		float ey;
		if(dy > 0)
		    a = ((ey = (float)((Math.floor(ly / tilesz.y) + 1) * tilesz.y)) - y1) / dy;
		else
		    a = ((ey = (float)((Math.ceil (ly / tilesz.y) - 1) * tilesz.y)) - y1) / dy;
		if(a < ma) {
		    nx = x1 + dx * a; ny = ey;
		    ma = a;
		    end = false;
		}
	    }
	    MeshBuf.Vertex nl = buf.new Vertex(gnd(nx, ny), nrm);
	    MeshBuf.Vertex nh = buf.new Vertex(gnd(nx, ny).add(0, 0, 3), nrm);
	    tex.set(nl, new Coord3f(ma * ed * bscale, 1, 0));
	    tex.set(nh, new Coord3f(ma * ed * bscale, 0, 0));
	    buf.new Face(lh, ll, nh); buf.new Face(ll, nl, nh);
	    ll = nl; lh = nh;
	    lx = nx; ly = ny;
	    if(end)
		return;
	    if(lim++ > 100)
		throw(new RuntimeException("stuck in trace"));
	}
    }

    RenderTree.Node mkbound() {
	MeshBuf buf = new MeshBuf();
	trace(buf, ul.x, ul.y, ul.x, br.y);
	trace(buf, ul.x, br.y, br.x, br.y);
	trace(buf, br.x, br.y, br.x, ul.y);
	trace(buf, br.x, ul.y, ul.x, ul.y);
	FastMesh mesh = buf.mkmesh();
	return(bmat.apply(mesh));
    }

    public void added(RenderTree.Slot slot) {
	slot.add(sign);
	if(bound != null) {
	    slot.add(bound);
	    for(Location loc : poles)
		slot.add(pole, loc);
	}
    }

    public void update(Message sdt) {
	for(int i = 0; i < 4; i++)
	    sdt.int8();
	done = sdt.uint8() / 255.0f;
    }
}
