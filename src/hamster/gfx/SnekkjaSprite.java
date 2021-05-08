package hamster.gfx;

import haven.*;
import haven.render.Render;
import haven.render.RenderTree;
import haven.res.lib.vmat.VarSprite;

import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.function.Consumer;

public class SnekkjaSprite extends VarSprite implements SailSprite {
    static final FastMesh proj = Resource.remote().loadwait("gfx/terobjs/vehicle/snekkja").layer(FastMesh.MeshRes.class, 16).m;
    static final Material base = Resource.remote().loadwait("gfx/terobjs/vehicle/snekkja").layer(Material.Res.class, 16).get();

    public SnekkjaSprite(final Owner owner, final Resource res, final Message sdt) {
        super(owner, res, sdt);
    }

    //This is the sail material
    Material sym = null;
    Resource res = null;

    public void iparts(int mask, Collection<RenderTree.Node> rbuf, Collection<Runnable> tbuf, Collection<Consumer<Render>> gbuf) {
	super.iparts(mask, rbuf, tbuf, gbuf);
	if(sym != null)
	    rbuf.add(animwrap(sym.apply(proj), tbuf, gbuf));
    }

    public void update(Message sdt) {
	if(!sdt.eom() && (sdt.uint8() == 1)) {
	    long id = sdt.int64();
	    final var tex = (res = Resource.remote().dynres(id).get()).layer(TexR.class).tex();
	    sym = new Material(base, tex.draw, tex.clip);
	} else {
	    sym = null;
	}
	super.update(sdt);
    }

    public BufferedImage sail() {
        return res != null ? res.layer(TexR.class).back() : null;
    }

    @Override
    public String toString() {
	return "Snekkja(" +
		"sym=" + sym +
		')';
    }
}
