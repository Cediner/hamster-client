/* Preprocessed source code */
/* $use: lib/vmat */

package haven.res.lib.dynspr;

import haven.*;
import haven.render.*;
import java.util.*;
import java.util.function.Consumer;

/* >spr: Dyntex */
public class Dyntex implements Sprite.Factory {
    public final int matid;
    public final int meshid;
    public final boolean pflag;

    public Dyntex(int matid, int meshid, boolean pflag) {
	this.matid = matid;
	this.meshid = meshid;
	this.pflag = pflag;
    }

    public Dyntex() {
	this(16, 0, true);
    }

    public Sprite create(Sprite.Owner owner, Resource res, Message sdt) {
	Material base = res.layer(Material.Res.class, matid).get();
	FastMesh proj = res.layer(FastMesh.MeshRes.class, meshid).m;
	Pipe.Op.Wrapping banner;
	if(!sdt.eom() && (!pflag || (sdt.uint8() == 1))) {
	    long id = sdt.int64();
	    TexRender tex = res.pool.dynres(id).get().layer(TexR.class).tex();
	    Material sym = new Material(base, tex.draw, tex.clip);
	    banner = sym.apply(proj);
	} else {
	    banner = null;
	}
	return(new haven.res.lib.vmat.VarSprite(owner, res, Message.nil) {
	    public void iparts(int mask, Collection<RenderTree.Node> rbuf, Collection<Runnable> tbuf, Collection<Consumer<Render>> gbuf) {
		super.iparts(mask, rbuf, tbuf, gbuf);
		if(banner != null)
		    rbuf.add(animwrap(banner, tbuf, gbuf));
	    }
	});
    }
}
