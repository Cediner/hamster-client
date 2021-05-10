/* Preprocessed source code */
package haven.res.lib.vmat;

import haven.*;
import haven.render.*;
import java.util.*;

public class Materials extends Mapping {
    public static final Map<Integer, Material> empty = Collections.emptyMap();
    public final Map<Integer, Material> mats;

    public static Map<Integer, Material> decode(Resource.Resolver rr, Message sdt) {
	Map<Integer, Material> ret = new IntMap<>();
	int idx = 0;
	while(!sdt.eom()) {
	    Indir<Resource> mres = rr.getres(sdt.uint16());
	    int mid = sdt.int8();
	    Material.Res mat;
	    if(mid >= 0)
		mat = mres.get().layer(Material.Res.class, mid);
	    else
		mat = mres.get().layer(Material.Res.class);
	    ret.put(idx++, mat.get());
	}
	return(ret);
    }

    private static final Collection<Pair<TexRender.TexDraw, TexRender.TexClip>> warned = new HashSet<>();
    public static Material stdmerge(Material orig, Material var) {
	Pipe.Op fixup = null;
	{
	    /* XXX */
	    Pipe op = new BufPipe(), vp = new BufPipe();
	    orig.states.apply(op);
	    var.states.apply(vp);
	    TexRender.TexClip oc = op.get(TexRender.TexClip.slot);
	    TexRender.TexDraw vd = vp.get(TexRender.TexDraw.slot);
	    TexRender.TexClip vc = vp.get(TexRender.TexClip.slot);
	    if((oc != null) && (vd != null) && (vc == null) && (oc.tex != vd.tex)) {
		if(warned.add(new Pair<>(vd, oc)))
		    Warning.warn("varmat texture mismatch: draw %s vs clip %s", vd, oc);
		fixup = vd.tex.clip;
	    }
	}
	return(new Material(Pipe.Op.compose(orig.states, var.states, fixup),
			    Pipe.Op.compose(orig.dynstates, var.dynstates)));
    }

    public Material mergemat(Material orig, int mid) {
	if(!mats.containsKey(mid))
	    return(orig);
	Material var = mats.get(mid);
	return(stdmerge(orig, var));
    }

    public Materials(Map<Integer, Material> mats) {
	this.mats = mats;
    }

    public Materials(Gob gob, Message dat) {
	this.mats = decode(gob.context(Resource.Resolver.class), dat);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        for(final var matid : mats.keySet()) {
            final var mat = mats.get(matid);
            sb.append(matid);
            sb.append("=");
            sb.append(mat);
            sb.append('\n');
        }
        return sb.toString();
    }
}

