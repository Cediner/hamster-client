/* Preprocessed source code */
package haven.res.lib.vmat;

import haven.*;
import haven.render.*;
import java.util.*;

public abstract class Mapping extends Gob.ResAttr {
    public abstract Material mergemat(Material orig, int mid);

    public RenderTree.Node[] apply(Resource res) {
	Collection<RenderTree.Node> rl = new LinkedList<>();
	for(FastMesh.MeshRes mr : res.layers(FastMesh.MeshRes.class)) {
	    String sid = mr.rdat.get("vm");
	    int mid = (sid == null)?-1:Integer.parseInt(sid);
	    if(mid >= 0) {
		rl.add(mergemat(mr.mat.get(), mid).apply(mr.m));
	    } else if(mr.mat != null) {
		rl.add(mr.mat.get().apply(mr.m));
	    }
	}
	return(rl.toArray(new RenderTree.Node[0]));
    }

    public final static Mapping empty = new Mapping() {
	    public Material mergemat(Material orig, int mid) {
		return(orig);
	    }
	};
}

/* >gattr: haven.res.lib.vmat.Materials */
