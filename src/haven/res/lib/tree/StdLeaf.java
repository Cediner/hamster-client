/* Preprocessed source code */
/* $use: lib/globfx */
/* $use: lib/leaves */
/* $use: lib/svaj */

package haven.res.lib.tree;

import haven.*;
import haven.res.lib.leaves.*;

public class StdLeaf extends FallingLeaves.Leaf {
    public final Material m;

    public StdLeaf(FallingLeaves fx, Coord3f c, Material m) {
	fx.super(c);
	this.m = m;
    }

    public Material mat() {return(m);}
}

