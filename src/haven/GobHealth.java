/*
 *  This file is part of the Haven & Hearth game client.
 *  Copyright (C) 2009 Fredrik Tolf <fredrik@dolda2000.com>, and
 *                     Björn Johannessen <johannessen.bjorn@gmail.com>
 *
 *  Redistribution and/or modification of this file is subject to the
 *  terms of the GNU Lesser General Public License, version 3, as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  Other parts of this source tree adhere to other copying
 *  rights. Please see the file `COPYING' in the root directory of the
 *  source tree for details.
 *
 *  A copy the GNU Lesser General Public License is distributed along
 *  with the source tree of which this file is a part in the file
 *  `doc/LPGL-3'. If it is missing for any reason, please see the Free
 *  Software Foundation's website at <http://www.fsf.org/>, or write
 *  to the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 *  Boston, MA 02111-1307 USA
 */

package haven;

import hamster.GlobalSettings;
import haven.render.*;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class GobHealth extends GAttrib implements Gob.SetupMod, RenderTree.Node, PView.Render2D {
    private static final Map<Float, Tex> gobhpmap = new HashMap<>();
    private static Tex gobhp(final float hp) {
        if(gobhpmap.containsKey(hp))
            return gobhpmap.get(hp);
        final var tex = Text.renderstroked(String.format("%d%%", (int)(hp*100f)), Color.WHITE, Color.BLACK, Text.std16).tex();
        gobhpmap.put(hp, tex);
        return tex;
    }

    public final float hp;
    public final MixColor fx;
    private final Tex hptex;

    public GobHealth(Gob g, float hp) {
	super(g);
	this.hp = hp;
	this.fx = new MixColor(255, 0, 0, 128 - Math.round(hp * 128));
    	this.hptex = gobhp(hp);
    }

    public void draw(GOut g, Pipe state) {
	final UI ui = gob.glob.ui.get();
	if(ui != null && ui.gui != null && GlobalSettings.SHOWGOBHP.get() && hp < 1) {
	    Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 5), state, Area.sized(g.sz())).round2();
	    if (sc.isect(Coord.z, g.sz())) {
		g.aimage(hptex, sc, 0.5, 1.0);
	    }
	}
    }
    
    public Pipe.Op gobstate() {
	if(hp >= 1)
	    return(null);
	return(fx);
    }
}
