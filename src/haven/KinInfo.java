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

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.awt.Color;

import hamster.GlobalSettings;
import hamster.gob.Tag;
import hamster.gob.sprites.FriendlyMark;
import haven.render.*;

public class KinInfo extends GAttrib implements RenderTree.Node, PView.Render2D {
    public static final BufferedImage vlg = Resource.loadimg("gfx/hud/vilind");
    public static final Text.Foundry nfnd = new Text.Foundry(Text.dfont, 10);
    public String name;
    public int group, type;
    public double seen = 0;
    private Tex rnm = null;
    
    public KinInfo(Gob g, String name, int group, int type) {
	super(g);
	this.name = name;
	this.group = group;
	this.type = type;
    }

    private void updateMark() {
	if(gob.hasTag(Tag.HUMAN) && GlobalSettings.CIRCLEFRIENDS.get()) {
	    final var mark = gob.findol(FriendlyMark.id);
	    if(group == GlobalSettings.BADKIN.get()) {
		if(mark != null)
		    ((FriendlyMark)mark.spr).rem();
	    } else {
		if(mark != null)
		    ((FriendlyMark)mark.spr).update(BuddyWnd.gc[group]);
		else
		    gob.daddol(FriendlyMark.id, new FriendlyMark(gob, BuddyWnd.gc[group]));
	    }
	}
    }

    @Override
    public String toString() {
	return "KinInfo(" +
		"name='" + name + '\'' +
		", group=" + group +
		", type=" + type +
		", seen=" + seen +
		')';
    }

    @Override
    public void ctick(double dt) {
	super.ctick(dt);
	updateMark();
    }

    public void update(String name, int group, int type) {
	this.name = name;
	this.group = group;
	this.type = type;
	rnm = null;
	updateMark();
    }

    public boolean isVillager() {
	return (type & 2) != 0;
    }
    
    public Tex rendered() {
	if(rnm == null) {
	    boolean hv = (type & 2) != 0;
	    BufferedImage nm = null;
	    if(name.length() > 0)
		nm = Utils.outline2(nfnd.render(name, BuddyWnd.gc[group]).img, Utils.contrast(BuddyWnd.gc[group]));
	    int w = 0, h = 0;
	    if(nm != null) {
		w += nm.getWidth();
		if(nm.getHeight() > h)
		    h = nm.getHeight();
	    }
	    if(hv) {
		w += vlg.getWidth() + 1;
		if(vlg.getHeight() > h)
		    h = vlg.getHeight();
	    }
	    if(w == 0) {
		rnm = new TexI(TexI.mkbuf(new Coord(1, 1)));
	    } else {
		BufferedImage buf = TexI.mkbuf(new Coord(w, h));
		Graphics g = buf.getGraphics();
		int x = 0;
		if(hv) {
		    g.drawImage(vlg, x, (h / 2) - (vlg.getHeight() / 2), null);
		    x += vlg.getWidth() + 1;
		}
		if(nm != null) {
		    g.drawImage(nm, x, (h / 2) - (nm.getHeight() / 2), null);
		    x += nm.getWidth();
		}
		g.dispose();
		rnm = new TexI(buf);
	    }
	}
	return(rnm);
    }
    
    public void draw(GOut g, Pipe state) {
	Coord sc = Homo3D.obj2view(new Coord3f(0, 0, 15), state, Area.sized(g.sz())).round2();
	if(sc.isect(Coord.z, g.sz())) {
	    double now = Utils.rtime();
	    if(seen == 0)
		seen = now;
	    double tm = now - seen;
	    Color show = null;
	    boolean auto = (type & 1) == 0;
	    if(false) {
		/* XXX: QQ, RIP in peace until constant
		 * mouse-over checks can be had. */
		if(auto && (tm < 7.5)) {
		    show = Utils.clipcol(255, 255, 255, (int)(255 - ((255 * tm) / 7.5)));
		}
	    } else {
		show = Color.WHITE;
	    }
	    if(show != null) {
		Tex t = rendered();
		if(t != null) {
		    g.chcolor(show);
		    g.aimage(t, sc, 0.5, 1.0);
		    g.chcolor();
		}
	    }
	} else {
	    seen = 0;
	}
    }
}
