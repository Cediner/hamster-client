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

import hamster.script.SessionDetails;

import java.util.*;
import java.awt.image.WritableRaster;

public class Inventory extends Widget implements DTarget {
    public static final Coord sqsz = UI.scale(new Coord(33, 33));
    private SessionDetails.InventoryType type;
    public static final Tex invsq;
    public boolean dropul = true;
    public Coord isz;
    final Map<GItem, WItem> wmap = new HashMap<>();

    static {
	Coord sz = sqsz.add(1, 1);
	WritableRaster buf = PUtils.imgraster(sz);
	for(int i = 1, y = sz.y - 1; i < sz.x - 1; i++) {
	    buf.setSample(i, 0, 0, 20); buf.setSample(i, 0, 1, 28); buf.setSample(i, 0, 2, 21); buf.setSample(i, 0, 3, 167);
	    buf.setSample(i, y, 0, 20); buf.setSample(i, y, 1, 28); buf.setSample(i, y, 2, 21); buf.setSample(i, y, 3, 167);
	}
	for(int i = 1, x = sz.x - 1; i < sz.y - 1; i++) {
	    buf.setSample(0, i, 0, 20); buf.setSample(0, i, 1, 28); buf.setSample(0, i, 2, 21); buf.setSample(0, i, 3, 167);
	    buf.setSample(x, i, 0, 20); buf.setSample(x, i, 1, 28); buf.setSample(x, i, 2, 21); buf.setSample(x, i, 3, 167);
	}
	for(int y = 1; y < sz.y - 1; y++) {
	    for(int x = 1; x < sz.x - 1; x++) {
		buf.setSample(x, y, 0, 36); buf.setSample(x, y, 1, 52); buf.setSample(x, y, 2, 38); buf.setSample(x, y, 3, 125);
	    }
	}
	invsq = new TexI(PUtils.rasterimg(buf));
    }

    @RName("inv")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new Inventory((Coord)args[0]));
	}
    }

    public void draw(GOut g) {
	Coord c = new Coord();
	for(c.y = 0; c.y < isz.y; c.y++) {
	    for(c.x = 0; c.x < isz.x; c.x++) {
		g.image(invsq, c.mul(sqsz));
	    }
	}
	super.draw(g);
    }
	
    public Inventory(Coord sz) {
	super(sqsz.mul(sz).add(1, 1));
	isz = sz;
    }

    @Override
    protected void binded() {
	super.binded();
	if(parent instanceof Window) {
	    final Window par = (Window) parent;
	    if(par.cap != null) {
		switch (par.cap.text) {
		    case "Inventory" -> type = SessionDetails.InventoryType.MAIN;
		    case "Belt" -> type = SessionDetails.InventoryType.BELT;
		    default -> type = SessionDetails.InventoryType.SUPPLEMENTAL;
		}
	    } else {
		type = SessionDetails.InventoryType.SUPPLEMENTAL;
	    }
	} else {
	    //pretty sure this has to be the study
	    type = SessionDetails.InventoryType.STUDY;
	}

	ui.sess.details.attachInventory(this, type);
    }
    
    public boolean mousewheel(Coord c, int amount) {
	if(ui.modshift) {
	    Inventory minv = getparent(GameUI.class).maininv;
	    if(minv != this) {
		if(amount < 0)
		    wdgmsg("invxf", minv.wdgid(), 1);
		else if(amount > 0)
		    minv.wdgmsg("invxf", this.wdgid(), 1);
	    }
	}
	return(true);
    }
    
    public void addchild(Widget child, Object... args) {
	add(child);
	Coord c = (Coord)args[0];
	if(child instanceof GItem) {
	    GItem i = (GItem)child;
	    wmap.put(i, add(new WItem(i), c.mul(sqsz).add(1, 1)));
	}
    }
    
    public void cdestroy(Widget w) {
	super.cdestroy(w);
	if(w instanceof GItem) {
	    GItem i = (GItem)w;
	    ui.destroy(wmap.remove(i));
	}
    }
    
    public boolean drop(Coord cc, Coord ul) {
	Coord dc;
	if(dropul)
	    dc = ul.add(sqsz.div(2)).div(sqsz);
	else
	    dc = cc.div(sqsz);
	wdgmsg("drop", dc);
	return(true);
    }
	
    public boolean iteminteract(Coord cc, Coord ul) {
	return(false);
    }
	
    public void uimsg(String msg, Object... args) {
	if(msg.equals("sz")) {
	    isz = (Coord)args[0];
	    resize(invsq.sz().add(UI.scale(new Coord(-1, -1))).mul(isz).add(UI.scale(new Coord(1, 1))));
	} else if(msg.equals("mode")) {
	    dropul = (((Integer)args[0]) == 0);
	} else {
	    super.uimsg(msg, args);
	}
    }


    /** For the Scripting API  **********************************************************/
    public String name() {
	if(parent instanceof Window && ((Window) parent).cap != null) {
	    return ((Window) parent).cap.text;
	} else {
	    return "";
	}
    }

    @SuppressWarnings("unused")
    public int totalSlots() {
	return isz.x * isz.y;
    }

    @SuppressWarnings("unused")
    public int usedSlots() {
	int slots = 0;
	synchronized (wmap) {
	    for (final WItem wi : wmap.values()) {
		slots += wi.size();
	    }
	}
	return slots;
    }

    @SuppressWarnings("unused")
    public WItem[][] itemmap() {
	final WItem[][] map = new WItem[isz.x][isz.y];
	synchronized (wmap) {
	    for (WItem wi : wmap.values()) {
		Coord wc = wi.c.div(invsq.sz().sub(1, 1));
		Coord wsz = wi.sz.div(invsq.sz().sub(1, 1)).add(wc);
		for (int x = wc.x; x < wsz.x; ++x) {
		    if (x >= 0 && x < map.length) {
			for (int y = wc.y; y < wsz.y; ++y) {
			    if (y >= 0 && y < map[x].length) {
				map[x][y] = wi;
			    }
			}
		    }
		}
	    }
	}
	return map;
    }

    @SuppressWarnings("unused")
    public boolean canDropAt(final Coord c) {
	final Coord cc = c.mul(sqsz);
	if (cc.between(Coord.z, sz)) {
	    synchronized (wmap) {
		for (WItem wi : wmap.values()) {
		    if (cc.between(wi.c, wi.sz))
			return false;
		}
	    }
	    return true;
	} else {
	    return false;
	}
    }
    @SuppressWarnings("unused")
    public GItem itemAt(final Coord c) {
	final Coord cc = c.mul(sqsz);
	synchronized (wmap) {
	    for (WItem wi : wmap.values()) {
		if (cc.between(wi.c, wi.sz))
		    return wi.item;
	    }
	}
	return null;
    }

    public GItem[] items() {
	synchronized (wmap) {
	    final List<GItem> items = new ArrayList<>();
	    for (final WItem wi : wmap.values()) {
		items.add(wi.item);
	    }
	    return items.toArray(new GItem[0]);
	}
    }
}
