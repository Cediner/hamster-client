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
import hamster.MouseBind;
import haven.resutil.WaterTile;

public class ItemDrag extends WItem {
    public Coord doff;
    
    public ItemDrag(Coord dc, GItem item) {
	super(item);
	z(100);
	this.doff = dc;
    }

    protected void added() {
	this.c = parent.ui.mc.add(doff.inv());
	ui.grabmouse(this);
    }
    
    public void drawmain(GOut g, GSprite spr) {
	g.chcolor(255, 255, 255, 128);
	super.drawmain(g, spr);
	g.chcolor();
    }

    public boolean dropon(Widget w, Coord c) {
	if(w instanceof DTarget) {
	    if(((DTarget)w).drop(c, c.add(doff.inv())))
		return(true);
	}
	for(Widget wdg = w.lchild; wdg != null; wdg = wdg.prev) {
	    if((wdg == this) || !wdg.visible)
		continue;
	    Coord cc = w.xlate(wdg.c, true);
	    if(c.isect(cc, wdg.sz)) {
		if(dropon(wdg, c.add(cc.inv())))
		    return(true);
	    }
	}
	return(false);
    }
	
    public boolean interact(Widget w, Coord c) {
	if(w instanceof DTarget) {
	    if(((DTarget)w).iteminteract(c, c.add(doff.inv())))
		return(true);
	}
	for(Widget wdg = w.lchild; wdg != null; wdg = wdg.prev) {
	    if((wdg == this) || !wdg.visible)
		continue;
	    Coord cc = w.xlate(wdg.c, true);
	    if(c.isect(cc, wdg.sz)) {
		if(interact(wdg, c.add(cc.inv())))
		    return(true);
	    }
	}
	return(false);
    }
	
    public boolean mousedown(Coord c, int button) {
        final String bind = MouseBind.generateSequence(ui, button);
        return (MouseBind.HITM_TOGGLE_LOCK.check(bind, () -> {
	    setLock(!locked());
	    return true;
	})) || (MouseBind.HITM_DROP.check(bind, () -> {
	    final Gob me = ui.sess.glob.oc.getgob(ui.gui.map.rlplgob);
	    if(!locked()
		&& (!GlobalSettings.WATERDROPITEMCTRL.get()
		    || me == null
		    || !(ui.sess.glob.map.tiler(ui.sess.glob.map.gettile_safe(me.rc.floor(MCache.tilesz))) instanceof WaterTile))) {
		dropon(parent, c.add(this.c));
		return true;
	    } else {
	        return false;
	    }
	})) || (MouseBind.HITM_DROP_WATER.check(bind, () -> {
	    if(!locked()) {
		dropon(parent, c.add(this.c));
		return true;
	    } else {
		return false;
	    }
	})) || (MouseBind.HITM_IACT_OBJ.check(bind, () -> {
	    if(!locked()) {
	        ui.setmods(false, false, false);
		interact(parent, c.add(this.c));
		return true;
	    } else {
		return false;
	    }
	})) || (MouseBind.HITM_TRANS_OBJ_SHIFT.check(bind, () -> {
	    if(!locked()) {
		ui.setmods(true, false, false);
		interact(parent, c.add(this.c));
		return true;
	    } else {
		return false;
	    }
	})) || (MouseBind.HITM_TRANS_ALL_OBJ.check(bind, () -> {
	    if(!locked()) {
		ui.setmods(true, true, false);
		interact(parent, c.add(this.c));
		return true;
	    } else {
		return false;
	    }
	})) || (MouseBind.HITM_IACT.check(bind, () -> {
	    if(locked()) {
		item.wdgmsg("iact", c, 0);
		return true;
	    } else {
		return false;
	    }
	}));
    }

    public void mousemove(Coord c) {
	this.c = this.c.add(c.add(doff.inv()));
    }

    @Override
    public Object tooltip(Coord c, Widget prev) {
	return !locked() ? super.tooltip(c, prev) : null;
    }
}
