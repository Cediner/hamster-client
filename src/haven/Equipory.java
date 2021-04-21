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
import hamster.KeyBind;
import hamster.ui.equip.EquipmentItem;
import hamster.data.character.EquipmentType;
import haven.res.ui.tt.Armor;
import haven.res.ui.tt.ISlots;
import haven.res.ui.tt.attrmod.AttrMod;
import haven.res.ui.tt.wpn.Damage;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.*;
import java.util.List;

import static hamster.KeyBind.*;
import static haven.Inventory.invsq;

public class Equipory extends Widget implements DTarget {
    private static final Tex bg = Resource.loadtex("gfx/hud/equip/bg");
    private static final Color debuff = new Color(255, 128, 128);
    private static final Color buff = new Color(128, 255, 128);
    private static final int
	rx = invsq.sz().x + bg.sz().x,
	yo = Inventory.sqsz.y;
    public static final Coord bgc = new Coord(invsq.sz().x, 0);
    public static final Coord ecoords[] = {
	new Coord( 0, 0 * yo), //head
	new Coord( 0, 1 * yo), //main accessory
	new Coord( 0, 2 * yo), //Shirt
	new Coord(rx, 2 * yo),  //Torso Armor
	new Coord( 0, 3 * yo), //Gloves
	new Coord(rx, 3 * yo), //Belt
	new Coord( 0, 4 * yo), //Left Hand
	new Coord(rx, 4 * yo), //Right Hand
	new Coord( 0, 5 * yo), //Left Hand Ring
	new Coord(rx, 5 * yo), //Right Hand Ring
	new Coord( 0, 6 * yo), //Cloaks & Robes
	new Coord(rx, 6 * yo), //Back
	new Coord( 0, 7 * yo), //Pants
	new Coord(rx, 7 * yo), //Leg Armor
	new Coord( 0, 8 * yo), //Cape
	new Coord(rx, 8 * yo), //Shoes
	new Coord(invsq.sz().x, 0 * yo), //Cosmetic Hat
	new Coord(rx, 0 * yo), //Eyes
	new Coord(rx, 1 * yo), //Mouth
    };
    public static final Tex[] ebgs = new Tex[ecoords.length];
    public static final Text[] etts = new Text[ecoords.length];
    static Coord isz;
    static {
	isz = new Coord();
	for(Coord ec : ecoords) {
	    if(ec.x + invsq.sz().x > isz.x)
		isz.x = ec.x + invsq.sz().x;
	    if(ec.y + invsq.sz().y > isz.y)
		isz.y = ec.y + invsq.sz().y;
	}
	for(int i = 0; i < ebgs.length; i++) {
	    Resource bgres = Resource.local().loadwait("gfx/hud/equip/ep" + i);
	    Resource.Image img = bgres.layer(Resource.imgc);
	    if(img != null) {
		ebgs[i] = bgres.layer(Resource.imgc).tex();
		etts[i] = Text.render(bgres.layer(Resource.tooltip).t);
	    }
	}
    }

    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
    public final WItem[] slots = new WItem[ecoords.length];
    Map<GItem, Collection<WItem>> wmap = new HashMap<>();
    private final Avaview ava;
    private GItem lweap, rweap;

    @RName("epry")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    long gobid;
	    if(args.length < 1)
		gobid = -2;
	    else if(args[0] == null)
		gobid = -1;
	    else
		gobid = Utils.uint32((Integer)args[0]);
	    return(new Equipory(gobid));
	}
    }

    protected void added() {
	if(ava.avagob == -2)
	    ava.avagob = getparent(GameUI.class).plid;
    }

    public Equipory(long gobid) {
	super(isz);
	ava = add(new Avaview(bg.sz(), gobid, "equcam") {
		public boolean mousedown(Coord c, int button) {
		    return(false);
		}

		public void draw(GOut g) {
		    g.image(bg, Coord.z);
		    super.draw(g);
		}

		{
		    basic.add(new Outlines(GlobalSettings.SYMMETRICOUTLINES));
		}

		final FColor cc = new FColor(0, 0, 0, 0);
		protected FColor clearcolor() {return(cc);}
	    }, bgc);
	ava.color = null;
	setKeybinds();
    }

    private void setKeybinds() {
    	binds.put(KB_EQ_HELD_INTO_LH, () -> {
    	   if(!ui.gui.hand.isEmpty()) {
    	       wdgmsg("drop", EquipmentType.LeftHand.slot);
    	       return true;
	   } else if(leftHand() != null) {
    	       leftHand().wdgmsg("take", Coord.o);
    	       return true;
	   }
    	   return false;
	});
	binds.put(KB_EQ_HELD_INTO_RH, () -> {
	    if(!ui.gui.hand.isEmpty()) {
		wdgmsg("drop", EquipmentType.RightHand.slot);
		return true;
	    } else if(rightHand() != null) {
		rightHand().wdgmsg("take", Coord.o);
		return true;
	    }
	    return false;
	});
    }

    public boolean globtype(char key, KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
	return(super.globtype(key, ev));
    }

    public static interface SlotInfo {
	public int slots();
    }

    public GItem getWeapon() {
	if (lweap != null && lweap.getinfo(Damage.class).isPresent()) {
	    return lweap;
	} else if (rweap != null && rweap.getinfo(Damage.class).isPresent()) {
	    return rweap;
	} else {
	    return null;
	}
    }

    public GItem leftHand() {
	return lweap;
    }

    public GItem rightHand() {
	return rweap;
    }

    /*******************************************************************************
     * For Scripting API only
     */
    public EquipmentItem[] getEquippedItems() {
	final ArrayList<EquipmentItem> itms = new ArrayList<>();

	for (final GItem itm : children(GItem.class)) {
	    EquipmentType type = EquipmentType.Unknown;
	    for (WItem witm : wmap.get(itm)) {
		if (EquipmentType.eqmap.containsKey(witm.c)) {
		    type = EquipmentType.eqmap.get(witm.c);
		}
	    }
	    itms.add(new EquipmentItem(type, itm));
	}

	return itms.toArray(new EquipmentItem[0]);
    }
    /******************************************************************************/

    public void addchild(Widget child, Object... args) {
	if(child instanceof GItem) {
	    add(child);
	    GItem g = (GItem)child;
	    ArrayList<WItem> v = new ArrayList<>();
	    for (Object arg : args) {
		int ep = (Integer) arg;
		final var itm = new WItem(g);
		if (ep < ecoords.length) {
		    v.add(add(itm, ecoords[ep].add(1, 1)));
		    slots[ep] = itm;
		}
		switch (ep) {
		    case 5 -> {
			if (GlobalSettings.SHOWBELTONLOGIN.get()) {
			    g.delayediact = true;
			}
		    }
		    case 6 -> {
		        lweap = g;
			ui.gui.lrhandview.additm(itm, new Coord(0, 0));
		    }
		    case 7 -> {
			rweap = g;
			ui.gui.lrhandview.additm(itm, new Coord(1, 0));
		    }
		}
	    }
	    v.trimToSize();
	    wmap.put(g, v);
	} else {
	    super.addchild(child, args);
	}
    }

    public void cdestroy(Widget w) {
	super.cdestroy(w);
	if(w instanceof GItem) {
	    GItem i = (GItem)w;
	    final Collection<WItem> witms = wmap.remove(i);
	    for(WItem v : witms) {
		ui.destroy(v);
		for (int s = 0; s < slots.length; ++s) {
		    if (slots[s] == v) {
			slots[s] = null;
		    }
		}
	    }
	    if (lweap == i) {
		lweap = null;
		for (WItem v : witms)
		    ui.gui.lrhandview.remitm(v);
	    } else if (rweap == i) {
		rweap = null;
		for (WItem v : witms)
		    ui.gui.lrhandview.remitm(v);
	    }
	}
    }

    public void uimsg(String msg, Object... args) {
	if(msg == "pop") {
	    ava.avadesc = Composited.Desc.decode(ui.sess, args);
	} else {
	    super.uimsg(msg, args);
	}
    }

    public int epat(Coord c) {
	for(int i = 0; i < ecoords.length; i++) {
	    if(c.isect(ecoords[i], invsq.sz()))
		return(i);
	}
	return(-1);
    }

    public boolean drop(Coord cc, Coord ul) {
	wdgmsg("drop", epat(cc));
	return(true);
    }

    public void drawslots(GOut g) {
	int slots = 0;
	GameUI gui = getparent(GameUI.class);
	if((gui != null) && (gui.vhand != null)) {
	    try {
		SlotInfo si = ItemInfo.find(SlotInfo.class, gui.vhand.item.info());
		if(si != null)
		    slots = si.slots();
	    } catch(Loading l) {
	    }
	}
	for(int i = 0; i < ecoords.length; i++) {
	    if((slots & (1 << i)) != 0) {
		g.chcolor(255, 255, 0, 64);
		g.frect(ecoords[i].add(1, 1), invsq.sz().sub(2, 2));
		g.chcolor();
	    }
	    g.image(invsq, ecoords[i]);
	    if(ebgs[i] != null)
		g.image(ebgs[i], ecoords[i]);
	}
    }

    public Object tooltip(Coord c, Widget prev) {
	Object tt = super.tooltip(c, prev);
	if(tt != null)
	    return(tt);
	int sl = epat(c);
	if(sl >= 0)
	    return(etts[sl]);
	return(null);
    }

    public void draw(GOut g) {
	drawslots(g);
	super.draw(g);

	if(GlobalSettings.SHOWEQUIPSTATS.get()) {
	    //Show Armor class in bottom left
	    try {
		int h = 0, s = 0;
		for (final GItem itm : wmap.keySet()) {
		    for (final ItemInfo info : itm.info()) {
			if (info instanceof Armor) {
			    h += ((Armor) info).hard;
			    s += ((Armor) info).soft;
			    break;
			}
		    }
		}
		final int width = FastText.textw(String.format("Armor Class %,d/%,d", h, s));
		g.chcolor(new Color(64, 64, 64, 215));
		g.frect(new Coord(invsq.sz().x + 5, sz.y - 15), new Coord(width, 15));
		g.chcolor();
		FastText.aprintf(g, new Coord(invsq.sz().x + 5, sz.y), 0.0, 1.0, "Armor Class %,d/%,d", h, s);
	    } catch (Exception e) {
		//fail silently
	    }
	    //Show Buffs/Debuffs from gear in bottom right
	    try {
		final HashMap<String, Integer> mods = new HashMap<>();
		int w = 0;
		for (final GItem itm : wmap.keySet()) {
		    for (final ItemInfo info : itm.info()) {
			if (info instanceof AttrMod) {
			    for (final AttrMod.Mod mod : ((AttrMod) info).mods) {
				mods.put(mod.name(), mods.getOrDefault(mod.name(), 0) + mod.mod);
				if (mods.get(mod.name()) != 0) {
				    w = Math.max(w, FastText.textw(mod.name() + ": " + mods.get(mod.name())));
				} else {
				    mods.remove(mod.name());
				}
			    }
			} else if (info instanceof ISlots) {
			    final ISlots slots = (ISlots) info;
			    for (final ISlots.SItem sitm : slots.s) {
				for (final ItemInfo sinfo : sitm.info) {
				    if (sinfo instanceof AttrMod) {
					for (final AttrMod.Mod mod : ((AttrMod) sinfo).mods) {
					    mods.put(mod.name(), mods.getOrDefault(mod.name(), 0) + mod.mod);
					    if (mods.get(mod.name()) != 0) {
						w = Math.max(w, FastText.textw(mod.name() + ": " + mods.get(mod.name())));
					    } else {
						mods.remove(mod.name());
					    }
					}
				    }
				}
			    }
			}
		    }
		}

		if (w > 0) {
		    final List<String> names = new ArrayList<>(mods.keySet());
		    names.sort(String::compareTo);
		    final int h = names.size() * 15;
		    final int x = invsq.sz().x + 5;
		    //background
		    g.chcolor(new Color(64, 64, 64, 215));
		    g.frect(sz.sub(x + w, h), new Coord(w, h));
		    g.chcolor();

		    //
		    int y = 0;
		    for (final String mod : names) {
			final int val = mods.get(mod);
			g.chcolor(val >= 0 ? buff : debuff);
			FastText.aprintf(g, sz.sub(x, y), 1.0, 1.0, "%s: %d", mod, val);
			g.chcolor();
			y += 15;
		    }
		}
	    } catch (Exception e) {
		//fail silently
	    }
	}
    }

    public boolean iteminteract(Coord cc, Coord ul) {
	return(false);
    }
}
