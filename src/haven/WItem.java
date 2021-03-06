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

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.*;

import hamster.GlobalSettings;
import hamster.MouseBind;
import hamster.data.itm.ItemData;
import hamster.data.character.EquipmentType;
import haven.ItemInfo.AttrCache;
import haven.res.ui.tt.Wear;
import haven.res.ui.tt.q.qbuff.Quality;
import haven.resutil.Curiosity;

import static haven.Inventory.sqsz;

public class WItem extends Widget implements DTarget {
    public static final Resource missing = Resource.local().loadwait("gfx/invobjs/missing");
    public static final Tex lockt = Resource.loadtex("custom/inv/locked");
    public final GItem item;
    private Resource cspr = null;
    private Message csdt = Message.nil;
    private boolean locked = false;
    //Quality display
    private double q_last;
    private Tex qtex;

    public WItem(GItem item) {
	super(sqsz);
	this.item = item;
	this.item.setWItem(this);
	itemols  = new AttrCache<>(this::info, info -> {
	    ArrayList<GItem.InfoOverlay<?>> buf = new ArrayList<>();
	    for(ItemInfo inf : info) {
		if (inf instanceof GItem.OverlayInfo) {
		    if (inf instanceof Quality) {
			//For Quality if we have Contents we'd rather get their quality over the item quality
			item.getinfo(ItemInfo.Contents.class).ifPresentOrElse(cnt -> {
			    item.getinfo(Quality.class, cnt.sub)
				    .ifPresentOrElse(q -> buf.add(GItem.InfoOverlay.create(q)),
					    () -> buf.add(GItem.InfoOverlay.create((GItem.OverlayInfo<?>) inf)));
			}, () -> buf.add(GItem.InfoOverlay.create((GItem.OverlayInfo<?>) inf)));
		    } else {
			buf.add(GItem.InfoOverlay.create((GItem.OverlayInfo<?>) inf));
		    }
		}
	    }
	    GItem.InfoOverlay<?>[] ret = buf.toArray(new GItem.InfoOverlay<?>[0]);
	    return(() -> ret);
	});
    }

    public boolean locked() {
	return locked;
    }

    public void setLock(final boolean val) {
	locked = val;
    }

    public void drawmain(GOut g, GSprite spr) {
	spr.draw(g);
    }

    public static BufferedImage shorttip(List<ItemInfo> info) {
	return(ItemInfo.shorttip(info));
    }

    public static BufferedImage longtip(GItem item, List<ItemInfo> info) {
	BufferedImage img = ItemInfo.longtip(info);
	Resource.Pagina pg = item.res.get().layer(Resource.pagina);
	if(pg != null)
	    img = ItemInfo.catimgs(0, img, RichText.render("\n" + pg.text, UI.scale(200)).img);
	if(GlobalSettings.DEBUG.get()) {
	    try {
		img = ItemInfo.catimgs(0, img, RichText.render("\n" + item.resource().name, UI.scale(200)).img);
	    } catch (Loading ignored) { }
	}
	return(img);
    }

    public BufferedImage longtip(List<ItemInfo> info) {
	return(longtip(item, info));
    }

    public class ItemTip implements Indir<Tex> {
	private final TexI tex;

	public ItemTip(BufferedImage img) {
	    if(img == null)
		throw(new Loading());
	    tex = new TexI(img);
	}

	public GItem item() {
	    return(item);
	}

	public Tex get() {
	    return(tex);
	}
    }

    public class ShortTip extends ItemTip {
	public ShortTip(List<ItemInfo> info) {super(shorttip(info));}
    }

    public class LongTip extends ItemTip {
	public LongTip(List<ItemInfo> info) {super(longtip(info));}
    }

    private double hoverstart;
    private ItemTip shorttip = null, longtip = null;
    private List<ItemInfo> ttinfo = null;

    public Object tooltip(Coord c, Widget prev) {
	double now = Utils.rtime();
	if(prev == this) {
	} else if(prev instanceof WItem) {
	    double ps = ((WItem)prev).hoverstart;
	    if(now - ps < 1.0)
		hoverstart = now;
	    else
		hoverstart = ps;
	} else {
	    hoverstart = now;
	}
	try {
	    List<ItemInfo> info = item.info();
	    if(info.size() < 1)
		return(null);
	    if(info != ttinfo) {
		shorttip = longtip = null;
		ttinfo = info;
	    }
	    if(now - hoverstart < 1.0 && !GlobalSettings.ALWAYSITEMLONGTIPS.get()) {
		if(shorttip == null)
		    shorttip = new ShortTip(info);
		return(shorttip);
	    } else {
		if(longtip == null)
		    longtip = new LongTip(info);
		return(longtip);
	    }
	} catch(Loading e) {
	    return("...");
	}
    }

    private List<ItemInfo> info() {return(item.info());}
    public final AttrCache<Color> olcol = new AttrCache<>(this::info, info -> {
	    Color ret = null;
	    for(ItemInfo inf : info) {
		if(inf instanceof GItem.ColorInfo) {
		    Color c = ((GItem.ColorInfo)inf).olcol();
		    if(c != null)
			ret = (ret == null) ? c : Utils.preblend(ret, c);
		}
	    }
	    Color fret = ret;
	    return(() -> fret);
	});
    public final AttrCache<GItem.InfoOverlay<?>[]> itemols;
    public final AttrCache<Double> itemmeter = new AttrCache<Double>(this::info, AttrCache.map1(GItem.MeterInfo.class, minf -> minf::meter));

    private GSprite lspr = null;
    public void tick(double dt) {
	/* XXX: This is ugly and there should be a better way to
	 * ensure the resizing happens as it should, but I can't think
	 * of one yet. */
	GSprite spr = item.spr();
	if((spr != null) && (spr != lspr)) {
	    Coord sz = new Coord(spr.sz());
	    if((sz.x % sqsz.x) != 0)
		sz.x = sqsz.x * ((sz.x / sqsz.x) + 1);
	    if((sz.y % sqsz.y) != 0)
		sz.y = sqsz.y * ((sz.y / sqsz.y) + 1);
	    resize(sz);
	    lspr = spr;
	}
    }

    public int size() {
	return sz.div(sqsz).area();
    }

    private static final Color[] wearclr = new Color[]{
	    new Color(233, 0, 14),
	    new Color(218, 128, 87),
	    new Color(246, 233, 87),
	    new Color(145, 225, 60)
    };

    public int wearlevel() {
	final Optional<Wear> wear = item.getinfo(Wear.class);
	if (wear.isPresent()) {
	    double p = 1 - wear.get().percent();
	    int h = (int) (p * (double) sz.y);
	    return p == 1.0 ? 3 : (int) (p / 0.25);
	} else {
	    return -1;
	}
    }

    private static final Color bgcol = new Color(128, 128, 128, 128);
    public void draw(GOut g) {
	GSprite spr = item.spr();
	if(spr != null) {
	    Coord sz = spr.sz();
	    g.defstate();
	    if(olcol.get() != null)
		g.usestate(new ColorMask(olcol.get()));
	    drawmain(g, spr);
	    g.defstate();
	    GItem.InfoOverlay<?>[] ols = itemols.get();
	    if(ols != null) {
		for(GItem.InfoOverlay<?> ol : ols)
		    ol.draw(g);
	    }
	    Double meter = (item.meter > 0) ? Double.valueOf(item.meter / 100.0) : itemmeter.get();
	    final var curio = item.getinfo(Curiosity.class);
	    if((meter != null) && (meter > 0)) {
		g.chcolor(255, 255, 255, 64);
		Coord half = sz.div(2);
		g.prect(half, half.inv(), half, meter * Math.PI * 2);
		g.chcolor();

		final String text;
		if(curio.isPresent() && GlobalSettings.SHOWTIMELEFTCURIO.get()) {
		    text = Curiosity.timeleft((int)(curio.get().getRealTime() - (curio.get().getRealTime() * meter)));
		} else if (GlobalSettings.SHOWMETERPER.get()) {
		    text = String.format("%s%%", (int)(meter * 100));
		} else {
		    text = null;
		}

		if(text != null) {
		    final int width = FastText.textw(text);
		    final Coord tsz = new Coord(width, 15);
		    final Coord c = new Coord(0, sz.y - tsz.y);
		    g.chcolor(bgcol);
		    g.frect(c, tsz);
		    g.chcolor();
		    FastText.print(g, c, text);
		}
	    } else if(GlobalSettings.SHOWTIMELEFTCURIO.get() && curio.isPresent()) {
		final var text = Curiosity.timeleft((int)(curio.get().getRealTime()));
		final int width = FastText.textw(text);
		final Coord tsz = new Coord(width, 15);
		final Coord c = new Coord(0, sz.y - tsz.y);
		g.chcolor(bgcol);
		g.frect(c, tsz);
		g.chcolor();
		FastText.print(g, c, text);
	    }

	    if(GlobalSettings.SHOWITEMWM.get()) {
	        item.getinfo(ItemInfo.AdHoc.class).ifPresent(inf -> {
		    if(inf.str.text.equals("Well mined")) {
			FastText.asprint(g, sz, 1.0, 1.0, "W");
		    }
		});
	    }

	    if (GlobalSettings.SHOWITEMWEAR.get()) {
		item.getinfo(Wear.class).ifPresent(wear -> {
		    double p = 1 - wear.percent();
		    int h = (int) (p * (double) sz.y);
		    g.chcolor(wearclr[p == 1.0 ? 3 : (int) (p / 0.25)]);
		    g.frect(new Coord(0, sz.y - h), new Coord(3, h));
		    g.chcolor();
		});
	    }

	    if (locked) {
	        g.aimage(lockt, sz.div(2), 0.5, 0.5);
	    }
	} else {
	    g.image(missing.layer(Resource.imgc).tex(), Coord.z, sz);
	}
    }

    public boolean mousedown(Coord c, int btn) {
	final String seq = MouseBind.generateSequence(ui, btn);
	if (!(MouseBind.ITM_TRANSFER.check(seq, () -> {
	    if(!locked) {
		item.wdgmsg("transfer", c, 1);
		return true;
	    } else {
	        return false;
	    }
	}) || MouseBind.ITM_TRANSFER_ALL_ALIKE.check(seq, () -> {
	    if(!locked) {
	        //Note: Since this is server-side locks on items don't apply
		item.wdgmsg("transfer", c, -1);
		return true;
	    } else {
		return false;
	    }
	}) || MouseBind.ITM_DROP.check(seq, () -> {
	    if(!locked) {
		item.wdgmsg("drop", c, 1);
		return true;
	    } else {
		return false;
	    }
	}) || MouseBind.ITM_DROP_ALL_ALIKE.check(seq, () -> {
	    if(!locked) {
		//Note: Since this is server-side locks on items don't apply
		item.wdgmsg("drop", c, -1);
		return true;
	    } else {
	        return false;
	    }
	}) || MouseBind.ITM_TAKE.check(seq, () -> {
	    if(!locked) {
		item.wdgmsg("take", c);
		return true;
	    } else {
	        return false;
	    }
	}) || MouseBind.ITM_TOGGLE_LOCK.check(seq, () -> {
	    locked = !locked;
	    return true;
	}) || MouseBind.ITM_AUTO_EQUIP.check(seq, () -> {
	    final Optional<String> name = item.name();
	    if (!locked && name.isPresent() && GlobalSettings.AUTOEQUIP.get() && ItemData.isEquipable(name.get())) {
		if (!(parent instanceof Equipory)) {
		    item.wdgmsg("take", c);
		    ui.gui.equ.wdgmsg("drop", -1);
		} else {
		    item.wdgmsg("transfer", c);
		}
		return true;
	    } else {
		return false;
	    }
	}) || MouseBind.ITM_AUTO_EQUIP_LH.check(seq, () -> {
	    final Optional<String> name = item.name();
	    if (!locked && name.isPresent() && GlobalSettings.AUTOEQUIP.get() && ItemData.isEquipable(name.get())) {
		item.wdgmsg("take", c);
		ui.gui.equ.wdgmsg("drop", EquipmentType.LeftHand.slot);
		return true;
	    } else {
		return false;
	    }
	}) || MouseBind.ITM_AUTO_EQUIP_RH.check(seq, () -> {
	    final Optional<String> name = item.name();
	    if (!locked && name.isPresent() && GlobalSettings.AUTOEQUIP.get() && ItemData.isEquipable(name.get())) {
		item.wdgmsg("take", c);
		ui.gui.equ.wdgmsg("drop", EquipmentType.RightHand.slot);
		return true;
	    } else {
		return false;
	    }
	}))) {
	    if (btn == 3) {
		item.wdgmsg("iact", c, ui.modflags());
		return true;
	    } else {
		return false;
	    }
	} else {
	    return true;
	}
    }

    public boolean drop(Coord cc, Coord ul) {
	return(false);
    }

    public boolean iteminteract(Coord cc, Coord ul) {
	item.wdgmsg("itemact", ui.modflags());
	return(true);
    }
}
