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

import hamster.KeyBind;
import hamster.data.itm.ItemData;
import hamster.gob.sprites.CurAggroSprite;
import hamster.gob.sprites.TargetSprite;
import hamster.ui.fight.*;
import haven.render.*;
import haven.res.ui.tt.q.qbuff.Quality;
import haven.res.ui.tt.wpn.Armpen;

import java.util.*;
import java.awt.Color;
import java.awt.event.KeyEvent;

import static hamster.KeyBind.*;

public class  Fightsess extends Widget {
    private static final Coord off = new Coord(UI.scale(32), UI.scale(32));
    public static final Tex cdframe = Resource.loadtex("gfx/hud/combat/cool");
    public static final Tex actframe = Buff.frame;
    public static final Coord actframeo = Buff.imgoff;
    public static final Tex indframe = Resource.loadtex("gfx/hud/combat/indframe");
    public static final Coord indframeo = (indframe.sz().sub(off)).div(2);
    public static final Tex indbframe = Resource.loadtex("gfx/hud/combat/indbframe");
    public static final Coord indbframeo = (indframe.sz().sub(off)).div(2);
    public static final Tex useframe = Resource.loadtex("gfx/hud/combat/lastframe");
    public static final Coord useframeo = (useframe.sz().sub(off)).div(2);
    public static final int actpitchx = UI.scale(75);
    public static final int actpitchy = UI.scale(75);
    public final Action[] actions;
    public int use = -1, useb = -1;
    public Coord pcc;
    public int pho;
    private Fightview fv;

    // Enhanced Combat UI
    private Coord actionAnchor;
    private Coord enemyBuffAnchor;
    private Coord enemyIPAnchor;
    private Coord enemyLastMoveAnchor;
    private Coord buffAnchor;
    private Coord IPAnchor;
    private Coord lastMoveAnchor;
    private Coord cooldownAnchor;
    //

    public static class Action {
	public final Indir<Resource> res;
	public double cs, ct;

	// Enhanced Combat UI
	public final int id;
	public Card card;
	public int cards;
	private boolean discovered;
	//

	public Action(Indir<Resource> res, final int id) {
	    this.res = res;
	    this.id = id;
	}

	public boolean isDiscovered() {
	    return discovered;
	}

	void tick(final UI ui) {
	    if (!discovered) {
		try {
		    card = Cards.lookup.getOrDefault(res.get().layer(Resource.tooltip).t, Cards.unknown);
		    cards = ui.gui.chrwdg.fight.cards(res.get().name);
		    discovered = true;
		} catch (Loading l) {
		    //ignore
		}
	    }
	}
    }

    @RName("fsess")
    public static class $_ implements Factory {
	public Widget create(UI ui, Object[] args) {
	    int nact = (Integer)args[0];
	    return(new Fightsess(nact));
	}
    }

    public Fightsess(int nact) {
	pho = -UI.scale(40);
	this.actions = new Action[nact];
	setKeybinds();
    }

    protected void added() {
	fv = parent.getparent(GameUI.class).fv;
	presize();
    }

    @Override
    protected void removed() {
	super.removed();
	ui.gui.fs = null;
    }

    public void presize() {
	resize(parent.sz);
	pcc = sz.div(2);
	final Coord center = sz.div(2);
	actionAnchor = new Coord(center.x, sz.y).sub(0, actc(9).y+UI.scale(100));
	cooldownAnchor = new Coord(center.x, 0).add(0,  (center.y / 2));
	enemyBuffAnchor = cooldownAnchor.add(UI.scale(50, 0));
	enemyIPAnchor = cooldownAnchor.add(UI.scale(75, 15));
	enemyLastMoveAnchor = cooldownAnchor.add(UI.scale(50, 50));
	buffAnchor = cooldownAnchor.sub(UI.scale(50, 0));
	IPAnchor = cooldownAnchor.add(UI.scale(-75, 15));
	lastMoveAnchor = cooldownAnchor.add(UI.scale(-50, 50));
    }

    private void updatepos() {
	MapView map;
	Gob pl;
	if(((map = getparent(GameUI.class).map) == null) || ((pl = map.player()) == null))
	    return;
	Coord3f raw = pl.placed.getc();
	if(raw == null)
	    return;
	pcc = map.screenxf(raw).round2();
	pho = (map.screenxf(raw.add(0, 0, UI.scale(20))).round2().sub(pcc).y) - UI.scale(20);
    }

    private static class Effect implements RenderTree.Node {
	Sprite spr;
	RenderTree.Slot slot;
	boolean used = true;

	Effect(Sprite spr) {this.spr = spr;}

	public void added(RenderTree.Slot slot) {
	    slot.add(spr);
	}
    }

    private static final Resource tgtfx = Resource.local().loadwait("gfx/hud/combat/trgtarw");
    private final Collection<Effect> curfx = new ArrayList<>();

    private Effect fxon(long gobid, Resource fx, Effect cur) {
	MapView map = getparent(GameUI.class).map;
	Gob gob = ui.sess.glob.oc.getgob(gobid);
	if((map == null) || (gob == null))
	    return(null);
	Pipe.Op place;
	try {
	    place = gob.placed.curplace();
	} catch(Loading l) {
	    return(null);
	}
	if((cur == null) || (cur.slot == null)) {
	    try {
		//cur = new Effect(Sprite.create(null, fx, Message.nil));
		cur = new Effect(new CurAggroSprite(null));
		cur.slot = map.basic.add(cur.spr, place);
	    } catch(Loading l) {
		return(null);
	    }
	    curfx.add(cur);
	} else {
	    cur.slot.cstate(place);
	}
	cur.used = true;
	return(cur);
    }

    public void tick(double dt) {
        if(parent.child != this)
            raise();
	for(Iterator<Effect> i = curfx.iterator(); i.hasNext();) {
	    Effect fx = i.next();
	    if(!fx.used) {
		if(fx.slot != null) {
		    fx.slot.remove();
		    fx.slot = null;
		}
		i.remove();
	    } else {
		fx.used = false;
		fx.spr.tick(dt);
	    }
	}
	for (final var action : actions) {
	    if (action != null) {
		action.tick(ui);
	    }
	}
    }

    public void destroy() {
	for(Effect fx : curfx) {
	    if(fx.slot != null)
		fx.slot.remove();
	}
	curfx.clear();
	super.destroy();
    }

    private static final Text.Furnace ipf = new PUtils.BlurFurn(new Text.Foundry(Text.serif, 18, new Color(128, 128, 255)).aa(true), 1, 1, new Color(48, 48, 96));
    private final Text.UText<?> ip = new Text.UText<Integer>(ipf) {
	public String text(Integer v) {return("IP: " + v);}
	public Integer value() {return(fv.current.ip);}
    };
    private final Text.UText<?> oip = new Text.UText<Integer>(ipf) {
	public String text(Integer v) {return("IP: " + v);}
	public Integer value() {return(fv.current.oip);}
    };

    private static Coord actc(int i) {
	int rl = 5;
	return(new Coord((actpitchx * (i % rl)) - (((rl - 1) * actpitchx) / 2),
		UI.scale(125) + ((i / rl) * actpitchy)));
    }

    private Indir<Resource> lastact1 = null, lastact2 = null;
    private Text lastacttip1 = null, lastacttip2 = null;
    private Effect curtgtfx;
    public void draw(GOut g) {
	updatepos();
	double now = Utils.rtime();

	for (Buff buff : fv.buffs.children(Buff.class)) {
	    buff.sessdraw(g, buffAnchor.add(-buff.c.x - Buff.cframe.sz().x - UI.scale(20),
		    buff.c.y - Buff.cframe.sz().y));
	}

	if(fv.current != null) {
	    for (Buff buff : fv.current.buffs.children(Buff.class)) {
		buff.sessdraw(g, enemyBuffAnchor.add(buff.c.x + UI.scale(20), buff.c.y - Buff.cframe.sz().y));
	    }

	    g.aimage(ip.get().tex(), IPAnchor, 1, 0.5);
	    g.aimage(oip.get().tex(), enemyIPAnchor, 0, 0.5);

	    curtgtfx = fxon(fv.current.gobid, tgtfx, curtgtfx);
	}

	{
	    Coord cdc = cooldownAnchor;
	    if (now < fv.atkct) {
		double a = (now - fv.atkcs) / (fv.atkct - fv.atkcs);
		g.chcolor(255, 0, 128, 224);
		g.fellipse(cdc, UI.scale(24, 24), Math.PI / 2 - (Math.PI * 2 * Math.min(1.0 - a, 1.0)),
			Math.PI / 2);
		g.chcolor();
		FastText.aprintf(g, cdc, 0.5, 0.5, "%.1f", fv.atkct - now);
	    }
	    g.image(cdframe, cdc.sub(cdframe.sz().div(2)));
	    if (fv.current != null && fv.current.estimatedBlockWeight != 0) {
		final int stat;
		final WeightType type;
		if (fv.current.maneuver != null) {
		    stat = (int) fv.current.maneuver.calculateStat(fv.current.estimatedBlockWeight);
		    type = fv.current.maneuver.type;
		} else {
		    //An animal, just assume blockweight -> UA
		    type = WeightType.UA;
		    stat = (int) fv.current.estimatedBlockWeight;
		}
		FastText.aprintf(g, cdc.add(0, -50), 0.5, 0.0, "%s: %d", type, stat);
	    }
	}

	try {
	    Indir<Resource> lastact = fv.lastact;
	    if(lastact != this.lastact1) {
		this.lastact1 = lastact;
		this.lastacttip1 = null;
	    }
	    double lastuse = fv.lastuse;
	    if(lastact != null) {
		Tex ut = lastact.get().layer(Resource.imgc).tex();
		Coord useul = lastMoveAnchor.sub(ut.sz().div(2));
		g.image(ut, useul);
		g.image(useframe, useul.sub(useframeo));
		double a = now - lastuse;
		if(a < 1) {
		    Coord off = new Coord((int)(a * ut.sz().x / 2), (int)(a * ut.sz().y / 2));
		    g.chcolor(255, 255, 255, (int)(255 * (1 - a)));
		    g.image(ut, useul.sub(off), ut.sz().add(off.mul(2)));
		    g.chcolor();
		}
	    }
	} catch(Loading ignored) { }
	if(fv.current != null) {
	    try {
		Indir<Resource> lastact = fv.current.lastact;
		if(lastact != this.lastact2) {
		    this.lastact2 = lastact;
		    this.lastacttip2 = null;
		}
		double lastuse = fv.current.lastuse;
		if(lastact != null) {
		    Tex ut = lastact.get().layer(Resource.imgc).tex();
		    Coord useul = enemyLastMoveAnchor.sub(ut.sz().div(2));
		    g.image(ut, useul);
		    g.image(useframe, useul.sub(useframeo));
		    double a = now - lastuse;
		    if(a < 1) {
			Coord off = new Coord((int)(a * ut.sz().x / 2), (int)(a * ut.sz().y / 2));
			g.chcolor(255, 255, 255, (int)(255 * (1 - a)));
			g.image(ut, useul.sub(off), ut.sz().add(off.mul(2)));
			g.chcolor();
		    }
		}
	    } catch(Loading ignored) { }
	}

	//My cards
	final GItem weapon = weap();
	final int weapq;
	final int weapdmg;
	final double weappen;
	if (weapon != null) {
	    weapq = weapon.getinfo(Quality.class).map(quality -> (int) quality.q).orElse(10);
	    weapdmg = ItemData.getWeaponDmg(weapon.name().orElse(""));
	    weappen = weapon.getinfo(Armpen.class).orElse(Armpen.NOPEN).deg;
	} else {
	    weapq = weapdmg = 0;
	    weappen = 0.0;
	}
	for (int i = 0; i < actions.length; i++) {
	    Coord ca = actionAnchor.add(actc(i));
	    Action act = actions[i];
	    try {
		if (act != null) {
		    Resource res = act.res.get();
		    Tex img = res.layer(Resource.imgc).tex();
		    Coord ic = ca.sub(img.sz().div(2));
		    g.image(img, ic);
		    if (now < act.ct) {
			//This is from an era when moves had their own cooldown
			double a = (now - act.cs) / (act.ct - act.cs);
			g.chcolor(0, 0, 0, 128);
			g.prect(ca, ic.sub(ca), ic.add(img.sz()).sub(ca), (1.0 - a) * Math.PI * 2);
			g.chcolor();
		    }
		    if (i == use) {
			g.image(indframe, ic.sub(indframeo));
		    } else if (i == useb) {
			g.image(indbframe, ic.sub(indbframeo));
		    } else {
			g.image(actframe, ic.sub(actframeo));
		    }

		    if (fv.current != null) {
			if (act.card instanceof Attack) {
			    final Attack atk = (Attack) act.card;
			    final Pair<Double, Double> dmg = atk.calculateDamage(weapdmg, weapq, weappen,
				    str(), fv.current.defweights);
			    FastText.printf(g, ic.add(0, UI.scale(35)), "%d/%d", Math.round(dmg.a), Math.round(dmg.b));
			    final int ua = ui.sess.glob.getcattr("unarmed").comp;
			    final int mc = ui.sess.glob.getcattr("melee").comp;

			    final Map<DefenseType, Double> newWeights = atk.calculateEnemyDefWeights(fv.maneuver, fv.maneuvermeter,
				    ua, mc, act.cards,
				    fv.current.defweights, fv.current.estimatedBlockWeight);
			    FastText.printf(g, ic.add(0, UI.scale(45)), "%d/%d/%d/%d",
				    Math.round(newWeights.get(DefenseType.RED) * 100),
				    Math.round(newWeights.get(DefenseType.GREEN) * 100),
				    Math.round(newWeights.get(DefenseType.BLUE) * 100),
				    Math.round(newWeights.get(DefenseType.YELLOW) * 100));
			} else if (act.card instanceof Restoration) {
			    final Restoration restro = (Restoration) act.card;
			    final Map<DefenseType, Double> newWeights = restro.getFutureWeights(act.cards, fv.defweights);
			    FastText.printf(g, ic.add(0, UI.scale(35)), "%d/%d/%d/%d",
				    Math.round(newWeights.get(DefenseType.RED) * 100),
				    Math.round(newWeights.get(DefenseType.GREEN) * 100),
				    Math.round(newWeights.get(DefenseType.BLUE) * 100),
				    Math.round(newWeights.get(DefenseType.YELLOW) * 100));
			    if (act.card == Cards.flex) {
				final int ua = ui.sess.glob.getcattr("unarmed").comp;
				final int mc = ui.sess.glob.getcattr("melee").comp;

				final Map<DefenseType, Double> enemyWeights = restro.calculateEnemyDefWeights(fv.maneuver,
					fv.maneuvermeter,
					ua, mc, act.cards,
					fv.current.defweights, fv.current.estimatedBlockWeight);
				FastText.printf(g, ic.add(0, UI.scale(45)), "%d/%d/%d/%d",
					Math.round(enemyWeights.get(DefenseType.RED) * 100),
					Math.round(enemyWeights.get(DefenseType.GREEN) * 100),
					Math.round(enemyWeights.get(DefenseType.BLUE) * 100),
					Math.round(enemyWeights.get(DefenseType.YELLOW) * 100));
			    }
			}
		    }
		}
	    } catch (Loading ignored) { }
	}
    }

    GItem weap() {
	return ui.gui.equ != null ? ui.gui.equ.getWeapon() : null;
    }

    int str() {
	final Glob.CAttr strattr = ui.sess.glob.getcattr("str");
	return strattr.comp;
    }

    private Widget prevtt = null;
    private Text acttip = null;
    public Object tooltip(Coord c, Widget prev) {
	for(Buff buff : fv.buffs.children(Buff.class)) {
	    Coord dc = buffAnchor.add(-buff.c.x - Buff.cframe.sz().x - UI.scale(20), buff.c.y - Buff.cframe.sz().y);
	    if(c.isect(dc, buff.sz)) {
		Object ret = buff.tooltip(c.sub(dc), prevtt);
		if(ret != null) {
		    prevtt = buff;
		    return(ret);
		}
	    }
	}
	if(fv.current != null) {
	    for(Buff buff : fv.current.buffs.children(Buff.class)) {
		Coord dc = enemyBuffAnchor.add(buff.c.x + UI.scale(20), buff.c.y - Buff.cframe.sz().y);
		if(c.isect(dc, buff.sz)) {
		    Object ret = buff.tooltip(c.sub(dc), prevtt);
		    if(ret != null) {
			prevtt = buff;
			return(ret);
		    }
		}
	    }
	}
	for(int i = 0; i < actions.length; i++) {
	    Coord ca = actionAnchor.add(actc(i));
	    Indir<Resource> act = (actions[i] == null) ? null : actions[i].res;
	    try {
		if(act != null) {
		    Tex img = act.get().layer(Resource.imgc).tex();
		    ca = ca.sub(img.sz().div(2));
		    if(c.isect(ca, img.sz())) {
			String tip = act.get().layer(Resource.tooltip).t;
			tip += " ($b{$col[255,128,0]{" + KB_FIGHT_MOVE[i].bind.get() + "}})";
			if((acttip == null) || !acttip.text.equals(tip))
			    acttip = RichText.render(tip, -1);
			return(acttip);
		    }
		}
	    } catch(Loading ignored) {}
	}
	try {
	    Indir<Resource> lastact = this.lastact1;
	    if(lastact != null) {
		Coord usesz = lastact.get().layer(Resource.imgc).sz;
		Coord lac = lastMoveAnchor;
		if(c.isect(lac.sub(usesz.div(2)), usesz)) {
		    if(lastacttip1 == null)
			lastacttip1 = Text.render(lastact.get().layer(Resource.tooltip).t);
		    return(lastacttip1);
		}
	    }
	} catch(Loading ignored) {}
	try {
	    Indir<Resource> lastact = this.lastact2;
	    if(lastact != null) {
		Coord usesz = lastact.get().layer(Resource.imgc).sz;
		Coord lac = enemyLastMoveAnchor;
		if(c.isect(lac.sub(usesz.div(2)), usesz)) {
		    if(lastacttip2 == null)
			lastacttip2 = Text.render(lastact.get().layer(Resource.tooltip).t);
		    return(lastacttip2);
		}
	    }
	} catch(Loading ignored) {}
	return(null);
    }

    public void uimsg(String msg, Object... args) {
	switch (msg) {
	    case "act": {
		int n = (Integer) args[0];
		if (args.length > 1) {
		    Indir<Resource> res = ui.sess.getres((Integer) args[1]);
		    actions[n] = new Action(res, n);
		} else {
		    actions[n] = null;
		}
		break;
	    }
	    case "acool": {
		int n = (Integer) args[0];
		double now = Utils.rtime();
		actions[n].cs = now;
		actions[n].ct = now + (((Number) args[1]).doubleValue() * 0.06);
		break;
	    }
	    case "use":
		this.use = (Integer) args[0];
		this.useb = (args.length > 1) ? ((Integer) args[1]) : -1;
		break;
	    case "used":
		break;
	    default:
		super.uimsg(msg, args);
		break;
	}
    }

    /* XXX: This is a bit ugly, but release message do need to be
     * properly sequenced with use messages in some way. */
    private class Release implements Runnable {
	final int n;

	Release(int n) {
	    this.n = n;
	    Environment env = ui.getenv();
	    Render out = env.render();
	    out.fence(this);
	    env.submit(out);
	}


	public void run() {
	    wdgmsg("rel", n);
	}
    }

    private UI.Grab holdgrab = null;
    private int held = -1;

    // Spam prevention otherwise you can dc...
    private int last_button = -1;
    private long last_sent = System.currentTimeMillis();

    //Keybinds
    private final Map<KeyBind, KeyBind.Command> binds = new HashMap<>();
    private void setKeybinds() {
        for(var i = 0; i < KB_FIGHT_MOVE.length; ++i) {
	    final int slot = i;
	    binds.put(KB_FIGHT_MOVE[i], () -> { use(slot); return true; });
	}
        binds.put(KB_CYCLEUP_OPP, () -> {
	    Fightview.Relation cur = fv.current;
	    if(cur != null) {
		fv.lsrel.remove(cur);
		fv.lsrel.addLast(cur);
	    }
	    fv.wdgmsg("bump", (int)fv.lsrel.get(0).gobid);
	    return(true);
	});
        binds.put(KB_CYCLEDOWN_OPP, () -> {
	    Fightview.Relation last = fv.lsrel.getLast();
	    if(last != null) {
		fv.lsrel.remove(last);
		fv.lsrel.addFirst(last);
	    }
	    fv.wdgmsg("bump", (int)fv.lsrel.get(0).gobid);
	    return(true);
	});
        binds.put(KB_PEACE_CURRENT, () -> {
           if(fv.current != null) {
               fv.current.peace();
               return true;
	   } else {
               return false;
	   }
	});
        binds.put(KB_TARGET_CURRENT, () -> {
            if(fv.current != null) {
		final Gob old = ui.sess.glob.oc.getgob(ui.gui.curtar);
		if (old != null) {
		    final Gob.Overlay ol = old.findol(TargetSprite.id);
		    if (ol != null) {
			((TargetSprite) ol.spr).rem();
		    }
		}
		final Gob g = ui.sess.glob.oc.getgob(fv.current.gobid);
		ui.gui.curtar = fv.current.gobid;
		if(g != null)
		    g.queueDeltas(Collections.singletonList((gob) -> gob.addol(new Gob.Overlay(gob, TargetSprite.id, new TargetSprite(gob)))));
		if(ui.gui.chat.party != null)
		    ui.gui.chat.party.send(String.format(TargetSprite.target_pat, fv.current.gobid));
                return true;
	    } else {
                return false;
	    }
	});
    }

    public void use(final int fn) {
	if (last_button != fn || (System.currentTimeMillis() - last_sent) >= 100) {

	    MapView map = getparent(GameUI.class).map;
	    Coord mvc = map.rootxlate(ui.mc);
	    if(held >= 0) {
		new Release(held);
		held = -1;
	    }
	    if(mvc.isect(Coord.z, map.sz)) {
		map.new Maptest(mvc) {
		    protected void hit(Coord pc, Coord2d mc) {
			wdgmsg("use", fn, 1, ui.modflags(), mc.floor(OCache.posres));
		    }

		    protected void nohit(Coord pc) {
			wdgmsg("use", fn, 1, ui.modflags());
		    }
		}.run();
	    }
	    if(holdgrab == null)
		holdgrab = ui.grabkeys(this);
	    held = fn;

	    last_button = fn;
	    last_sent = System.currentTimeMillis();
	}
    }
    public boolean globtype(char key, KeyEvent ev) {
        final String bind = KeyBind.generateSequence(ev, ui);
	for(final var kb : binds.keySet()) {
	    if(kb.check(bind, binds.get(kb)))
		return true;
	}
	return(super.globtype(key, ev));
    }

    public boolean keydown(KeyEvent ev) {
	return(false);
    }

    public boolean keyup(KeyEvent ev) {
	final String bind = KeyBind.generateSequence(ev, ui);
	if((holdgrab != null) && (KB_FIGHT_MOVE[held].match(bind))) {
	    new Release(held);
	    holdgrab.remove();
	    holdgrab = null;
	    held = -1;
	    return(true);
	}
	return(false);
    }
}
