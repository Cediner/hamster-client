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

import com.google.common.flogger.FluentLogger;
import hamster.GlobalSettings;
import hamster.gob.sprites.Mark;
import hamster.gob.sprites.TargetSprite;
import hamster.ui.ChatUtils;

import java.util.*;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.KeyEvent;
import java.awt.font.TextAttribute;
import java.awt.font.TextHitInfo;
import java.awt.image.BufferedImage;
import java.text.*;
import java.text.AttributedCharacterIterator.Attribute;
import java.net.URL;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.regex.*;
import java.io.IOException;
import java.awt.datatransfer.*;

import static haven.MCache.tilesz;

//TODO: A bit of scripting changes are needed in here
public class ChatUI extends Widget {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    public static final RichText.Foundry fnd = new RichText.Foundry(new ChatParser(TextAttribute.FONT, Text.dfont.deriveFont(UI.scale(10f)), TextAttribute.FOREGROUND, Color.BLACK));
    public static final Text.Foundry qfnd = new Text.Foundry(Text.dfont, 12, new java.awt.Color(192, 255, 192));
    public static final int selw = UI.scale(130);
    public static final Coord marg = UI.scale(new Coord(9, 9));
    public static final int offset = UI.scale(28);
    public static final Color[] urgcols = new Color[] {
	null,
	new Color(0, 128, 255),
	new Color(255, 128, 0),
	new Color(255, 0, 0),
    };
    public Channel sel = null;
    public int urgency = 0;
    private final Selector chansel;
    private QuickLine qline = null;
    private final LinkedList<Notification> notifs = new LinkedList<Notification>();
    private UI.Grab qgrab;

    public EntryChannel area;
    public EntryChannel party;
    public EntryChannel village;
    public EntryChannel realm;
    private final List<EntryChannel> privchats = new ArrayList<>();

    public ChatUI(int w, int h) {
	super(new Coord(w, h));
	chansel = add(new Selector(new Coord(selw, sz.y - marg.y)), marg);
	setfocusctl(true);
	setcanfocus(true);
	if(h < 1)
	    hide();
    }

    protected void added() {
	resize(this.sz);
    }

    public void addPrivChat(final EntryChannel chan) {
	synchronized (privchats) {
	    privchats.add(chan);
	}
    }

    public void remPrivChat(final EntryChannel chan) {
	synchronized (privchats) {
	    privchats.remove(chan);
	}
    }

    public EntryChannel[] privchats() {
	synchronized (privchats) {
	    return privchats.toArray(new EntryChannel[0]);
	}
    }

    public static class ChatAttribute extends Attribute {
	private ChatAttribute(String name) {
	    super(name);
	}

	public static final Attribute HYPERLINK = new ChatAttribute("hyperlink");
    }
    
    public static class FuckMeGentlyWithAChainsaw {
	/* This wrapper class exists to work around the possibly most
	 * stupid Java bug ever (and that's saying a lot): That
	 * URL.equals and URL.hashCode do DNS lookups and
	 * block. Which, of course, not only sucks performance-wise
	 * but also breaks actual correct URL equality. */
	public final URL url;
	
	public FuckMeGentlyWithAChainsaw(URL url) {
	    this.url = url;
	}
    }

    public static class ChatParser extends RichText.Parser {
	public static final Pattern urlpat = Pattern.compile("\\b((https?://)|(www\\.[a-z0-9_.-]+\\.[a-z0-9_.-]+))[a-z0-9/_.~#%+?&:*=-]*", Pattern.CASE_INSENSITIVE);
	public static final Map<? extends Attribute, ?> urlstyle = RichText.fillattrs(TextAttribute.FOREGROUND, new Color(64, 64, 255),
										      TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
	
	public ChatParser(Object... args) {
	    super(args);
	}
	
	protected RichText.Part text(PState s, String text, Map<? extends Attribute, ?> attrs) throws IOException {
	    RichText.Part ret = null;
	    int p = 0;
	    while(true) {
		Matcher m = urlpat.matcher(text);
		if(!m.find(p))
		    break;
		URL url;
		try {
		    String su = text.substring(m.start(), m.end());
		    if(su.indexOf(':') < 0)
			su = "http://" + su;
		    url = new URL(su);
		} catch(java.net.MalformedURLException e) {
		    p = m.end();
		    continue;
		}
		RichText.Part lead = new RichText.TextPart(text.substring(p, m.start()), attrs);
		if(ret == null) ret = lead; else ret.append(lead);
		Map<Attribute, Object> na = new HashMap<Attribute, Object>(attrs);
		na.putAll(urlstyle);
		na.put(ChatAttribute.HYPERLINK, new FuckMeGentlyWithAChainsaw(url));
		ret.append(new RichText.TextPart(text.substring(m.start(), m.end()), na));
		p = m.end();
	    }
	    if(ret == null)
		ret = new RichText.TextPart(text, attrs);
	    else
		ret.append(new RichText.TextPart(text.substring(p), attrs));
	    return(ret);
	}
    }

    public static abstract class Channel extends Widget {
	public final List<Message> msgs = new LinkedList<Message>();
	private final Scrollbar sb;
	private final IButton cb;
	public int urgency = 0;
	
	public static abstract class Message {
	    public final double time = Utils.ntime();
	    
	    public abstract Text text();
	    public abstract Tex tex();
	    public abstract Coord sz();
	}
	
	public static class SimpleMessage extends Message {
	    private final Text t;
	    
	    public SimpleMessage(String text, Color col, int w) {
		if(col == null)
		    this.t = fnd.render(RichText.Parser.quote(text), w);
		else
		    this.t = fnd.render(RichText.Parser.quote(text), w, TextAttribute.FOREGROUND, col);
	    }
	    
	    public Text text() {
		return(t);
	    }

	    public Tex tex() {
		return(t.tex());
	    }
	    
	    public Coord sz() {
		return(t.sz());
	    }
	}

	public Channel(boolean closable) {
	    sb = add(new Scrollbar(0, 0, 0));
	    if(closable)
		cb = add(new IButton("gfx/hud/chat-close", "", "-d", "-h"));
	    else
		cb = null;
	}
	
	public void append(Message msg) {
	    synchronized(msgs) {
		msgs.add(msg);
		int y = 0;
		for(Message m : msgs)
		    y += m.sz().y;
		boolean b = sb.val >= sb.max;
		sb.max = y - ih();
		if(b)
		    sb.val = sb.max;
	    }
	}

	public void append(String line, Color col) {
	    append(new SimpleMessage(line, col, iw()));
	}
	
	public int iw() {
	    return(sz.x - sb.sz.x);
	}
	
	public int ih() {
	    return(sz.y);
	}
	
	public void updurgency(int urg) {
	    if(urgency != urg) {
		urgency = urg;
		int mu = 0;
		ChatUI p = getparent(ChatUI.class);
		for(Selector.DarkChannel ch : p.chansel.chls)
		    mu = Math.max(mu, ch.chan.urgency);
		p.urgency = mu;
	    }
	}

	public void draw(GOut g) {
	    g.chcolor(0, 0, 0, 128);
	    g.frect(Coord.z, sz);
	    g.chcolor();
	    int y = 0;
	    boolean sel = false;
	    synchronized(msgs) {
		for(Message msg : msgs) {
		    if((selstart != null) && (msg == selstart.msg))
			sel = true;
		    int y1 = y - sb.val;
		    int y2 = y1 + msg.sz().y;
		    if((y2 > 0) && (y1 < ih())) {
			if(sel)
			    drawsel(g, msg, y1);
			g.image(msg.tex(), new Coord(0, y1));
		    }
		    if((selend != null) && (msg == selend.msg))
			sel = false;
		    y += msg.sz().y;
		}
	    }
	    sb.max = y - ih();
	    super.draw(g);
	    updurgency(0);
	}
	
	public boolean mousewheel(Coord c, int amount) {
	    sb.ch(amount * 15);
	    return(true);
	}
	
	public void resize(Coord sz) {
	    super.resize(sz);
	    if(sb != null) {
		sb.move(new Coord(sz.x - (UI.scale(12) - marg.x), UI.scale(34) - marg.y));
		sb.resize(ih() - sb.c.y);
		int y = 0;
		for(Message m : msgs)
		    y += m.sz().y;
		boolean b = sb.val >= sb.max;
		sb.max = y - ih();
		if(b)
		    sb.val = sb.max;
	    }
	    if(cb != null) {
		cb.c = new Coord(sz.x + marg.x - cb.sz.x, -marg.y);
	    }
	}
	
	public void notify(Message msg, int urgency) {
	    getparent(ChatUI.class).notify(this, msg);
	    updurgency(Math.max(this.urgency, urgency));
	}
	
	public static class CharPos {
	    public final Message msg;
	    public final RichText.TextPart part;
	    public final TextHitInfo ch;
	    
	    public CharPos(Message msg, RichText.TextPart part, TextHitInfo ch) {
		this.msg = msg;
		this.part = part;
		this.ch = ch;
	    }
	    
	    public boolean equals(Object oo) {
		if(!(oo instanceof CharPos)) return(false);
		CharPos o = (CharPos)oo;
		return((o.msg == this.msg) && (o.part == this.part) && o.ch.equals(this.ch));
	    }
	}

	public final Comparator<CharPos> poscmp = new Comparator<CharPos>() {
	    public int compare(CharPos a, CharPos b) {
		if(a.msg != b.msg) {
		    synchronized(msgs) {
			for(Message msg : msgs) {
			    if(msg == a.msg)
				return(-1);
			    else if(msg == b.msg)
				return(1);
			}
		    }
		    throw(new IllegalStateException("CharPos message is no longer contained in the log"));
		} else if(a.part != b.part) {
		    for(RichText.Part part = ((RichText)a.msg.text()).parts; part != null; part = part.next) {
			if(part == a.part)
			    return(-1);
			else
			    return(1);
		    }
		    throw(new IllegalStateException("CharPos is no longer contained in the log"));
		} else {
		    return(a.ch.getInsertionIndex() - b.ch.getInsertionIndex());
		}
	    }
	};

	public Message messageat(Coord c, Coord hc) {
	    int y = -sb.val;
	    synchronized(msgs) {
		for(Message msg : msgs) {
		    Coord sz = msg.sz();
		    if((c.y >= y) && (c.y < y + sz.y)) {
			if(hc != null) {
			    hc.x = c.x;
			    hc.y = c.y - y;
			}
			return(msg);
		    }
		    y += sz.y;
		}
	    }
	    return(null);
	}
	
	public CharPos charat(Coord c) {
	    if(c.y < -sb.val) {
		if(msgs.size() < 1)
		    return(null);
		Message msg = msgs.get(0);
		if(!(msg.text() instanceof RichText))
		    return(null);
		RichText.TextPart fp = null;
		for(RichText.Part part = ((RichText)msg.text()).parts; part != null; part = part.next) {
		    if(part instanceof RichText.TextPart) {
			fp = (RichText.TextPart)part;
			break;
		    }
		}
		if(fp == null)
		    return(null);
		return(new CharPos(msg, fp, TextHitInfo.leading(0)));
	    }

	    Coord hc = new Coord();
	    Message msg = messageat(c, hc);
	    if((msg == null) || !(msg.text() instanceof RichText))
		return(null);
	    RichText rt = (RichText)msg.text();
	    RichText.Part p = rt.partat(hc);
	    if(p == null) {
		RichText.TextPart lp = null;
		for(RichText.Part part = ((RichText)msg.text()).parts; part != null; part = part.next) {
		    if(part instanceof RichText.TextPart)
			lp = (RichText.TextPart)part;
		}
		if(lp == null) return(null);
		return(new CharPos(msg, lp, TextHitInfo.trailing(lp.end - lp.start - 1)));
	    }
	    if(!(p instanceof RichText.TextPart))
		return(null);
	    RichText.TextPart tp = (RichText.TextPart)p;
	    return(new CharPos(msg, tp, tp.charat(hc)));
	}

	private CharPos selorig, lasthit, selstart, selend;
	private UI.Grab grab;
	private boolean dragging;
	public boolean mousedown(Coord c, int btn) {
	    if(super.mousedown(c, btn))
		return(true);
	    if(btn == 1) {
		selstart = selend = null;
		CharPos ch = charat(c);
		if(ch != null) {
		    selorig = lasthit = ch;
		    dragging = false;
		    grab = ui.grabmouse(this);
		}
		return(true);
	    }
	    return(false);
	}
	
	public void mousemove(Coord c) {
	    if(selorig != null) {
		CharPos ch = charat(c);
		if((ch != null) && !ch.equals(lasthit)) {
		    lasthit = ch;
		    if(!dragging && !ch.equals(selorig))
			dragging = true;
		    int o = poscmp.compare(selorig, ch);
		    if(o < 0) {
			selstart = selorig; selend = ch;
		    } else if(o > 0) {
			selstart = ch; selend = selorig;
		    } else {
			selstart = selend = null;
		    }
		}
	    } else {
		super.mousemove(c);
	    }
	}
	
	protected void selected(CharPos start, CharPos end) {
	    StringBuilder buf = new StringBuilder();
	    synchronized(msgs) {
		boolean sel = false;
		for(Message msg : msgs) {
		    if(!(msg.text() instanceof RichText))
			continue;
		    RichText rt = (RichText)msg.text();
		    RichText.Part part = null;
		    if(sel) {
			part = rt.parts;
		    } else if(msg == start.msg) {
			sel = true;
			for(part = rt.parts; part != null; part = part.next) {
			    if(part == start.part)
				break;
			}
		    }
		    if(sel) {
			for(; part != null; part = part.next) {
			    if(!(part instanceof RichText.TextPart))
				continue;
			    RichText.TextPart tp = (RichText.TextPart)part;
			    CharacterIterator iter = tp.ti();
			    int sch;
			    if(tp == start.part)
				sch = tp.start + start.ch.getInsertionIndex();
			    else
				sch = tp.start;
			    int ech;
			    if(tp == end.part)
				ech = tp.start + end.ch.getInsertionIndex();
			    else
				ech = tp.end;
			    for(int i = sch; i < ech; i++)
				buf.append(iter.setIndex(i));
			    if(part == end.part) {
				sel = false;
				break;
			    }
			    buf.append(' ');
			}
			if(sel)
			    buf.append('\n');
		    }
		    if(msg == end.msg)
			break;
		}
	    }
	    Clipboard cl;
	    if((cl = java.awt.Toolkit.getDefaultToolkit().getSystemSelection()) == null)
		cl = java.awt.Toolkit.getDefaultToolkit().getSystemClipboard();
	    try {
		final CharPos ownsel = selstart;
		cl.setContents(new StringSelection(buf.toString()),
			       new ClipboardOwner() {
			public void lostOwnership(Clipboard cl, Transferable tr) {
			    if(selstart == ownsel)
				selstart = selend = null;
			}
		    });
	    } catch(IllegalStateException e) {}
	}

	protected void clicked(CharPos pos) {
	    AttributedCharacterIterator inf = pos.part.ti();
	    inf.setIndex(pos.ch.getCharIndex() + pos.part.start);
	    FuckMeGentlyWithAChainsaw url = (FuckMeGentlyWithAChainsaw)inf.getAttribute(ChatAttribute.HYPERLINK);
	    if((url != null) && (WebBrowser.self != null)) {
		try {
		    WebBrowser.self.show(url.url);
		} catch(WebBrowser.BrowserException e) {
		    getparent(GameUI.class).error("Could not launch web browser.");
		}
	    }
	}

	public boolean mouseup(Coord c, int btn) {
	    if(btn == 1) {
		if(selorig != null) {
		    if(selstart != null)
			selected(selstart, selend);
		    else
			clicked(selorig);
		    grab.remove();
		    selorig = null;
		    dragging = false;
		}
	    }
	    return(super.mouseup(c, btn));
	}

	public void select() {
	    getparent(ChatUI.class).select(this);
	}

	public void display() {
	    select();
	    ChatUI chat = getparent(ChatUI.class);
	    chat.parent.setfocus(chat);
	}

	private void drawsel(GOut g, Message msg, int y) {
	    RichText rt = (RichText)msg.text();
	    boolean sel = msg != selstart.msg;
	    for(RichText.Part part = rt.parts; part != null; part = part.next) {
		if(!(part instanceof RichText.TextPart))
		    continue;
		RichText.TextPart tp = (RichText.TextPart)part;
		if(tp.start == tp.end)
		    continue;
		TextHitInfo a, b;
		if(sel) {
		    a = TextHitInfo.leading(0);
		} else if(tp == selstart.part) {
		    a = selstart.ch;
		    sel = true;
		} else {
		    continue;
		}
		if(tp == selend.part) {
		    sel = false;
		    b = selend.ch;
		} else {
		    b = TextHitInfo.trailing(tp.end - tp.start - 1);
		}
		Coord ul = new Coord(tp.x + (int)tp.advance(0, a.getInsertionIndex()), tp.y + y);
		Coord sz = new Coord((int)tp.advance(a.getInsertionIndex(), b.getInsertionIndex()), tp.height());
		g.chcolor(0, 0, 255, 255);
		g.frect(ul, sz);
		g.chcolor();
		if(!sel)
		    break;
	    }
	}

	public void uimsg(String name, Object... args) {
	    if(name == "sel") {
		select();
	    } else if(name == "dsp") {
		display();
	    } else {
		super.uimsg(name, args);
	    }
	}

	public void wdgmsg(Widget sender, String msg, Object... args) {
	    if(sender == cb) {
		wdgmsg("close");
	    } else {
		super.wdgmsg(sender, msg, args);
	    }
	}

	public abstract String name();
    }
    
    public static class Log extends Channel {
	private final String name;
	
	public Log(String name) {
	    super(false);
	    this.name = name;
	}
	
	public String name() {return(name);}

	@Override
	public void append(Message msg) {
	    super.append(msg);
	    if(name.equals("System"))
	    	ui.sess.details.context.dispatchmsg(this, "sys", msg.text().text);
	}
    }
    
    public static abstract class EntryChannel extends Channel {
	private final TextEntry in;
	private List<String> history = new ArrayList<String>();
	private int hpos = 0;
	private String hcurrent;
	
	public EntryChannel(boolean closable) {
	    super(closable);
	    setfocusctl(true);
	    this.in = new TextEntry(0, "") {
		    public void activate(String text) {
			if(text.length() > 0)
			    send(text);
			settext("");
			hpos = history.size();
		    }

		    public boolean keydown(KeyEvent ev) {
			if(ev.getKeyCode() == KeyEvent.VK_UP) {
			    if(hpos > 0) {
				if(hpos == history.size())
				    hcurrent = text;
				rsettext(history.get(--hpos));
			    }
			    return(true);
			} else if(ev.getKeyCode() == KeyEvent.VK_DOWN) {
			    if(hpos < history.size()) {
				if(++hpos == history.size())
				    rsettext(hcurrent);
				else
				    rsettext(history.get(hpos));
			    }
			    return(true);
			} else {
			    return(super.keydown(ev));
			}
		    }
		};
	    add(this.in);
	}
	
	public int ih() {
	    return(sz.y - in.sz.y);
	}
	
	public void resize(Coord sz) {
	    super.resize(sz);
	    if(in != null) {
		in.c = new Coord(0, this.sz.y - in.sz.y);
		in.resize(this.sz.x);
		in.redraw();
	    }
	}
	
	public void send(String text) {
	    history.add(text);
	    wdgmsg("msg", text);
	}
    }
    
    public static class SimpleChat extends EntryChannel {
	public final String name;

	public SimpleChat(boolean closable, String name) {
	    super(closable);
	    this.name = name;
	}

	public void uimsg(String msg, Object... args) {
	    if((msg.equals("msg")) || (msg.equals("log"))) {
		String line = (String)args[0];
		Color col = null;
		if(args.length > 1) col = (Color)args[1];
		if(col == null) col = Color.WHITE;
		int urgency = (args.length > 2)?(Integer)args[2]:0;
		Message cmsg = new SimpleMessage(line, col, iw());
		append(cmsg);
		if(urgency > 0)
		    notify(cmsg, urgency);
	    } else {
		super.uimsg(msg, args);
	    }
	}

	public String name() {
	    return(name);
	}
    }

    /**
     * Chat between player and scripts only
     */
    public static class BotChat extends SimpleChat {
	private final Map<Pattern, Consumer<Matcher>> chat_ext_mapping = new HashMap<>();
	public BotChat() {
	    super(false, "Bot");

	    chat_ext_mapping.put(Mark.CHAT_FMT_PAT, (match) -> {
		final long gid = Long.parseLong(match.group(1));
		final int life = Integer.parseInt(match.group(2));
		final Gob g = ui.sess.glob.oc.getgob(gid);
		if (g != null) {
		    g.mark(life);
		}
	    });
	    chat_ext_mapping.put(Mark.CHAT_TILE_FMT_PAT, (match) -> {
		final long gid = Long.parseLong(match.group(1));
		final double offx = Double.parseDouble(match.group(2));
		final double offy = Double.parseDouble(match.group(3));
		ui.sess.glob.map.getgrido(gid).ifPresent(grid -> {
		    final Coord2d mc = new Coord2d(grid.ul).add(offx, offy).mul(tilesz);
		    ui.sess.glob.loader.defer(() -> {
			final Gob g = ui.sess.glob.oc.new ModdedGob(mc, 0);
			g.addol(new Gob.Overlay(g, Mark.id, new Mark(2000)));
			ui.sess.glob.oc.add(g);
		    }, null);
		});
	    });
	    chat_ext_mapping.put(TargetSprite.TARGET_PATTERN, (match) -> {
		final long gid = Long.parseLong(match.group(1));
		final Gob old = ui.sess.glob.oc.getgob(ui.gui.curtar);
		if (old != null) {
		    final Gob.Overlay ol = old.findol(TargetSprite.id);
		    if (ol != null) {
			((TargetSprite) ol.spr).rem();
		    }
		}

		ui.gui.curtar = gid;
		final Gob g = ui.sess.glob.oc.getgob(gid);
		if (g != null)
		    g.queueDeltas(Collections.singletonList((gob) -> gob.addol(new Gob.Overlay(gob, TargetSprite.id, new TargetSprite(gob)))));
	    });
	}

	public void uimsg(String msg, Object... args) {
	    super.uimsg(msg, args);
	    if(msg.equals("msg") && args[0] instanceof String) {
		String line = (String)args[0];

		try { // Handle any extensions from party chat that we can parse out
		    for(final var pat : chat_ext_mapping.keySet()) {
			final var match = pat.matcher(line);
			if(match.find()) {
			    chat_ext_mapping.get(pat).accept(match);
			    return;
			}
		    }
		} catch (Exception e) {
		    logger.atWarning().withCause(e).log("Failed to parse custom botchat msg");
		}
	    }
	}

	@Override
	public void send(String text) {
	    ui.sess.details.context.dispatchmsg(this, "msg", text);
	    uimsg("msg", text);
	}
    }


    public static class MultiChat extends EntryChannel {
	public final int urgency;
	private final String name;
	private final Map<Integer, Color> pc = new HashMap<Integer, Color>();
	
	public class NamedMessage extends Message {
	    public final int from;
	    public final String text;
	    public final int w;
	    public final Color col;
	    private String cn;
	    private Text r = null;
	    
	    public NamedMessage(int from, String text, Color col, int w) {
		this.from = from;
		this.text = text;
		this.w = w;
		this.col = col;
	    }

	    public Text text() {
		BuddyWnd.Buddy b = getparent(GameUI.class).buddies.find(from);
		String nm = (b == null)?"???":(b.name);
		if((r == null) || !nm.equals(cn)) {
		    r = fnd.render(RichText.Parser.quote(String.format("%s: %s", nm, text)), w, TextAttribute.FOREGROUND, col);
		    cn = nm;
		}
		return(r);
	    }

	    public Tex tex() {
		return(text().tex());
	    }

	    public Coord sz() {
		if(r == null)
		    return(text().sz());
		else
		    return(r.sz());
	    }
	}

	public class MyMessage extends SimpleMessage {
	    public MyMessage(String text, int w) {
		super(text, new Color(192, 192, 255), w);
	    }
	}

	public MultiChat(boolean closable, String name, int urgency) {
	    super(closable);
	    this.name = name;
	    this.urgency = urgency;
	}

	@Override
	protected void added() {
	    super.added();
	    if (name.equals("Area Chat")) {
		ui.gui.chat.area = this;
	    } else if (name.endsWith("(P)") && urgency == 0) {
		ui.gui.chat.realm = this;
	    } else {
		ui.gui.chat.village = this;
	    }
	}
	
	private float colseq = 0;
	private Color nextcol() {
	    return(new Color(Color.HSBtoRGB(colseq = ((colseq + (float)Math.sqrt(2)) % 1.0f), 0.5f, 1.0f)));
	}

	public Color fromcolor(int from) {
	    synchronized(pc) {
		Color c = pc.get(from);
		if(c == null)
		    pc.put(from, c = nextcol());
		return(c);
	    }
	}

	public void uimsg(String msg, Object... args) {
	    if(msg == "msg") {
		Integer from = (Integer)args[0];
		String line = (String)args[1];
		final String subject;
		if (this == ui.gui.chat.area)
		    subject = "area-msg";
		else if (this == ui.gui.chat.realm)
		    subject = "realm-msg";
		else
		    subject = "village-msg";
		if(from == null) {
		    append(new MyMessage(line, iw()));
		    ui.sess.details.context.dispatchmsg(this, subject, line, ui.sess.details.chrname());
		} else {
		    BuddyWnd.Buddy b = getparent(GameUI.class).buddies.find(from);
		    String nm = (b == null) ? "???" : (b.name);
		    ui.sess.details.context.dispatchmsg(this, subject, line, nm);

		    Message cmsg = new NamedMessage(from, line, fromcolor(from), iw());
		    append(cmsg);
		    if(urgency > 0)
			notify(cmsg, urgency);
		}
	    } else {
		super.uimsg(msg, args);
	    }
	}
	
	public String name() {
	    return(name);
	}
    }
    
    public static class PartyChat extends MultiChat {
        private final Map<Pattern, BiConsumer<Matcher, Long>> chat_ext_mapping = new HashMap<>();

	public PartyChat() {
	    super(false, "Party", 2);

	    chat_ext_mapping.put(Mark.CHAT_FMT_PAT, (match, senderid) -> {
		final long gid = Long.parseLong(match.group(1));
		final int life = Integer.parseInt(match.group(2));
		final Gob g = ui.sess.glob.oc.getgob(gid);
		if (g != null) {
		    g.mark(life);
		}
	    });
	    chat_ext_mapping.put(Mark.CHAT_TILE_FMT_PAT, (match, senderid) -> {
		final long gid = Long.parseLong(match.group(1));
		final double offx = Double.parseDouble(match.group(2));
		final double offy = Double.parseDouble(match.group(3));
		ui.sess.glob.map.getgrido(gid).ifPresent(grid -> {
		    final Coord2d mc = new Coord2d(grid.ul).add(offx, offy).mul(tilesz);
		    ui.sess.glob.loader.defer(() -> {
			final Gob g = ui.sess.glob.oc.new ModdedGob(mc, 0);
			g.addol(new Gob.Overlay(g, Mark.id, new Mark(2000)));
			ui.sess.glob.oc.add(g);
		    }, null);
		});
	    });
	    chat_ext_mapping.put(TargetSprite.TARGET_PATTERN, (match, senderid) -> {
		final long gid = Long.parseLong(match.group(1));
		final Gob old = ui.sess.glob.oc.getgob(ui.gui.curtar);
		if (old != null) {
		    final Gob.Overlay ol = old.findol(TargetSprite.id);
		    if (ol != null) {
			((TargetSprite) ol.spr).rem();
		    }
		}

		ui.gui.curtar = gid;
		final Gob g = ui.sess.glob.oc.getgob(gid);
		if (g != null)
		    g.queueDeltas(Collections.singletonList((gob) -> gob.addol(new Gob.Overlay(gob, TargetSprite.id, new TargetSprite(gob)))));
	    });
	    chat_ext_mapping.put(ChatUtils.CHAT_SEXT_MSG_PAT, (match, senderid) -> {
		final long targetid = Long.parseLong(match.group(1));
		if (ui.gui.map.plgob == targetid && ui.gui.map.ext.isMaster(senderid)) {
		    final String subject = match.group(2);
		    final String dargs = match.group(3);
		    ChatUtils.parseExternalCommand(ui, true, this, subject, dargs);
		}
	    });
	    chat_ext_mapping.put(ChatUtils.CHAT_EXT_MSG_PAT, (match, senderid) -> {
		final String subject = match.group(1);
		final String dargs = match.group(2);
		ChatUtils.parseExternalCommand(ui, false, this, subject, dargs);
	    });
	}

	@Override
	protected void added() {
	    super.added();
	    ui.gui.chat.party = this;
	}


	public void uimsg(String msg, Object... args) {
	    if(msg == "msg") {
		Integer from = (Integer)args[0];
		long gobid = Utils.uint32((Integer)args[1]);
		String line = (String)args[2];
		Color col = Color.WHITE;

		try { // Handle any extensions from party chat that we can parse out
		    for(final var pat : chat_ext_mapping.keySet()) {
		        final var match = pat.matcher(line);
		        if(match.find()) {
		            chat_ext_mapping.get(pat).accept(match, gobid);
		            return;
			}
		    }
		} catch (Exception e) {
		    logger.atWarning().withCause(e);
		}

		synchronized(ui.sess.glob.party.memb) {
		    Party.Member pm = ui.sess.glob.party.memb.get(gobid);
		    if(pm != null)
			col = pm.col;
		}
		if(from == null) {
		    append(new MyMessage(line, iw()));
		} else {
		    BuddyWnd.Buddy b = getparent(GameUI.class).buddies.find(from);
		    String nm = (b == null) ? "???" : (b.name);
		    ui.sess.details.context.dispatchmsg(this, "pt-msg", line, nm);

		    Message cmsg = new NamedMessage(from, line, Utils.blendcol(col, Color.WHITE, 0.5), iw());
		    append(cmsg);
		    if(urgency > 0)
			notify(cmsg, urgency);
		}
	    } else {
		super.uimsg(msg, args);
	    }
	}
    }
    
    public static class PrivChat extends EntryChannel {
	private final int other;
	
	public class InMessage extends SimpleMessage {
	    public InMessage(String text, int w) {
		super(text, new Color(255, 128, 128, 255), w);
	    }
	}

	public class OutMessage extends SimpleMessage {
	    public OutMessage(String text, int w) {
		super(text, new Color(128, 128, 255, 255), w);
	    }
	}

	public PrivChat(boolean closable, int other) {
	    super(closable);
	    this.other = other;
	}

	@Override
	protected void added() {
	    super.added();
	    ui.gui.chat.addPrivChat(this);
	}

	@Override
	public void dispose() {
	    super.dispose();
	    ui.gui.chat.remPrivChat(this);
	}

	public void uimsg(String msg, Object... args) {
	    if(msg == "msg") {
		String t = (String)args[0];
		String line = (String)args[1];
		if(t.equals("in")) {
		    try {
			final Matcher dmatch = ChatUtils.CHAT_EXT_MSG_PAT.matcher(line);
			if (dmatch.find()) {
			    final String subject = dmatch.group(1);
			    final String dargs = dmatch.group(2);
			    ChatUtils.parseExternalCommand(ui, false, this, subject, dargs);
			    return;
			}
		    } catch (Exception e) {
		        logger.atWarning().withCause(e);
		    }
		    ui.sess.details.context.dispatchmsg(this, "priv-in-msg", line, name());
		    Message cmsg = new InMessage(line, iw());
		    append(cmsg);
		    notify(cmsg, 3);
		} else if(t.equals("out")) {
		    ui.sess.details.context.dispatchmsg(this, "priv-out-msg", line);
		    append(new OutMessage(line, iw()));
		}
	    } else if(msg == "err") {
		String err = (String)args[0];
		Message cmsg = new SimpleMessage(err, Color.RED, iw());
		append(cmsg);
		notify(cmsg, 3);
	    } else {
		super.uimsg(msg, args);
	    }
	}
	
	public String name() {
	    BuddyWnd.Buddy b = getparent(GameUI.class).buddies.find(other);
	    if(b == null)
		return("???");
	    else
		return(b.name);
	}
    }
    
    @RName("schan")
    public static class $SChan implements Factory {
	public Widget create(UI ui, Object[] args) {
	    String name = (String)args[0];
	    return(new SimpleChat(false, name));
	}
    }
    @RName("mchat")
    public static class $MChat implements Factory {
	public Widget create(UI ui, Object[] args) {
	    String name = (String)args[0];
	    int urgency = (Integer)args[1];
	    return(new MultiChat(false, name, urgency));
	}
    }
    @RName("pchat")
    public static class $PChat implements Factory {
	public Widget create(UI ui, Object[] args) {
	    return(new PartyChat());
	}
    }
    @RName("pmchat")
    public static class $PMChat implements Factory {
	public Widget create(UI ui, Object[] args) {
	    int other = (Integer)args[0];
	    return(new PrivChat(true, other));
	}
    }

    public void addchild(Widget child, Object... args) {
	add(child);
    }

    public <T extends Widget> T add(T w) {
	if(w instanceof Channel) {
	    Channel chan = (Channel)w;
	    chan.c = chansel.c.add(chansel.sz.x, 0);
	    chan.resize(sz.x - marg.x - chan.c.x, sz.y - chan.c.y);
	    super.add(w);
	    select(chan, false);
	    chansel.add(chan);
	    return(w);
	} else {
	    return(super.add(w));
	}
    }

    public void cdestroy(Widget w) {
	if(w instanceof Channel) {
	    Channel chan = (Channel)w;
	    if(chan == sel)
		sel = null;
	    chansel.rm(chan);
	}
    }
    
    private static final Tex chandiv = Resource.loadtex("gfx/hud/chat-cdiv");
    private static final Tex chanseld = Resource.loadtex("gfx/hud/chat-csel");
    private class Selector extends Widget {
	public final BufferedImage ctex = Resource.loadimg("gfx/hud/chantex");
	public final Text.Foundry tf = new Text.Foundry(Text.serif.deriveFont(Font.BOLD, UI.scale(12))).aa(true);
	public final Text.Furnace[] nf = {
	    new PUtils.BlurFurn(new PUtils.TexFurn(tf, ctex), 1, 1, new Color(80, 40, 0)),
	    new PUtils.BlurFurn(new PUtils.TexFurn(tf, ctex), 1, 1, new Color(0, 128, 255)),
	    new PUtils.BlurFurn(new PUtils.TexFurn(tf, ctex), 1, 1, new Color(255, 128, 0)),
	    new PUtils.BlurFurn(new PUtils.TexFurn(tf, ctex), 1, 1, new Color(255, 0, 0)),
	};
	private final List<DarkChannel> chls = new ArrayList<DarkChannel>();
	private int s = 0;
	
	private class DarkChannel {
	    public final Channel chan;
	    public Text rname;
	    private int urgency = 0;
	    
	    private DarkChannel(Channel chan) {
		this.chan = chan;
	    }
	}
	
	public Selector(Coord sz) {
	    super(sz);
	}
	
	private void add(Channel chan) {
	    synchronized(chls) {
		chls.add(new DarkChannel(chan));
	    }
	}
	
	private void rm(Channel chan) {
	    synchronized(chls) {
		for(Iterator<DarkChannel> i = chls.iterator(); i.hasNext();) {
		    DarkChannel c = i.next();
		    if(c.chan == chan)
			i.remove();
		}
	    }
	}
	
	public void draw(GOut g) {
	    int i = s;
	    int y = 0;
	    synchronized(chls) {
		while(i < chls.size()) {
		    DarkChannel ch = chls.get(i);
		    if(ch.chan == sel)
			g.image(chanseld, new Coord(0, y));
		    g.chcolor(255, 255, 255, 255);
		    if((ch.rname == null) || !ch.rname.text.equals(ch.chan.name()) || (ch.urgency != ch.chan.urgency))
			ch.rname = nf[ch.urgency = ch.chan.urgency].render(ch.chan.name());
		    g.aimage(ch.rname.tex(), new Coord(sz.x / 2, y + UI.scale(8)), 0.5, 0.5);
		    g.image(chandiv, new Coord(0, y + UI.scale(18)));
		    y += offset;
		    if(y >= sz.y)
			break;
		    i++;
		}
	    }
	    g.chcolor();
	}
	
	public boolean up() {
	    Channel prev = null;
	    for(DarkChannel ch : chls) {
		if(ch.chan == sel) {
		    if(prev != null) {
			select(prev);
			return(true);
		    } else {
			return(false);
		    }
		}
		prev = ch.chan;
	    }
	    return(false);
	}
	
	public boolean down() {
	    for(Iterator<DarkChannel> i = chls.iterator(); i.hasNext();) {
		DarkChannel ch = i.next();
		if(ch.chan == sel) {
		    if(i.hasNext()) {
			select(i.next().chan);
			return(true);
		    } else {
			return(false);
		    }
		}
	    }
	    return(false);
	}
	
	private Channel bypos(Coord c) {
	    int i = (c.y / offset) + s;
	    if((i >= 0) && (i < chls.size()))
		return(chls.get(i).chan);
	    return(null);
	}

	public boolean mousedown(Coord c, int button) {
	    if(button == 1) {
		Channel chan = bypos(c);
		if(chan != null)
		    select(chan);
	    }
	    return(true);
	}
	
	public boolean mousewheel(Coord c, int amount) {
	    if(!ui.modshift) {
		s += amount;
		if(s >= chls.size() - (sz.y / offset))
		    s = chls.size() - (sz.y / offset);
		if(s < 0)
		    s = 0;
	    } else {
		if(amount < 0)
		    up();
		else if(amount > 0)
		    down();
	    }
	    return(true);
	}
    }
    
    public void select(Channel chan, boolean focus) {
	Channel prev = sel;
	sel = chan;
	if(prev != null)
	    prev.hide();
	sel.show();
	resize(sz);
	if(focus || hasfocus)
	    setfocus(chan);
    }

    public void select(Channel chan) {
	select(chan, true);
    }

    private class Notification {
	public final Channel chan;
	public final Text chnm;
	public final Channel.Message msg;
	public final double time = Utils.ntime();
	
	private Notification(Channel chan, Channel.Message msg) {
	    this.chan = chan;
	    this.msg = msg;
	    this.chnm = chansel.nf[0].render(chan.name());
	}
    }

    private Text.Line rqline = null;
    private int rqpre;
    public void drawsmall(GOut g, Coord br, int h) {
	Coord c;
	if(qline != null) {
	    if((rqline == null) || !rqline.text.equals(qline.line)) {
		String pre = String.format("%s> ", qline.chan.name());
		rqline = qfnd.render(pre + qline.line);
		rqpre = pre.length();
	    }
	    c = br.sub(UI.scale(0, 20));
	    g.image(rqline.tex(), c);
	    int lx = rqline.advance(qline.point + rqpre) + UI.scale(1);
	    g.line(new Coord(br.x + lx, br.y - UI.scale(18)), new Coord(br.x + lx, br.y - UI.scale(6)), 1);
	} else {
	    c = br.sub(UI.scale(0, 5));
	}
	double now = Utils.ntime();
	synchronized(notifs) {
	    for(Iterator<Notification> i = notifs.iterator(); i.hasNext();) {
		Notification n = i.next();
		if(now - n.time > 5.0) {
		    i.remove();
		    continue;
		}
		if((c.y -= n.msg.sz().y) < br.y - h)
		    break;
		g.image(n.chnm.tex(), c, br.sub(0, h), br.add(selw - UI.scale(10), 0));
		g.image(n.msg.tex(), c.add(selw, 0));
	    }
	}
    }

    private static final Resource notifsfx = Resource.local().loadwait("sfx/hud/chat");
    public void notify(Channel chan, Channel.Message msg) {
	synchronized(notifs) {
	    notifs.addFirst(new Notification(chan, msg));
	}
	if(GlobalSettings.ALLOWCHATSOUND.get())
	    ui.sfx(notifsfx);
    }

    private class Spring extends NormAnim {
	final int oh = sz.y, nh;
	Spring(int nh) {
	    super(0.15);
	    this.nh = nh;
	    show();
	}

	public void ntick(double a) {
	    double b = Math.cos(Math.PI * 2.5 * a) * Math.exp(-5 * a);
	    resize(sz.x, nh + (int)((nh - oh) * b));
	    if((a == 1.0) && (nh == 0)) {
		hide();
	    }
	}
    }

    public void resize(Coord sz) {
	super.resize(sz);
	chansel.resize(new Coord(selw, this.sz.y - marg.y));
	if(sel != null)
	    sel.resize(new Coord(this.sz.x - marg.x - sel.c.x, this.sz.y - sel.c.y));
    }

    public int targeth = sz.y;
    public void sresize(int h) {
	clearanims(Spring.class);
	new Spring(targeth = h);
    }

    public void hresize(int h) {
	clearanims(Spring.class);
	resize(sz.x, targeth = h);
    }

    public void resize(int w) {
	resize(new Coord(Math.max(w, selw + marg.x + UI.scale(10) + marg.x), sz.y));
    }
    
    public void move(Coord base) {
	this.c = base;
    }

    private class QuickLine extends LineEdit {
	public final EntryChannel chan;
	
	private QuickLine(EntryChannel chan) {
	    this.chan = chan;
	}
	
	private void cancel() {
	    qline = null;
	    qgrab.remove();
	}
	
	protected void done(String line) {
	    if(line.length() > 0)
		chan.send(line);
	    cancel();
	}

	public boolean key(char c, int code, int mod) {
	    if(c == 27) {
		cancel();
	    } else {
		return(super.key(c, code, mod));
	    }
	    return(true);
	}
    }

    public boolean keydown(KeyEvent ev) {
	boolean M = (ev.getModifiersEx() & (KeyEvent.META_DOWN_MASK | KeyEvent.ALT_DOWN_MASK)) != 0;
	if(qline != null) {
	    if(M && (ev.getKeyCode() == KeyEvent.VK_UP)) {
		Channel prev = this.sel;
		while(chansel.up()) {
		    if(this.sel instanceof EntryChannel)
			break;
		}
		if(!(this.sel instanceof EntryChannel)) {
		    select(prev);
		    return(true);
		}
		qline = new QuickLine((EntryChannel)sel);
		return(true);
	    } else if(M && (ev.getKeyCode() == KeyEvent.VK_DOWN)) {
		Channel prev = this.sel;
		while(chansel.down()) {
		    if(this.sel instanceof EntryChannel)
			break;
		}
		if(!(this.sel instanceof EntryChannel)) {
		    select(prev);
		    return(true);
		}
		qline = new QuickLine((EntryChannel)sel);
		return(true);
	    }
	    qline.key(ev);
	    return(true);
	} else {
	    if(M && (ev.getKeyCode() == KeyEvent.VK_UP)) {
		chansel.up();
		return(true);
	    } else if(M && (ev.getKeyCode() == KeyEvent.VK_DOWN)) {
		chansel.down();
		return(true);
	    }
	    return(super.keydown(ev));
	}
    }
}
