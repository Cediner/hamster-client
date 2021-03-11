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

import java.util.*;
import java.awt.Toolkit;
import java.awt.Robot;
import java.awt.Point;
import java.util.concurrent.atomic.AtomicReference;

import com.google.common.flogger.FluentLogger;
import com.jogamp.opengl.*;
import com.jogamp.opengl.awt.*;
import hamster.GlobalSettings;
import hamster.util.MessageBus;
import hamster.util.ObservableCollection;
import hamster.util.ObservableListener;
import haven.render.*;
import haven.render.States;
import haven.render.gl.*;

public class JOGLPanel extends GLCanvas implements Runnable, UIPanel, Console.Directory, UI.Context {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    /* MultiSession */
    //All of our UIs
    private final ObservableCollection<UI> sessions = new ObservableCollection<>(new ArrayList<>());
    //The UI for the next frame, or null if no change
    private final AtomicReference<UI> nextUI = new AtomicReference<>(null);
    //The current active UI
    private final AtomicReference<UI> ui = new AtomicReference<>(null);
    /*  */

    /* MultiSession Mailsystem */
    public static class RemoveUIMessage extends MessageBus.Message {
	final UI lui;

	public RemoveUIMessage(final UI lui) {
	    this.lui = lui;
	}
    }

    public static class AddUIMessage extends MessageBus.Message {
	final UI lui;

	public AddUIMessage(final UI lui) {
	    this.lui = lui;
	}
    }

    public static class CloseUIMessage extends MessageBus.Message {
	final UI lui;

	public CloseUIMessage(final UI lui) {
	    this.lui = lui;
	}
    }

    private MessageBus.Office UIThreadOffice;
    private MessageBus.MailBox<AddUIMessage> adduibox;
    private MessageBus.MailBox<RemoveUIMessage> remuibox;
    private MessageBus.MailBox<CloseUIMessage> closeuibox;
    /*  */

    private static final boolean dumpbgl = true;
    public final boolean vsync = true;
    public final CPUProfile uprof = new CPUProfile(300), rprof = new CPUProfile(300);
    public final GPUProfile gprof = new GPUProfile(300);
    private boolean bgmode = false;
    private boolean aswap;
    private int fps, framelag;
    private volatile int frameno;
    private double uidle = 0.0, ridle = 0.0;
    private final Dispatcher ed;
    private GLEnvironment env = null;
    private Area shape;
    private Pipe base, wnd;

    public static class ProfileException extends Environment.UnavailableException {
	public final String availability;

	public ProfileException(Throwable cause) {
	    super("No OpenGL suitable profile is available", cause);
	    String a;
	    try {
		a = GLProfile.glAvailabilityToString();
	    } catch(Throwable t) {
		a = String.valueOf(t);
	    }
	    this.availability = a;
	}
    }

    private static GLCapabilities mkcaps() {
	GLProfile prof;
	try {
	    prof = GLProfile.getMaxProgrammableCore(true);
	} catch(com.jogamp.opengl.GLException e) {
	    try {
		/* If not core, let GLEnvironment handle that. */
		prof = GLProfile.getDefault();
	    } catch(com.jogamp.opengl.GLException e2) {
		e2.addSuppressed(e);
		throw(new ProfileException(e2));
	    }
	}
	GLCapabilities caps = new GLCapabilities(prof);
	caps.setDoubleBuffered(true);
	caps.setAlphaBits(8);
	caps.setRedBits(8);
	caps.setGreenBits(8);
	caps.setBlueBits(8);
	return(caps);
    }

    public JOGLPanel(Coord sz) {
	super(mkcaps(), null, null);
	base = new BufPipe();
	base.prep(new FragColor<>(FragColor.defcolor)).prep(new DepthBuffer<>(DepthBuffer.defdepth));
	base.prep(FragColor.blend(new BlendMode()));
	setSize(sz.x, sz.y);
	setAutoSwapBufferMode(false);
	addGLEventListener(new GLEventListener() {
		public void display(GLAutoDrawable d) {
		    redraw(d.getGL());
		}

		public void init(GLAutoDrawable d) {
		}

		public void reshape(GLAutoDrawable wdg, int x, int y, int w, int h) {
		    Area area = Area.sized(new Coord(x, y), new Coord(w, h));
		    shape = area;
		    wnd = base.copy();
		    wnd.prep(new States.Viewport(area)).prep(new Ortho2D(area));
		}

		public void dispose(GLAutoDrawable wdg) {
		}
	    });
	setFocusTraversalKeysEnabled(false);
	ed = new Dispatcher();
	ed.register(this);
	if(Toolkit.getDefaultToolkit().getMaximumCursorColors() >= 256)
	    cursmode = "awt";
    }

    private double framedur() {
        return (bgmode && (1.0 / GlobalSettings.BGFPS.get() > 0.0))
		? 1.0 / GlobalSettings.BGFPS.get()
		: 1.0 / GlobalSettings.FPS.get();
    }

    public void setupMail(final Thread owner) {
	UIThreadOffice = new MessageBus.Office(owner);
	adduibox = new MessageBus.MailBox<>(UIThreadOffice, "add");
	remuibox = new MessageBus.MailBox<>(UIThreadOffice, "rem");
	closeuibox = new MessageBus.MailBox<>(UIThreadOffice, "close");
    }

    private void initgl(GL gl) {
	Collection<String> exts = Arrays.asList(gl.glGetString(GL.GL_EXTENSIONS).split(" "));
	GLCapabilitiesImmutable caps = getChosenGLCapabilities();
	gl.setSwapInterval((aswap = GlobalSettings.VSYNC.get()) ? 1 : 0);
	if(exts.contains("GL_ARB_multisample") && caps.getSampleBuffers()) {
	    /* Apparently, having sample buffers in the config enables
	     * multisampling by default on some systems. */
	    gl.glDisable(GL.GL_MULTISAMPLE);
	}
    }

    private final haven.error.ErrorHandler errh = haven.error.ErrorHandler.find();
    private void setenv(GLEnvironment env) {
	if(this.env != null)
	    this.env.dispose();
	this.env = env;
	synchronized (sessions) {
	    for(final var ui : sessions) {
	        ui.env = this.env;
	    }
	}

	if(errh != null) {
	    GLEnvironment.Caps caps = env.caps();
	    errh.lsetprop("gl.vendor", caps.vendor);
	    errh.lsetprop("gl.version", caps.version);
	    errh.lsetprop("gl.renderer", caps.renderer);
	    errh.lsetprop("render.caps", caps);
	}
    }

    private boolean debuggl = false;
    private long lastrcycle = 0, ridletime = 0;
    private void redraw(GL gl) {
	GLContext ctx = gl.getContext();
	GLEnvironment env;
	synchronized(this) {
	    if((this.env == null) || (this.env.ctx != ctx)) {
		setenv(new GLEnvironment(gl, ctx, shape));
		initgl(gl);
	    }
	    env = this.env;
	    if(!env.shape().equals(shape))
		env.reshape(shape);
	}
	GL3 gl3 = gl.getGL3();
	try {
	    if(false) {
		System.err.println("\n-----\n\n");
		gl3 = new TraceGL3(gl3, System.err);
	    }
	    if(debuggl) {
		gl3 = new DebugGL3(gl3);
	    }
	    env.process(gl3);
	    long end = System.nanoTime();
	} catch(BGL.BGLException e) {
	    if(dumpbgl)
		e.dump.dump();
	    Utils.setprefb("glcrash", true);
	    throw(e);
	}
    }

    {
	if(Utils.getprefb("glcrash", false)) {
	    Warning.warn("enabling GL debug-mode due to GL crash flag being set");
	    Utils.setprefb("glcrash", false);
	    if(errh != null)
		errh.lsetprop("gl.debug", Boolean.TRUE);
	    debuggl = true;
	}
    }

    public static enum SyncMode {
	FRAME, TICK, SEQ, FINISH
    }

    private class BufferSwap implements BGL.Request {
	final int frameno;

	BufferSwap(int frameno) {
	    this.frameno = frameno;
	}

	public void run(GL3 gl) {
	    long start = System.nanoTime();
	    boolean iswap = GlobalSettings.VSYNC.get();
	    if(debuggl)
		haven.render.gl.GLException.checkfor(gl, null);
	    if(iswap != aswap)
		gl.setSwapInterval((aswap = iswap) ? 1 : 0);
	    if(debuggl)
		haven.render.gl.GLException.checkfor(gl, null);
	    JOGLPanel.this.swapBuffers();
	    if(debuggl)
		haven.render.gl.GLException.checkfor(gl, null);
	    ridletime += System.nanoTime() - start;
	    framelag = JOGLPanel.this.frameno - frameno;
	}
    }

    private class GLFinish implements BGL.Request {
	public void run(GL3 gl) {
	    long start = System.nanoTime();
	    gl.glFinish();
	    /* Should this count towards idle time? Who knows. */
	    ridletime += System.nanoTime() - start;
	}
    }

    private class FrameCycle implements BGL.Request {
	public void run(GL3 gl) {
	    long now = System.nanoTime();
	    if(lastrcycle != 0) {
		double fridle = (double)ridletime / (double)(now - lastrcycle);
		ridle = (ridle * 0.95) + (fridle * 0.05);
	    }
	    lastrcycle = now;
	    ridletime = 0;
	}
    }

    private static class ProfileCycle implements BGL.Request {
	final CPUProfile prof;
	final String label;
	ProfileCycle prev;
	CPUProfile.Frame frame;

	ProfileCycle(CPUProfile prof, ProfileCycle prev, String label) {
	    this.prof = prof;
	    this.prev = prev;
	    this.label = label;
	}

	public void run(GL3 gl) {
	    if(prev != null) {
		if(label != null)
		    prev.frame.tick(label);
		prev.frame.fin();
		prev = null;
	    }
	    frame = prof.new Frame();
	}
    }

    private static class ProfileTick implements BGL.Request {
	final ProfileCycle prof;
	final String label;

	ProfileTick(ProfileCycle prof, String label) {
	    this.prof = prof;
	    this.label = label;
	}

	public void run(GL3 gl) {
	    if(prof != null)
		prof.frame.tick(label);
	}
    }

    private void uglyjoglhack() throws InterruptedException {
	try {
	    display();
	} catch(RuntimeException e) {
	    InterruptedException irq = Utils.hascause(e, InterruptedException.class);
	    if(irq != null)
		throw(irq);
	    throw(e);
	}
    }

    private void renderloop() {
	try {
	    uglyjoglhack();
	    synchronized(this) {
		if(env == null)
		    throw(new RuntimeException("Did not get GL environment even after display"));
		notifyAll();
	    }
	    while(true) {
		long wst = System.nanoTime();
		env.submitwait();
		ridletime += System.nanoTime() - wst;
		uglyjoglhack();
	    }
	} catch(InterruptedException e) {
	}
    }

    private Object prevtooltip = null;
    private Indir<Tex> prevtooltex = null;
    private Disposable freetooltex = null;
    private void drawtooltip(UI ui, GOut g) {
	Object tooltip;
        try {
	    synchronized(ui) {
		tooltip = ui.root.tooltip(ui.mc, ui.root);
	    }
	} catch(Loading e) {
	    tooltip = "...";
	}
	Indir<Tex> tt = null;
	if(Utils.eq(tooltip, prevtooltip)) {
	    tt = prevtooltex;
	} else {
	    if(freetooltex != null) {
		freetooltex.dispose();
		freetooltex = null;
	    }
	    prevtooltip = null;
	    prevtooltex = null;
	    Disposable free = null;
	    if(tooltip != null) {
		if(tooltip instanceof Text) {
		    Tex t = ((Text)tooltip).tex();
		    tt = () -> t;
		} else if(tooltip instanceof Tex) {
		    Tex t = (Tex)tooltip;
		    tt = () -> t;
		} else if(tooltip instanceof Indir<?>) {
		    @SuppressWarnings("unchecked")
		    Indir<Tex> c = (Indir<Tex>)tooltip;
		    tt = c;
		} else if(tooltip instanceof String) {
		    if(((String)tooltip).length() > 0) {
			Tex r = new TexI(Text.render((String)tooltip).img, false);
			tt = () -> r;
			free = r;
		    }
		}
	    }
	    prevtooltip = tooltip;
	    prevtooltex = tt;
	    freetooltex = free;
	}
	Tex tex = (tt == null) ? null : tt.get();
	if(tex != null) {
	    Coord sz = tex.sz();
	    Coord pos = ui.mc.add(sz.inv());
	    if(pos.x < 0)
		pos.x = 0;
	    if(pos.y < 0)
		pos.y = 0;
	    g.chcolor(244, 247, 21, 192);
	    g.rect(pos.add(-3, -3), sz.add(6, 6));
	    g.chcolor(35, 35, 35, 192);
	    g.frect(pos.add(-2, -2), sz.add(4, 4));
	    g.chcolor();
	    g.image(tex, pos);
	}
	ui.lasttip = tooltip;
    }

    private String cursmode = "tex";
    private Resource lastcursor = null;
    private void drawcursor(UI ui, GOut g) {
	Resource curs;
	synchronized(ui) {
	    curs = ui.getcurs(ui.mc);
	}
	if(cursmode == "awt") {
	    if(curs != lastcursor) {
		try {
		    if(curs == null)
			setCursor(null);
		    else
			setCursor(UIPanel.makeawtcurs(curs.layer(Resource.imgc).img, curs.layer(Resource.negc).cc));
		} catch(Exception e) {
		    cursmode = "tex";
		}
	    }
	} else if(cursmode == "tex") {
	    if(curs == null) {
		if(lastcursor != null)
		    setCursor(null);
	    } else {
		if(lastcursor == null)
		    setCursor(emptycurs);
		Coord dc = ui.mc.add(curs.layer(Resource.negc).cc.inv());
		g.image(curs.layer(Resource.imgc), dc);
	    }
	}
	lastcursor = curs;
    }

    private long prevfree = 0, framealloc = 0;
    @SuppressWarnings("deprecation")
    private void drawstats(UI ui, GOut g, GLRender buf) {
	int y = g.sz().y - UI.scale(190), dy = FastText.h;
	FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "FPS: %d (%d%%, %d%% idle, latency %d)", fps, (int)(uidle * 100.0), (int)(ridle * 100.0), framelag);
	Runtime rt = Runtime.getRuntime();
	long free = rt.freeMemory(), total = rt.totalMemory();
	if(free < prevfree)
	    framealloc = ((prevfree - free) + (framealloc * 19)) / 20;
	prevfree = free;
	FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Mem: %,011d/%,011d/%,011d/%,011d (%,d)", free, total - free, total, rt.maxMemory(), framealloc);
	FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "State slots: %d", State.Slot.numslots());
	FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "GL progs: %d", buf.env.numprogs());
	FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "V-Mem: %s", buf.env.memstats());
	MapView map = ui.root.findchild(MapView.class);
	if((map != null) && (map.back != null)) {
	    FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Camera: %s", map.camstats());
	    FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Mapview: %s", map.stats());
	    // FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Click: Map: %s, Obj: %s", map.clmaplist.stats(), map.clobjlist.stats());
	}
	if(ui.sess != null)
	    FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Async: L %s, D %s", ui.sess.glob.loader.stats(), Defer.gstats());
	else
	    FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "Async: D %s", Defer.gstats());
	int rqd = Resource.local().qdepth() + Resource.remote().qdepth();
	if(rqd > 0)
	    FastText.aprintf(g, new Coord(10, y -= dy), 0, 1, "RQ depth: %d (%d)", rqd, Resource.local().numloaded() + Resource.remote().numloaded());
    }

    private void display(UI ui, GLRender buf) {
	buf.clear(wnd, FragColor.fragcol, FColor.BLACK);
	Pipe state = wnd.copy();
	state.prep(new FrameInfo());
	GOut g = new GOut(buf, state, new Coord(getSize()));;
	synchronized(ui) {
	    ui.draw(g);
	}
	if(Config.dbtext)
	    drawstats(ui, g, buf);
	drawtooltip(ui, g);
	drawcursor(ui, g);
    }

    private void processMail() {
	{ //Handle transfers
	    UIThreadOffice.processTransfers();
	}
	{ //Handle new UIs
	    final Iterator<AddUIMessage> add = adduibox.mailqueue.iterator();
	    while (add.hasNext()) {
		final UI lui = add.next().lui;
		lui.setupMail(Thread.currentThread());

		synchronized (sessions) {
		    sessions.add(lui);
		}

		this.ui.getAndUpdate(cui -> cui != null ? cui : lui);
		add.remove();
	    }
	}
	{ //Handle any UI's that need to be destroyed.
	    final Iterator<RemoveUIMessage> rem = remuibox.mailqueue.iterator();
	    while (rem.hasNext()) {
		final UI lui = rem.next().lui;
		logger.atFine().log("Destroying UI [ui %s]", lui);
		lui.destroy();
		synchronized (sessions) {
		    sessions.remove(lui);
		}
		logger.atFine().log("Destroying UI [ui %s] [sessions %s]", lui, sessions.size());

		this.ui.getAndUpdate(ui -> {
		    logger.atFine().log("Comparing UIs [cur %s] [rem %s]", ui, lui);
		    if (ui == lui) {
			synchronized (sessions) {
			    Iterator<UI> itr = sessions.iterator();
			    if (itr.hasNext())
				return itr.next();
			}
			return null;
		    }
		    return ui;
		});
		rem.remove();
	    }
	}
	{ //Handle any UI's that need to start the closure process
	    final Iterator<CloseUIMessage> close = closeuibox.mailqueue.iterator();
	    while (close.hasNext()) {
		final UI lui = close.next().lui;
		if (lui.gui != null) {
		    lui.gui.act("lo");
		} else {
		    if (lui.sess != null) {
			lui.sess.close();
		    } else {
		        //Login screen
			lui.root.wdgmsg("close");
		    }
		}
		close.remove();
	    }
	}
    }

    private void updateBackgroundSessions(final UI ui) {
	//Update all other UIs as well, just don't render
	synchronized (sessions) {
	    for (final UI lui : sessions) {
		if (lui != ui) {
		    synchronized (lui) {
			if (lui.sess != null)
			    lui.sess.glob.ctick();
			lui.tick();
			if ((ui.root.sz.x != (shape.br.x - shape.ul.x)) || (ui.root.sz.y != (shape.br.y - shape.ul.y)))
			    lui.root.resize(new Coord(shape.br.x - shape.ul.x, shape.br.y - shape.ul.y));
		    }
		}
	    }
	}
    }

    public void run() {
	Thread drawthread = new HackThread(this::renderloop, "Render thread");
	drawthread.start();
	try {
	    GLRender buf = null;
	    try {
		synchronized(this) {
		    while(this.env == null)
			this.wait();
		}
		double then = Utils.rtime();
		double[] frames = new double[128], waited = new double[frames.length];
		Fence prevframe = null;
		ProfileCycle rprofc = null;
		int framep = 0;
		while(true) {
		    double fwaited = 0;

		    //Process any incoming mail
		    processMail();

		    //Get next UI to render this frame off
		    final UI ui;
		    final UI nextUI = this.nextUI.getAndSet(null);
		    if(nextUI != null) {
		        final UI oui = this.ui.get();
		        ui = nextUI;
		        this.ui.set(ui);
		        oui.audio.amb.clear();
		    } else {
		        ui = this.ui.get();
		    }
		    if(ui == null) {
		        if(sessions.size() > 0)
		            logger.atSevere().log("UI is missing [sessions %s]", sessions.size());
		        continue;
		    }
		    if(ui.env != this.env) {
		        ui.env = this.env;
		    }

		    if(!GlobalSettings.PAUSED.get()) {
			GLEnvironment env = this.env;
			buf = env.render();
			Debug.cycle(ui.modflags());
			SyncMode syncmode = SyncMode.valueOf(GlobalSettings.FRAMESYNCMODE.get());
			CPUProfile.Frame curf = Config.profile ? uprof.new Frame() : null;
			GPUProfile.Frame curgf = Config.profilegpu ? gprof.new Frame(buf) : null;
			BufferBGL.Profile frameprof = false ? new BufferBGL.Profile() : null;
			if(frameprof != null) buf.submit(frameprof.start);
			buf.submit(new ProfileTick(rprofc, "wait"));
			Fence curframe = new Fence();
			if(syncmode == SyncMode.FRAME)
			    buf.submit(curframe);

			boolean tickwait = (syncmode == SyncMode.FRAME) || (syncmode == SyncMode.TICK);
			if(!tickwait) {
			    if(prevframe != null) {
				double now = Utils.rtime();
				prevframe.waitfor();
				prevframe = null;
				fwaited += Utils.rtime() - now;
			    }
			    if(curf != null) curf.tick("dwait");
			}

			int cfno = frameno++;
			synchronized(ui) {
			    ed.dispatch(ui);
			    if(curf != null) curf.tick("dsp");

			    if(ui.sess != null) {
				ui.sess.glob.ctick();
				ui.sess.glob.gtick(buf);
			    }
			    if(curf != null) curf.tick("stick");
			    ui.tick();
			    ui.gtick(buf);
			    if((ui.root.sz.x != (shape.br.x - shape.ul.x)) || (ui.root.sz.y != (shape.br.y - shape.ul.y)))
				ui.root.resize(new Coord(shape.br.x - shape.ul.x, shape.br.y - shape.ul.y));
			    if(curf != null) curf.tick("tick");
			    buf.submit(new ProfileTick(rprofc, "tick"));
			    if(curgf != null) curgf.tick(buf, "tick");
			}

			if(tickwait) {
			    if(prevframe != null) {
				double now = Utils.rtime();
				prevframe.waitfor();
				prevframe = null;
				fwaited += Utils.rtime() - now;
			    }
			    if(curf != null) curf.tick("dwait");
			}

			display(ui, buf);
			if(curf != null) curf.tick("draw");
			if(curgf != null) curgf.tick(buf, "draw");
			buf.submit(new ProfileTick(rprofc, "gl"));
			buf.submit(new BufferSwap(cfno));
			if(curgf != null) curgf.tick(buf, "swap");
			buf.submit(new ProfileTick(rprofc, "swap"));
			if(curgf != null) curgf.fin(buf);
			if(syncmode == SyncMode.FINISH) {
			    buf.submit(new GLFinish());
			    buf.submit(new ProfileTick(rprofc, "finish"));
			}
			if(syncmode != SyncMode.FRAME)
			    buf.submit(curframe);
			if(Config.profile)
			    buf.submit(rprofc = new ProfileCycle(rprof, rprofc, "aux"));
			else
			    rprofc = null;
			buf.submit(new FrameCycle());
			if(frameprof != null) {
			    buf.submit(frameprof.stop);
			    buf.submit(frameprof.dump(new java.io.File("frameprof")));
			}
			env.submit(buf);
			buf = null;
			if(curf != null) curf.tick("aux");

			double now = Utils.rtime();
			double fd = framedur();
			if(then + fd > now) {
			    then += fd;
			    long nanos = (long)((then - now) * 1e9);
			    Thread.sleep(nanos / 1000000, (int)(nanos % 1000000));
			} else {
			    then = now;
			}
			fwaited += Utils.rtime() - now;
			frames[framep] = now;
			waited[framep] = fwaited;
			{
			    double twait = 0;
			    int i = 0, ckf = framep;
			    for(; i < frames.length - 1; i++) {
				ckf = (ckf - 1 + frames.length) % frames.length;
				twait += waited[ckf];
				if(now - frames[ckf] > 1)
				    break;
			    }
			    if(now > frames[ckf]) {
				fps = (int)Math.round((i + 1) / (now - frames[ckf]));
				uidle = twait / (now - frames[ckf]);
			    }
			}
			framep = (framep + 1) % frames.length;
			if(curf != null) curf.tick("wait");

			if(curf != null) curf.fin();
			prevframe = curframe;
		    } else {
			//Paused game
			synchronized(ui) {
			    ui.audio.amb.clear();
			    ed.dispatch(ui);
			    if (ui.sess != null) {
				ui.sess.glob.ctick();
			    }
			    ui.tick();
			    if ((ui.root.sz.x != (shape.br.x - shape.ul.x)) || (ui.root.sz.y != (shape.br.y - shape.ul.y)))
				ui.root.resize(new Coord(shape.br.x - shape.ul.x, shape.br.y - shape.ul.y));
			}
		    }

		    //Update background UIs
		    updateBackgroundSessions(ui);
		}
	    } finally {
		if(buf != null)
		    buf.dispose();
		drawthread.interrupt();
		drawthread.join();
	    }
	} catch(InterruptedException e) {
	}
    }

    public UI newui(UI.Runner fun) {
        final UI lui = new UI(this, new Coord(getSize()), fun);
        lui.env = this.env;
        lui.root.guprof = uprof;
        lui.root.grprof = rprof;
        lui.root.ggprof = gprof;
        if(getParent() instanceof  Console.Directory) {
            lui.cons.add((Console.Directory)getParent());
	}
        lui.cons.add(this);
        adduibox.mail(new AddUIMessage(lui));
        return lui;
    }


    public UI getActiveUI() {
	return this.ui.get();
    }

    public boolean isActiveUI(final UI lui) {
	return getActiveUI() == lui;
    }

    public void setActiveUI(final UI lui) {
	if (getActiveUI() != lui) {
	    this.nextUI.set(lui);
	}
    }

    public int sessionCount() {
	synchronized (sessions) {
	    return sessions.size();
	}
    }

    public boolean isMasterUIActive() {
	synchronized (sessions) {
	    final UI ui = this.ui.get();
	    Iterator<UI> itr = sessions.iterator();
	    if (itr.hasNext())
		return itr.next() == ui;
	    else
		return false;
	}
    }

    @Override
    public void listenToSessions(ObservableListener<UI> listener) {
	synchronized (sessions) {
	    sessions.addListener(listener);
	}
    }

    @Override
    public void stopListeningToSessions(ObservableListener<UI> listener) {
	synchronized (sessions) {
	    sessions.removeListener(listener);
	}
    }

    public void closeCurrentSession() {
	final UI ui = this.ui.get();
	closeuibox.mail(new CloseUIMessage(ui));
    }

    //Remove a UI
    public void removeUI(final UI lui) {
	logger.atFine().log("Closing session [ui %s] [thread %s]", lui, Thread.currentThread());
	remuibox.mail(new RemoveUIMessage(lui));
    }

    public void background(boolean bg) {
	bgmode = bg;
    }

    private Robot awtrobot;
    public void setmousepos(Coord c) {
	java.awt.EventQueue.invokeLater(() -> {
		if(awtrobot == null) {
		    try {
			awtrobot = new Robot(getGraphicsConfiguration().getDevice());
		    } catch(java.awt.AWTException e) {
			return;
		    }
		}
		Point rp = getLocationOnScreen();
		awtrobot.mouseMove(rp.x + c.x, rp.y + c.y);
	    });
    }

    private Map<String, Console.Command> cmdmap = new TreeMap<String, Console.Command>();
    {
	cmdmap.put("gldebug", (cons, args) -> {
		debuggl = Utils.parsebool(args[1]);
	    });
    }
    public Map<String, Console.Command> findcmds() {
	return(cmdmap);
    }
}
