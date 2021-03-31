/* Preprocessed source code */
import hamster.GlobalSettings;
import haven.*;

/* >wdg: ExpWnd */
public class ExpWnd extends Window {
    public static Resource sfx = Loading.waitfor(Resource.remote().load("sfx/exp", 1));
    public static final RichText.Foundry fnd = new RichText.Foundry();
    public final Indir<Resource> exp;
    public final int ep;
    private Button close;
    private Img img;

    public static Widget mkwidget(UI ui, Object... args) {
	Indir<Resource> res = ui.sess.getres((Integer)args[0]);
	int ep = (args.length > 1)?((Integer)args[1]):0;
	return(new ExpWnd(res, ep));
    }

    public ExpWnd(Indir<Resource> exp, int ep) {
	super(UI.scale(300, 50), "Hey, listen!", true);
	this.exp = exp;
	this.ep = ep;
	hide();
    }

    protected void added() {
        if(ep > 0)
            ui.gui.msg(String.format("Experience points gained: %d", ep));
        if(GlobalSettings.SHOWEXPWND.get()) {
	    if (c.equals(0, 0))
		c = new Coord((parent.sz.x - sz.x) / 2, ((parent.sz.y / 2) - sz.y) / 2);
	    Audio.play(sfx);
	    show();
	    super.added();
	} else {
            hide();
	}
    }

    @Override
    protected void binded() {
	super.binded();
	if(!GlobalSettings.SHOWEXPWND.get()) {
	    wdgmsg("close");
	}
    }

    public void tick(double dt) {
	if(img == null) {
	    Tex img;
	    String cap, text;
	    try {
		img = exp.get().layer(Resource.imgc).tex();
		Resource.Tooltip tt = exp.get().layer(Resource.tooltip);
		cap = (tt == null)?null:(tt.t);
		text = exp.get().layer(Resource.pagina).text;
	    } catch(Loading e) {
		return;
	    }
	    if(cap != null)
		chcap(cap);
	    this.img = add(new Img(img), 0, UI.scale(10));
	    Img text1 = add(new Img(fnd.render(text, UI.scale(300)).tex()), img.sz().x + UI.scale(5), 10);
	    if(ep > 0)
		add(new Label("Experience points gained: " + ep), text1.c.x, text1.c.y + text1.sz.y + UI.scale(10));
	    Coord csz = contentsz();
	    this.close = adda(new Button(UI.scale(100), "Okay!"), csz.x / 2, csz.y + UI.scale(25), 0.5, 0);
	    resize(contentsz());
	    this.c = new Coord((parent.sz.x - sz.x) / 2, ((parent.sz.y / 2) - sz.y) / 2);
	}
    }

    public void wdgmsg(Widget sender, String msg, Object... args) {
	if(sender == close)
	    wdgmsg("close");
	else
	    super.wdgmsg(sender, msg, args);
    }
}
