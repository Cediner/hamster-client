/* Preprocessed source code */
import haven.*;
import java.util.*;
import java.awt.Color;

/* >wdg: ThingTravel */
public class ThingTravel extends Window {
    public static final int width = UI.scale(200);
    public static final Color[] colors;
    public final String name;
    public final Collection<Destination> dest = new ArrayList<>();
    private int nextid = 0;
    private Position nextdest;

    static {
	colors = new Color[11];
	float c = 1.0f / 3.0f, o = (1.0f / 3.0f) / 4.0f;
	colors[0] = Color.getHSBColor(c, 1.0f, 1.0f);
	for(int i = 1; i <= 5; i++) {
	    colors[(i * 2) - 1] = Color.getHSBColor(c - (o * i), 1.0f, 1.0f);
	    colors[(i * 2)] = Color.getHSBColor(c + (o * i), 1.0f, 1.0f);
	}
    }

    public static class Destination {
	public final int id;
	public final String name;
	public final double a, d, cost;
	public final Color color;
	public boolean highlight;

	public Destination(int id, String name, double a, double d, double cost) {
	    this.id = id;
	    this.name = name;
	    this.a = a;
	    this.d = d;
	    this.cost = cost;
	    this.color = colors[id % colors.length];
	}
    }

    public class DestButton extends Button {
	public final Destination dest;

	public DestButton(int width, Destination dest) {
	    super(width, dest.name, false);
	    this.dest = dest;
	    // change(dest.name, dest.color);
	}

	public void click() {
	    ThingTravel.this.wdgmsg("trav", dest.id);
	}

	public void mousemove(Coord c) {
	    dest.highlight = c.isect(Coord.z, this.sz);
	}
    }

    public static class Compass extends Widget {
	public static final Tex bg = Resource.remote().loadwait("ui/thingwall").layer(Resource.imgc, 0).tex();
	public static final Tex mark = Resource.remote().loadwait("ui/thingwall").layer(Resource.imgc, 1).tex();
	public static final int cr = UI.scale(60), pr = UI.scale(5);
	public final ThingTravel tr;
	private double mr;

	public Compass(ThingTravel tr) {
	    super(bg.sz());
	    this.tr = tr;
	}

	private Coord destc(Destination dest) {
	    if(mr == 0)
		return(Coord.z);
	    return(sz.div(2).add(Coord.sc(-dest.a, cr * dest.d / mr)));
	}

	public void draw(GOut g) {
	    g.image(bg, Coord.z);
	    if(tr.dest.isEmpty())
		return;
	    mr = 0;
	    for(Destination dest : tr.dest)
		mr = Math.max(mr, dest.d);
	    for(Destination dest : tr.dest) {
		g.chcolor(dest.highlight ? Color.RED : dest.color);
		g.aimage(mark, destc(dest), 0.5, 0.5);
	    }
	}

	public Object tooltip(Coord c, Widget prev) {
	    for(Destination dest : tr.dest) {
		Coord ul = destc(dest).sub(mark.sz().div(2));
		if(c.isect(ul, mark.sz()))
		    return(dest.name);
	    }
	    return(super.tooltip(c, prev));
	}
    }

    public ThingTravel(String name) {
	super(Coord.z, "Thingwall");
	this.name = name;
	Widget prev = add(new Img(CharWnd.catf.render(name).tex()), 0, 0);
	Compass cmp;
	prev = cmp = add(new Compass(this), prev.pos("bl").adds(0, 5));
	prev = add(new Label("Travel to:"), prev.pos("bl").adds(0, 10));
	nextdest = prev.pos("bl").adds(0, 5).x(0);
	pack();
	for(Widget ch : children())
	    ch.move(new Coord((asz.x - ch.sz.x) / 2, ch.c.y));
    }

    public static ThingTravel mkwidget(UI ui, Object... args) {
	String name = (String)args[0];
	return(new ThingTravel(name));
    }

    public void uimsg(String name, Object... args) {
	if(name.equals("dest")) {
	    Destination dest = new Destination(nextid++,
		    (String)args[0],
		    ((Number)args[1]).doubleValue() * Math.PI / 180.0,
		    ((Number)args[2]).doubleValue() / 100.0,
		    ((Number)args[3]).doubleValue() / 100.0);
	    this.dest.add(dest);
	    Button btn = add(new DestButton(width, dest), nextdest);
	    btn.settip(String.format("Travel weariness: %.1f", dest.cost));
	    nextdest = btn.pos("bl").adds(0, 5).x((asz.x - btn.sz.x) / 2);
	    pack();
	} else {
	    super.uimsg(name, args);
	}
    }
}
