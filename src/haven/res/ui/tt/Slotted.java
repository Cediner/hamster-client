package haven.res.ui.tt;

/* Preprocessed source code */
import hamster.data.character.Attribute;
import haven.*;
import haven.res.ui.tt.attrmod.AttrMod;

import static haven.PUtils.*;
import java.awt.image.*;
import java.util.*;

/* >tt: Fac */
public class Slotted extends ItemInfo.Tip implements Gild {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(Owner owner, Raw raw, Object... args) {
	    Resource.Resolver rr = owner.context(Resource.Resolver.class);
	    int a = 1;
	    double pmin = ((Number)args[a++]).doubleValue();
	    double pmax = ((Number)args[a++]).doubleValue();
	    List<Resource> attrs = new LinkedList<>();
	    while(args[a] instanceof Integer)
		attrs.add(rr.getres((Integer)args[a++]).get());
	    Object[] rawargs = (Object[])args[a];
	    return(new Slotted(owner, pmin, pmax, attrs.toArray(new Resource[0]), ItemInfo.buildinfo(owner, rawargs)));
	}
    }


    public static final Text.Line ch = Text.render("As gilding:");
    public final double pmin, pmax;
    public final Resource[] attrs;
    public final List<ItemInfo> sub;

    public Slotted(Owner owner, double pmin, double pmax, Resource[] attrs, List<ItemInfo> sub) {
	super(owner);
	this.pmin = pmin;
	this.pmax = pmax;
	this.attrs = attrs;
	this.sub = sub;
    }

    public static final String chc = "192,192,255";
    public void layout(Layout l) {
	l.cmp.add(ch.img, new Coord(0, l.cmp.sz.y));
	if(attrs.length > 0) {
	    BufferedImage head = RichText.render(String.format("Chance: $col[%s]{%d%%} to $col[%s]{%d%%}", chc, Math.round(100 * pmin), chc, Math.round(100 * pmax)), 0).img;
	    int h = head.getHeight();
	    int x = 10, y = l.cmp.sz.y;
	    l.cmp.add(head, new Coord(x, y));
	    x += head.getWidth() + 10;
	    for(int i = 0; i < attrs.length; i++) {
		BufferedImage icon = convolvedown(attrs[i].layer(Resource.imgc).img, new Coord(h, h), CharWnd.iconfilter);
		l.cmp.add(icon, new Coord(x, y));
		x += icon.getWidth() + 2;
	    }
	} else {
	    BufferedImage head = RichText.render(String.format("Chance: $col[%s]{%d%%}", chc, (int)Math.round(100 * pmin)), 0).img;
	    l.cmp.add(head, new Coord(10, l.cmp.sz.y));
	}

	BufferedImage stip = longtip(sub);
	if(stip != null)
	    l.cmp.add(stip, new Coord(10, l.cmp.sz.y));
    }

    public int order() {
	return(200);
    }

    public double pmin() { return Math.round(100d*pmin); }
    public double pmax() { return Math.round(100d*pmax); }
    public boolean hasAttr(final Attribute attr) {
	for(final var res : attrs) {
	    if(attr.is(res.name))
		return true;
	}
	return false;
    }

    public Optional<AttrMod.Mod> mod(final Attribute attr) {
        for(final var inf : sub) {
            if(inf instanceof AttrMod && ((AttrMod) inf).hasAttr(attr)) {
                return ((AttrMod) inf).mod(attr);
	    }
	}
        return Optional.empty();
    }
}
