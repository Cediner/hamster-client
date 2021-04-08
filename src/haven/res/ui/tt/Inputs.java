package haven.res.ui.tt;

/* Preprocessed source code */
/* $use: lib/tspec */

import haven.*;
import java.util.*;
import java.awt.image.BufferedImage;
import haven.res.lib.tspec.Spec;

/* >tt: Inputs */
public class Inputs extends ItemInfo.Tip {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return mkinfo(owner, args);
	}
    }

    private static final BufferedImage comma = Text.render(", ").img;
    public final Input[] inputs;

    public Inputs(Owner owner, Input[] inputs) {
	super(owner);
	this.inputs = inputs;
    }

    public static class Input {
	public final Spec spec;
	public BufferedImage img;
	public int num;
	public boolean opt;

	public Input(Spec spec) {
	    this.spec = spec;
	    img = null;
	    GSprite spr = spec.spr();
	    if(spr instanceof GSprite.ImageSprite)
		img = ((GSprite.ImageSprite)spec.spr()).image();
	    if(img == null)
		img = spec.res.res.get().layer(Resource.imgc).img;
	    int h = comma.getHeight();
	    img = PUtils.convolvedown(img, new Coord(h, h), CharWnd.iconfilter);
	}
    }

    public static ItemInfo mkinfo(ItemInfo.Owner owner, Object... args) {
	Resource.Resolver rr = owner.context(Resource.Resolver.class);
	int a = 1;
	List<Input> buf = new ArrayList<>();
	while(a < args.length) {
	    int fl = (Integer)args[a++];
	    Indir<Resource> res = rr.getres((Integer)args[a++]);
	    int num = (Integer)args[a++];
	    Message sdt = Message.nil;
	    if((fl & 1) != 0)
		sdt = new MessageBuf((byte[])args[a++]);
	    Input inp = new Input(new Spec(new ResData(res, sdt), owner, null));
	    inp.num = num;
	    if((fl & 2) != 0)
		inp.opt = true;
	    buf.add(inp);
	}
	return(new Inputs(owner, buf.toArray(new Input[0])));
    }

    public void layout(Layout l) {
	CompImage line = new CompImage();
	boolean f = true;
	for(Input inp : inputs) {
	    if(!f)
		line.add(comma, new Coord(line.sz.x, 0));
	    f = false;
	    line.add(inp.img, new Coord(line.sz.x, 0));
	    line.add(Text.render(String.format("\u00d7%d", inp.num)).img, new Coord(line.sz.x, 0));
	    if(line.sz.x > 200) {
		l.cmp.add(line, new Coord(0, l.cmp.sz.y));
		line = new CompImage();
		f = true;
	    }
	}
	l.cmp.add(line, new Coord(0, l.cmp.sz.y));
    }
}
