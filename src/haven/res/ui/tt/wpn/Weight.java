package haven.res.ui.tt.wpn;

/* Preprocessed source code */
/* $use: ui/tt/wpn/info */
import haven.*;
import haven.res.ui.tt.wpn.info.*;
import static haven.PUtils.*;
import java.awt.image.BufferedImage;

/* >tt: Weight */
public class Weight extends WeaponInfo {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return mkinfo(owner, args);
	}
    }

    public final Resource attr;
    public Weight(Owner owner, Resource attr) {
	super(owner);
	this.attr = attr;
    }

    public static Weight mkinfo(Owner owner, Object... args) {
	return(new Weight(owner, owner.context(Resource.Resolver.class).getres((Integer)args[1]).get()));
    }

    public BufferedImage wpntip() {
	BufferedImage line = Text.render("Attack weight: ").img;
	BufferedImage icon = convolvedown(attr.layer(Resource.imgc).img, new Coord(line.getHeight(), line.getHeight()), CharWnd.iconfilter);
	return(catimgsh(0, line, icon));
    }

    public int order() {return(75);}
}
