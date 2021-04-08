package haven.res.ui.tt;

/* Preprocessed source code */
import haven.*;
import java.awt.image.BufferedImage;

/* >tt: Cost */
public class Cost extends ItemInfo.Tip {
    public static class Fac implements ItemInfo.InfoFactory {
	@Override
	public ItemInfo build(ItemInfo.Owner owner, ItemInfo.Raw raw, Object... args) {
	    return mkinfo(owner, args);
	}
    }

    public final int enc;

    public Cost(Owner owner, int enc) {
	super(owner);
	this.enc = enc;
    }

    public static ItemInfo mkinfo(Owner owner, Object... args) {
	return(new Cost(owner, ((Number)args[1]).intValue()));
    }

    public BufferedImage tipimg() {
	return(Text.render(String.format("Cost: %,d XP", enc)).img);
    }
}
