/* Preprocessed source code */
/* $use: lib/vmat */

import hamster.gfx.KnarrSprite;
import haven.*;

/* >spr: Knarr */
public class Knarr implements Sprite.Factory {
    public Sprite create(Sprite.Owner owner, Resource res, Message sdt) {
	final var ret = new KnarrSprite(owner, res, Message.nil);
	ret.update(sdt);
	return ret;
    }
}
