/* Preprocessed source code */
/* $use: lib/vmat */

import hamster.gfx.SnekkjaSprite;
import haven.*;

/* >spr: Snekkja */
public class Snekkja implements Sprite.Factory {
    public Sprite create(Sprite.Owner owner, Resource res, Message sdt) {
        final var ret = new SnekkjaSprite(owner, res, Message.nil);
        ret.update(sdt);
        return ret;
    }
}
