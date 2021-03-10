package hamster.gob.sprites;

import haven.Gob;
import haven.Message;
import haven.Resource;
import haven.SkelSprite;
import haven.render.BaseColor;
import haven.render.Pipe;

import java.awt.*;
import java.util.Collections;

/**
 * Color mod for Gobs we have Aggro'd and Sprite Model to display the floating pointer above them
 */
public class AggroMark extends SkelSprite implements Gob.SetupMod {
    private static final BaseColor col = new BaseColor(Color.RED);
    private static final Resource tgtfx = Resource.local().loadwait("custom/fx/partytgt");
    public static final int id = -4214129;

    private boolean alive = true;

    public AggroMark() {
        super(null, tgtfx, Message.nil);
    }


    public void rem() {
        final Gob g = ((Gob)owner);
        if(g != null) {
            g.queueDeltas(Collections.singletonList((gob) -> {
                final Gob.Overlay ol = gob.findol(id);
                if (ol != null) {
                    ol.remove();
                }
            }));
        }
    }

    @Override
    public Pipe.Op gobstate() {
        return col;
    }
}
