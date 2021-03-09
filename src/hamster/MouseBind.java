package hamster;

import haven.UI;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MouseBind {
    private static final Settings datastore = new Settings("mousebinds", new HashMap<>());
    public static final Map<String, List<MouseBind>> bindgrps = new HashMap<>();
    //MapView related
    public static final MouseBind
        MV_LOCK_PLACING_OBJ, MV_SHOW_SPEC_MENU, MV_QUEUE_MOVE, MV_PATHFIND_MOVE;
    //Item related
    public static final MouseBind
        ITM_TRANSFER, ITM_TRANSFER_ALL_ALIKE, ITM_DROP, ITM_DROP_ALL_ALIKE,
        ITM_TAKE, ITM_TOGGLE_LOCK, ITM_AUTO_EQUIP, ITM_AUTO_EQUIP_LH,
        ITM_AUTO_EQUIP_RH;
    //Held Item Related
    public static final MouseBind
        HITM_TOGGLE_LOCK, HITM_DROP, HITM_IACT_OBJ, HITM_IACT;

    private static MouseBind addMB(final String name, final String group, final String bind) {
        final MouseBind mb = new MouseBind(name, group,
                new IndirSetting<>(datastore, name.replaceAll("\\s","").toLowerCase(), bind));
        if (bindgrps.containsKey(mb.grouping))
            bindgrps.get(mb.grouping).add(mb);
        else {
            final List<MouseBind> set = new ArrayList<>();
            set.add(mb);
            bindgrps.put(mb.grouping, set);
        }
        return mb;
    }

    static {
        //Map related
        final String MV_GRP = "Map";
        MV_LOCK_PLACING_OBJ = addMB("Lock placing object", MV_GRP, "C-B3");
        MV_SHOW_SPEC_MENU = addMB("Show special menu", MV_GRP, "M-B3");
        MV_QUEUE_MOVE = addMB("Queue move", MV_GRP, "M-B1");
        MV_PATHFIND_MOVE = addMB("Pathfind move", MV_GRP, "S-C-M-B1");
        //Item related
        final String ITM_GRP = "Item";
        ITM_TRANSFER = addMB("Transfer item", ITM_GRP, "S-B1");
        ITM_TRANSFER_ALL_ALIKE = addMB("Transfer all alike items", ITM_GRP, "M-B1");
        ITM_DROP = addMB("Drop item", ITM_GRP, "C-B1");
        ITM_DROP_ALL_ALIKE = addMB("Drop all alike items", ITM_GRP, "M-B1");
        ITM_TAKE = addMB("Take item", ITM_GRP, "B1");
        ITM_TOGGLE_LOCK = addMB("Toggle lock on item", ITM_GRP, "C-B3");
        ITM_AUTO_EQUIP = addMB("Auto equip item", ITM_GRP, "M-B3");
        ITM_AUTO_EQUIP_LH = addMB("Auto equip item into left hand", ITM_GRP, "S-B2");
        ITM_AUTO_EQUIP_RH = addMB("Auto equip item into right hand", ITM_GRP, "C-B2");
        //Held Item related
        final String HITM_GRP = "Held Item";
        HITM_TOGGLE_LOCK = addMB("Toggle lock on held item", HITM_GRP, "C-B3");
        HITM_DROP = addMB("Drop Held Item ", HITM_GRP, "B1");
        HITM_IACT_OBJ = addMB("Interact Held Item with Object", HITM_GRP, "B3");
        //XXX: This one may no longer be possible due to server-side updates
        HITM_IACT = addMB("Interact with Held Item (only when locked)", HITM_GRP, "M-B3");
    }

    @FunctionalInterface
    public interface Command {
        boolean run();
    }

    public final String name;
    public final String grouping;
    public final IndirSetting<String> bind;

    public MouseBind(final String name, final String grouping, final IndirSetting<String> bind) {
        this.name = name;
        this.grouping = grouping;
        this.bind = bind;
    }

    public boolean check(final String ibind, final Command action) {
        return ibind.equals(bind.get()) && action.run();
    }

    public boolean match(final String ibind) {
        return ibind.equals(bind.get());
    }

    public static boolean validBinding(final IndirSetting<String> ignore, final String group, final String binding) {
        if (binding.equals("")) {
            return true;
        } else {
            for (final MouseBind mb : bindgrps.get(group)) {
                if (mb.bind != ignore && mb.bind.get().equals(binding)) {
                    return false;
                }
            }
        }
        return true;
    }

    public static String generateSequence(final UI ui, final int mbutton) {
        final StringBuilder seq = new StringBuilder();
        if (ui.modshift)
            seq.append("S-");
        if (ui.modctrl)
            seq.append("C-");
        if (ui.modmeta)
            seq.append("M-");
        seq.append("B");
        seq.append(mbutton);
        return seq.toString();
    }
}
