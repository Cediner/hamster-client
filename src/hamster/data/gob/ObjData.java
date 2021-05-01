package hamster.data.gob;

import com.google.common.flogger.FluentLogger;
import com.google.gson.Gson;
import hamster.GlobalSettings;
import hamster.gob.Tag;
import haven.Gob;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.*;
import java.util.regex.Pattern;

public class ObjData {
    private static final FluentLogger logger = FluentLogger.forEnclosingClass();
    private static final Map<String, ObjData> objmap = new HashMap<>();
    private static final List<ObjData> regobjlst = new ArrayList<>();
    private static final Map<String, CropData> cropmap = new HashMap<>();
    private static final List<ForageData> forageables = new ArrayList<>();

    public static void init() {
        logger.atInfo().log("Loading Object Data");
        final var gson = new Gson();
        try {
            final var objs = gson.fromJson(new FileReader("data/ObjData.json5"), ObjData[].class);
            for(final var obj : objs) {
                if(!obj.regex)
                    objmap.put(obj.res, obj);
                else {
                    obj.resregex = Pattern.compile(obj.res);
                    regobjlst.add(obj);
                }
                if(obj.cropdata != null)
                   cropmap.put(obj.res, obj.cropdata);
                if(obj.foragedata != null) {
                    obj.foragedata.init(obj);
                    forageables.add(obj.foragedata);
                }
            }
        } catch (FileNotFoundException e) {
            logger.atSevere().withCause(e).log("Failed to load object data");
        }
    }

    public static boolean isACrop(final String name) {
        return cropmap.containsKey(name);
    }
    public static CropData getCropData(final String name) {
        return cropmap.get(name);
    }

    public static boolean isForagable(final String name, final Gob g) {
        return name.startsWith("gfx/terobjs/herbs/") || g.hasTag(Tag.CAN_PICK_UP) ||
                (GlobalSettings.FORAGEANIMALS.get() && g.hasTag(Tag.ANIMAL) && g.hasTag(Tag.CAN_PICK_UP)) ||
                (name.equals("gfx/kritter/bat/bat") && g.isDead()) ||
                (name.equals("gfx/kritter/swan/swan") && g.isDead()) ||
                (name.equals("gfx/kritter/adder/adder") && g.isDead()) ||
                (name.startsWith("gfx/terobjs/items/"));
    }

    public static List<ForageData> getForageables() { return forageables; }

    public static boolean hasRange(final String name) {
        return objmap.containsKey(name) && objmap.get(name).range != -1;
    }
    public static int getRange(final String name) {
        return objmap.containsKey(name) ? objmap.get(name).range : 0;
    }

    public static Optional<ContainerData> getContData(final String name) {
        return objmap.containsKey(name) ? Optional.ofNullable(objmap.get(name).containerdata) : Optional.empty();
    }

    public static Set<Tag> getTags(final String name) {
        final Set<Tag> tags = objmap.containsKey(name) ? objmap.get(name).tags : new HashSet<>();
        if(tags.size() == 0) {
            for(final var obj : regobjlst) {
                if(obj.resregex.matcher(name).matches())
                    return obj.tags;
            }
        }
        return tags;
    }

    // Resource file name
    private String res = "";
    private Pattern resregex;
    private boolean regex = false;
    // Simplified name, if has any
    private String name = "";
    //tmp to generate tagset
    private Set<Tag> tags;
    //If Obj has a radius that can be toggled
    private int range = -1;
    private CropData cropdata = null;
    private ForageData foragedata = null;
    private ContainerData containerdata = null;

    public String name() { return name; }
    public String res() { return res; }
}
