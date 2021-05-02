package hamster;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Session based settings based off account:character name
 */
public class SessionSettings {
    public static class Buff {
        public final IndirSetting<Boolean> status;
        public final String res;

        public Buff(final IndirSetting<Boolean> status, final String res) {
            this.status = status;
            this.res = res;
        }
    }

    //Tracking server states of buffs
    //Seems like the server will remember: swimming, criminal acts, tracking, and visiting
    //In loftar's infinite wisdom he made it remember on an account level and not character...
    public final IndirSetting<Boolean> SWIM;
    public final IndirSetting<Boolean> TRACKING;
    public final IndirSetting<Boolean> CRIMINALACTS;
    public final IndirSetting<Boolean> VISITING;
    // Party perms is not remembered by the server, so if they log off with it on I will need
    // to retoggle it on log in to stay consistent
    public final IndirSetting<Boolean> PARTYPERMS;
    private final Map<String, Buff> buffmap = new HashMap<>();

    public SessionSettings(final String account, final String character) {
        final Settings local = new Settings(String.format("%s:%s", account, character));
        final Settings acc = new Settings(String.format("%s", account));

        SWIM = new IndirSetting<>(acc, "state.swimming", false);
        TRACKING = new IndirSetting<>(acc, "state.tracking", false);
        CRIMINALACTS = new IndirSetting<>(acc, "state.criminal-acts", false);
        VISITING = new IndirSetting<>(acc, "state.visiting", false);
        PARTYPERMS = new IndirSetting<>(local, "state.party-perms", false);
        buffmap.put("Swimming", new Buff(SWIM, "custom/paginae/default/toggles/swim"));
        buffmap.put("Tracking", new Buff(TRACKING, "custom/paginae/default/toggles/tracking"));
        buffmap.put("Visiting", new Buff(VISITING, "custom/paginae/default/toggles/nopeace"));
        buffmap.put("Criminal", new Buff(CRIMINALACTS, "custom/paginae/default/toggles/crime"));
        buffmap.put("Party", new Buff(PARTYPERMS, "custom/paginae/default/toggles/permshare"));
    }

    public Buff buff(final String name) {
        return buffmap.get(name);
    }

    public Collection<Buff> buffs() {
        return buffmap.values();
    }
}
