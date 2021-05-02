package hamster.ui.fight;

import java.util.HashMap;
import java.util.Map;

import static hamster.ui.fight.DefenseType.*;
import static hamster.ui.fight.WeightType.MC;
import static hamster.ui.fight.WeightType.UA;

//TODO: lots of things could make this mess nicer, there are custom features about attacks not accounted for currently also
//      ideally, cards should take in State and return new State for the entire fight, not for specific things..
public class Cards {
    public static final Card unknown = new Card("Unknown", false, 0);

    //Maneuvers
    public static final Maneuver bloodlust = new Maneuver("Bloodlust", 10, UA, 0.75);
    public static final Maneuver chinup = new Maneuver("Chin Up", 10, UA, 1.0);
    public static final Maneuver combatmed = new Maneuver("Combat Meditation", 10, UA, 1.0);
    public static final Maneuver dog = new Maneuver("Death or Glory", 10, UA, 0.75);
    public static final Maneuver oakstance = new Maneuver("Oak Stance", 10, UA, 1.5);
    public static final Maneuver parry = new Maneuver("Parry", 10, MC, 0.80);
    public static final Maneuver shieldup = new Maneuver("Shield Up", 10, MC, 2.50);
    public static final Maneuver toarms = new Maneuver("To Arms", 10, MC, 1.0);

    //Restorations
    public static final Restoration arteva = new Restoration("Artful Evasion", false, 40, 0, 0.2, 0.2, 0.2, 0.2,
            (mip, eip) -> eip >= 1);
    public static final Restoration fdodge = new Restoration("Feigned Dodge", false, 35, 0, 0.0, 0.15, 0.0, 0.0);
    public static final Restoration flex = new Restoration("Flex", false, 30, 0, 0.1, 0.0, 0.0, 0.1, 0.0, 0.0, 0.0, 0.15, UA, 1,
            (mip, eip) -> true);
    public static final Restoration jump = new Restoration("Jump", false, 25, 0, 0.0, 0.0, 0.2, 0.0);
    public static final Restoration qdodge = new Restoration("Quick Dodge", false, 25, 0, 0.0, 0.2, 0.0, 0.0);
    public static final Restoration regaincomp = new Restoration("Regain Composure", false, 25, 0, 0.0, 0.2, 0.2, 0.0);
    public static final Restoration sidestep = new Restoration("Sidestep", false, 25, 0, 0.0, 0.0, 0.0, 0.2);
    public static final Restoration watchitsmoves = new Restoration("Watch Its Moves", false, 45, 0, 0.0, 0.0, 0.0, 0.3,
            (mip, eip) -> eip >= 1);
    //technically there's more to yieldground but who cares
    public static final Restoration yieldground = new Restoration("Yield Ground", false, 30, 0, 0.3, 0.3, 0.0, 0.0);
    public static final Restoration zigzag = new Restoration("Zig-Zag Ruse", false, 50, 0, 0.5, 0.0, 0.5, 0.0,
            (mip, eip) -> eip >= 2);

    //Attacks
    public static final Attack jugular = new Attack("Go for the Jugular", false, 45, UA, 4, 40, 0.3, 1.0, false, 1.0, 0.1, 0.15, 0.0, 0.0, GREEN, RED);
    public static final Attack hmaker = new Attack("Haymaker", false, 50, UA, 0, 20, 0.15, 1.0, false, 1.0, 0.0, 0.0, 0.15, 0.0, YELLOW);
    public static final Attack kick = new Attack("Kick", false, 45, UA, 1, 25, 0.15, 1.0, false, 1.0, 0.0, 0.0, 0.2, 0.0, YELLOW);
    public static final Attack kito = new Attack("Knock Its Teeth Out", false, 35, UA, 1, 30, 0.25, 1.0, false, 1.0, 0.2, 0.0, 0.0, 0.0, RED);
    public static final Attack lhook = new Attack("Left Hook", false, 40, UA, 0, 15, 0.1, 1.0, false, 1.0, 0.0, 0.0, 0.0, 0.15, BLUE);
    public static final Attack lblow = new Attack("Low Blow", false, 50, UA, 0, 20, 0.3, 1.0, false, 1.0, 0.0, 0.1, 0.0, 0.0, BLUE);
    public static final Attack punch = new Attack("Punch", false, 30, UA, 0, 10, 0.05, 1.0, false, 0.8, 0.0, 0.15, 0.0, 0.0, GREEN);
    public static final Attack punchaoe = new Attack("Punch 'em Both", false, 40, UA, 0, 10, 0.075, 1.0, false, 1.0, 0.0, 0.0, 0.15, 0.0, GREEN, YELLOW);
    public static final Attack ripapart = new Attack("Rip Apart", false, 60, UA, 6, 50, 0.3, 1.0, false, 1.0, 0.075, 0.075, 0.075, 0.075, GREEN, BLUE, YELLOW, RED);
    public static final Attack stealthunder = new Attack("Steal Thunder", false, 40, UA, 0, 0, 0, false, 1.0, 0.0, 0.0, 0.0, 0.1, BLUE, YELLOW);
    public static final Attack takedown = new Attack("Takedown", false, 50, UA, 3, 40, 0.3, 1.0, false, 1.0, 0.0, 0.0, 0.3, 0.0, YELLOW, RED);
    public static final Attack uppercut = new Attack("Uppercut", false, 30, UA, 0, 30, 0.05, 1.0, false, 0.8, 0.0, 0.0, 0.15, 0.0, GREEN, BLUE);

    public static final Attack chop = new Attack("Chop", false, 40, MC, 1, 0.0, 1.0, false, 1.0, 0.0, 0.15, 0.0, 0.0, GREEN);
    public static final Attack cleave = new Attack("Cleave", false, 80, MC, 6, 0.0, 1.5, false, 1.0, 0.25, 0.0, 0.0, 0.0, BLUE, RED);
    public static final Attack fcircle = new Attack("Full Circle", false, 40, MC, 0, 0.0, 1.0, false, 0.9, 0.05, 0.15, 0.0, 0.0, YELLOW, RED);
    public static final Attack sting = new Attack("Sting", false, 50, MC, 2, 0, 1.25, false, 1.0, 0.0, 0.0, 0.1, 0.2, BLUE, GREEN);
    public static final Attack storm = new Attack("Storm of Swords", false, 50, MC, 2, 0, 1.0, false, 1.0, 0.0, 0.0, 0.3, 0.0, BLUE, YELLOW);
    public static final Attack qbarrage = new Attack("Quick Barrage", false, 20, MC, 0, 0, 0.25, false, 1.0, 0.1, 0.0, 0.0, 0.0, RED);
    public static final Attack ravensbite = new Attack("Raven's Bite", false, 40, MC, 4, 0, 1.1, false, 1.0, 0.0, 0.15, 0.15, 0.0, GREEN, YELLOW);
    public static final Attack sideswipe = new Attack("Sideswipe", false, 25, MC, 0, 0, 0.75, false, 1.0, 0.0, 0.0, 0.075, 0.0, YELLOW);

    //Weird custom moves
    public static final Card takeaim = new Card("Take Aim", true, 30);
    public static final Attack opknocks = new Attack("Opportunity Knocks", false, 45, UA, 4, false, 1.0, 0.4, 0.4, 0.4, 0.4) {
        @Override
        public Map<DefenseType, Double> calculateEnemyDefWeights(Maneuver maneuver, double maneuvermeter,
                                                                 int ua, int mc, int cards, Map<DefenseType, Double> enemyDefWeight, double enemyBlockWeight) {
            //Op knocks only applies its weight against the opponent's greatest opening by 40% * Mu
            final double atkweight = getAttackweight(maneuver, maneuvermeter, ua, mc, cards);
            final double blockweight = enemyBlockWeight == 0 ? atkweight : enemyBlockWeight;
            final Map<DefenseType, Double> futureWeights = new HashMap<>();
            //Get highest Def Type
            DefenseType maxopening = RED;
            double max = 0;
            for (final DefenseType def : DefenseType.values()) {
                futureWeights.put(def, enemyDefWeight.get(def));
                if(enemyDefWeight.get(def) > max) {
                    maxopening = def;
                    max = enemyDefWeight.get(def);
                }
            }
            //Apply weight to just that def type
            futureWeights.put(maxopening, enemyDefWeight.get(maxopening) + ((1 - enemyDefWeight.get(maxopening)) * openingweights.get(maxopening)
                    * Math.cbrt(atkweight / blockweight)));
            return futureWeights;
        }
    };
    public static final Restoration dash = new Restoration("Dash", true, 80, 0, 1.0, 1.0, 1.0, 1.0) {
        @Override
        public Map<DefenseType, Double> getFutureWeights(int cards, Map<DefenseType, Double> CurrentWeights) {
            //Dash is special in that it will completely remove your slightest opening
            final Map<DefenseType, Double> NextWeights = new HashMap<>();
            //Get lowest Def Type that isn't 0
            DefenseType opening = RED;
            double min = Double.MAX_VALUE;
            for (final DefenseType def : DefenseType.values()) {
                if(CurrentWeights.get(def) < min && CurrentWeights.get(def) != 0.0d) {
                    opening = def;
                    min = CurrentWeights.get(def);
                }
                NextWeights.put(def, CurrentWeights.get(def));
            }
            //Apply reduction to slightest opening
            NextWeights.put(opening, 0.0d);
            return NextWeights;
        }
    };

    public static final Map<String, Card> lookup = new HashMap<>();

    static {
        lookup.put(bloodlust.name, bloodlust);
        lookup.put(chinup.name, chinup);
        lookup.put(combatmed.name, combatmed);
        lookup.put(dog.name, dog);
        lookup.put(oakstance.name, oakstance);
        lookup.put(parry.name, parry);
        lookup.put(shieldup.name, shieldup);
        lookup.put(toarms.name, toarms);
        lookup.put(arteva.name, arteva);
        lookup.put(fdodge.name, fdodge);
        lookup.put(flex.name, flex);
        lookup.put(jump.name, jump);
        lookup.put(qdodge.name, qdodge);
        lookup.put(regaincomp.name, regaincomp);
        lookup.put(sidestep.name, sidestep);
        lookup.put(watchitsmoves.name, watchitsmoves);
        lookup.put(yieldground.name, yieldground);
        lookup.put(zigzag.name, zigzag);
        lookup.put(chop.name, chop);
        lookup.put(cleave.name, cleave);
        lookup.put(fcircle.name, fcircle);
        lookup.put(jugular.name, jugular);
        lookup.put(hmaker.name, hmaker);
        lookup.put(kick.name, kick);
        lookup.put(kito.name, kito);
        lookup.put(lhook.name, lhook);
        lookup.put(lblow.name, lblow);
        lookup.put(punch.name, punch);
        lookup.put(punchaoe.name, punchaoe);
        lookup.put(qbarrage.name, qbarrage);
        lookup.put(ravensbite.name, ravensbite);
        lookup.put(ripapart.name, ripapart);
        lookup.put(sideswipe.name, sideswipe);
        lookup.put(stealthunder.name, stealthunder);
        lookup.put(sting.name, sting);
        lookup.put(storm.name, storm);
        lookup.put(takedown.name, takedown);
        lookup.put(uppercut.name, uppercut);
    }
}
