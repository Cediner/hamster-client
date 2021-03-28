import haven.AWidget;
import haven.UI;

/**
 * This effectively removes the  Fadein effect that can be seen during creation room
 */
@SuppressWarnings("unused") // Dynamically created by resource
public class Fadein extends AWidget {
    public Fadein(double paramDouble) { }

    public static Fadein mkwidget(UI paramUI, Object... paramVarArgs) {
	double d = ((Number)paramVarArgs[0]).doubleValue();
	return new Fadein(d);
    }
}
