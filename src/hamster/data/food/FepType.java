package hamster.data.food;

public enum FepType {
    STR, AGI, INT, CON, PER, CHA, DEX, WIL, PSY,
    STR2, AGI2, INT2, CON2, PER2, CHA2, DEX2, WIL2, PSY2;

    public FepType complement() {
	return switch (this) {
	    case STR  -> STR2;
	    case AGI -> AGI2;
	    case INT -> INT2;
	    case CON -> CON2;
	    case PER -> PER2;
	    case CHA -> CHA2;
	    case DEX -> DEX2;
	    case WIL -> WIL2;
	    case PSY -> PSY2;
	    case STR2 -> STR;
	    case AGI2 -> AGI;
	    case INT2 -> INT;
	    case CON2 -> CON;
	    case PER2 -> PER;
	    case CHA2 -> CHA;
	    case DEX2 -> DEX;
	    case WIL2 -> WIL;
	    case PSY2 -> PSY;
	};
    }
}
