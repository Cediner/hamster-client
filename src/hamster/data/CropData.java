package hamster.data;

public class CropData {
    public enum CropType {
	GROUND, TRELLIS
    }

    private CropType type;
    private int min_stage, final_stage;

    public CropType type() { return type; }
    public int min_stage() { return min_stage; }
    public int final_stage() { return final_stage; }
    public boolean multistage() { return min_stage < final_stage; }
}
